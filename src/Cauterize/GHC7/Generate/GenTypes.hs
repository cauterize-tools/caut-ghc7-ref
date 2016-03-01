{-# LANGUAGE QuasiQuotes #-}
module Cauterize.GHC7.Generate.GenTypes
       ( generateOutput
       ) where

import Cauterize.GHC7.Options
import Cauterize.GHC7.Generate.Utils
import Data.String.Interpolate.IsString
import Data.String.Interpolate.Util
import System.FilePath.Posix
import qualified Cauterize.Specification as Spec
import qualified Cauterize.CommonTypes as CT
import qualified Cauterize.Hash as H
import Data.List (intercalate)
import Data.Maybe (catMaybes, fromMaybe)

generateOutput :: Spec.Specification -> CautGHC7Opts -> IO ()
generateOutput spec opts = do
  genDir <- createPath ([out, "src"] ++ hsName)
  let genPath = genDir `combine` "Types.hs"
  let genData = genTempl hsName spec
  writeFile genPath genData
  where
    hsName = fromMaybe
      ["Cauterize", "Generated", nameToHsName (Spec.specName spec)]
      (modulePathAsList opts)
    out = outputDirectory opts

genTempl :: [String] -> Spec.Specification -> String
genTempl libpath spec = unlines parts
  where
    libmod =
      [ "{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, DeriveDataTypeable #-}"
      , "{- WARNING: This is generated code. DO NOT EDIT. -}"
      , [i|module #{libname}.Types where\n|]
      ]
    imports =
      [ "import Cauterize.GHC7.Support.Prototypes"
      , "import Cauterize.GHC7.Support.Result"
      , "import Data.Data"
      , ""
      ]
    specInfo =
      let sz = Spec.specSize spec
          szMin = CT.sizeMin sz
          szMax = CT.sizeMax sz
          hashToStr h = show (H.hashToBytes h)
      in [ [i|specName :: String|]
         , [i|specName = "#{Spec.specName spec}"|]
         , [i|specSize :: (MinSize, MaxSize)|]
         , [i|specSize = (#{szMin},#{szMax})|]
         , [i|specFingerprint :: Hash|]
         , [i|specFingerprint = #{hashToStr (Spec.specFingerprint spec)}|]
         , ""
         ]
    types = map libTypeTempl (Spec.specTypes spec)
    parts = libmod ++ imports ++ specInfo ++ types
    libname = intercalate "." libpath

libTypeTempl :: Spec.Type -> String
libTypeTempl t =  unlines [declinst, transinst, typeinst]
  where
   tn = Spec.typeName t
   sz = Spec.typeSize t
   tCtor = identToHsName tn
   transinst = transcodableTempl tn (CT.sizeMin sz) (CT.sizeMax sz)
   typeinst = typeTempl tCtor (Spec.typeFingerprint t)
   declinst =
     case Spec.typeDesc t of
       Spec.Synonym { Spec.synonymRef = r } ->
         synonymTempl tn r
       Spec.Range { Spec.rangeOffset = ro, Spec.rangeLength = rl, Spec.rangeTag = rt, Spec.rangePrim = rp } ->
         rangeTempl tn ro rl rt rp
       Spec.Array { Spec.arrayRef = r, Spec.arrayLength = l } ->
         arrayTempl tn r l
       Spec.Vector { Spec.vectorRef = r, Spec.vectorLength = l, Spec.vectorTag = vt } ->
         vectorTempl tn r l vt
       Spec.Enumeration { Spec.enumerationValues = ev, Spec.enumerationTag = et } ->
         enumerationTempl tn ev et
       Spec.Union { Spec.unionFields = fs, Spec.unionTag = ut } ->
         unionTempl tn ut fs
       Spec.Record { Spec.recordFields = rf } ->
         recordTempl tn rf
       Spec.Combination { Spec.combinationFields = fs, Spec.combinationTag = ct } ->
         combinationTempl tn fs ct

transcodableTempl :: CT.Identifier -> Integer -> Integer -> String
transcodableTempl n szMin szMax = unindent [i|
  instance CautTranscodable #{identToHsName n} where
    cautName = const "#{CT.unIdentifier n}"
    cautSize = const (#{szMin},#{szMax})|]

typeTempl :: String -> H.Hash -> String
typeTempl ctor hash = unindent [i|
  instance CautType #{ctor} where
    cautFingerprint = const #{hashToStr hash}|]
  where
    hashToStr h = show (H.hashToBytes h)

synonymTempl :: CT.Identifier -> CT.Identifier -> String
synonymTempl tn r = unindent [i|
  data #{tCtor} = #{tCtor} #{rCtor} deriving (Data, Typeable, Show, Eq, Ord)
  instance CautSynonym #{tCtor} where
  instance Serializable CautResult #{tCtor} where
    serialize t@(#{tCtor} a) = genSynonymSerialize a t
    deserialize = genSynonymDeserialize (undefined :: #{tCtor}) #{tCtor}|]
  where
    tCtor = identToHsName tn
    rCtor = identToHsName r

rangeTempl :: CT.Identifier -> CT.Offset -> CT.Length -> CT.Tag -> CT.Prim -> String
rangeTempl tn ro rl rt rp = unindent [i|
  data #{tCtor} = #{tCtor} #{rpCtor} deriving (Data, Typeable, Show, Eq, Ord)
  instance CautRange #{tCtor} where
    rangeTagWidth = const #{rtWidth}
    rangeOffset = const #{fmtNegative ro}
    rangeLength = const #{rl}
  instance Serializable CautResult #{tCtor} where
    serialize t@(#{tCtor} a) = genRangeSerialize a t
    deserialize = genRangeDeserialize (undefined :: #{tCtor}) #{tCtor}|]
  where
    tCtor = identToHsName tn
    rpCtor = (identToHsName . CT.primToText) rp
    rtWidth = tagToTagWidth rt
    fmtNegative n | n < 0 = "(" ++ show n ++ ")"
                  | otherwise = show n

arrayTempl :: CT.Identifier -> CT.Identifier -> CT.Length -> String
arrayTempl tn r l = unindent [i|
  data #{tCtor} = #{tCtor} (Vector #{rCtor}) deriving (Data, Typeable, Show, Eq, Ord)
  instance CautArray #{tCtor} where
    arrayLength = const #{l}
  instance Serializable CautResult #{tCtor} where
    serialize t@(#{tCtor} a) = genArraySerialize a t
    deserialize = genArrayDeserialize (undefined :: #{tCtor}) #{tCtor}|]
  where
    tCtor = identToHsName tn
    rCtor = identToHsName r

vectorTempl :: CT.Identifier -> CT.Identifier -> CT.Length -> CT.Tag -> String
vectorTempl tn r l t = unindent [i|
  data #{tCtor} = #{tCtor} (Vector #{rCtor}) deriving (Data, Typeable, Show, Eq, Ord)
  instance CautVector #{tCtor} where
    vectorMaxLength = const #{l}
    vectorTagWidth = const #{tWidth}
  instance Serializable CautResult #{tCtor} where
    serialize t@(#{tCtor} a) = genVectorSerialize a t
    deserialize = genVectorDeserialize (undefined :: #{tCtor}) #{tCtor}|]
  where
    tCtor = identToHsName tn
    rCtor = identToHsName r
    tWidth = tagToTagWidth t

enumerationTempl :: CT.Identifier -> [Spec.EnumVal] -> CT.Tag -> String
enumerationTempl _ [] _ = error "INVALID SPEC: enumerations must have at least one value"
enumerationTempl tn allevs@(ev:evs) et = intercalate "\n" parts
  where
    tCtor = identToHsName tn
    tWidth = tagToTagWidth et
    enumValToHsName = (tCtor ++) . identToHsName . Spec.enumValName

    maximumIndex = foldr (max) 0 (map Spec.enumValIndex allevs)

    parts =
      [ dataType
      , enumInst
      , seriInst
      ]
    dataType =
      let ty = "data " ++ tCtor
          evCtor = enumValToHsName ev
          evsCtors = map enumValToHsName evs
          ctors = ("  = " ++ evCtor) : map ("  | " ++) evsCtors
          deriv = "  deriving (Data, Typeable, Show, Ord, Eq, Enum)"
      in intercalate "\n" ((ty:ctors) ++ [deriv])
    enumInst = unindent [i|
      instance CautEnumeration #{tCtor} where
        enumerationTagWidth = const #{tWidth}
        enumerationMaxVal = const #{maximumIndex}|]
    seriInst = unindent [i|
      instance Serializable CautResult #{tCtor} where
        serialize t = genEnumerationSerialize t
        deserialize = genEnumerationDeserialize (undefined :: #{tCtor})|]

recordTempl :: CT.Identifier -> [Spec.Field] -> String
recordTempl tn fs = intercalate "\n" parts
  where
    tCtor = identToHsName tn
    tVar = identToHsVar tn
    fctor:fctors = case catMaybes $ map recordFieldToHsType fs of
                     [] -> error "SPECIFICATION ERROR: must have at least one DataField in a record."
                     x -> x
    parts =
      [ dataType
      , recInst
      , seriInst
      ]
    dataType =
      let ty = "data " ++ tCtor ++ " = " ++ tCtor
          ctors = ("  { " ++ fctor) : map ("  , " ++ ) fctors
          deriv = "  } deriving (Data, Typeable, Show, Ord, Eq)"
      in intercalate "\n" ((ty:ctors) ++ [deriv])
    recInst = unindent [i|
      instance CautRecord #{tCtor} where|]
    seriInst = intercalate "\n" $
      [ [i|instance Serializable CautResult #{tCtor} where|]
      , [i|  serialize r = withTrace (TRecord $ cautName r) $ do|]
      ] ++ (map genFldSer fs) ++
      [ [i|  deserialize = withTrace (TRecord $ cautName (undefined :: #{tCtor})) $|]
      , [i|    return #{tCtor}|]
      ] ++ (map genFldDeser fs)

    genFldDeser :: Spec.Field -> String
    genFldDeser f =
      let n = Spec.fieldName f
      in [i|     `ap` genFieldDeserialize (TRecordField "#{unpackIdent n}")|]

    genFldSer :: Spec.Field -> String
    genFldSer f =
      let n = Spec.fieldName f
      in [i|    genFieldSerialize (TRecordField "#{unpackIdent n}") (#{tVar}#{identToHsName n} r)|]

    recordFieldToHsType (Spec.DataField n _ r) = Just (tVar ++ identToHsName n ++ " :: " ++ identToHsName r)
    recordFieldToHsType (Spec.EmptyField _ _) = Nothing -- nothing is generated for empty record fields

unionTempl :: CT.Identifier -> CT.Tag -> [Spec.Field] -> String
unionTempl _ _ [] = error "SPECIFICATION ERROR: unions must have at least one field"
unionTempl tn t allfs@(f:fs) = intercalate "\n" parts
  where
    tCtor = identToHsName tn
    parts =
      [ dataType
      , unionInst
      , seriInst
      ]
    dataType =
      let ty = "data " ++ tCtor
          fCtor = unionFieldToHsType f
          fsCtors = map unionFieldToHsType fs
          ctors = ("  = " ++ fCtor) : map ("  | " ++) fsCtors
          deriv = "  deriving (Data, Typeable, Show, Ord, Eq)"
      in intercalate "\n" ((ty:ctors) ++ [deriv])
    unionInst = unindent [i|
      instance CautUnion #{tCtor} where
        unionTagWidth = const #{tagToTagWidth t}|]
    seriInst =
      let inst =
            [ [i|instance Serializable CautResult #{tCtor} where|]
            , [i|  serialize r = withTrace (TUnion $ cautName r) $|]
            , [i|                  case r of|]
            ] ++ sers ++
            [ [i|  deserialize =|]
            , [i|    let u = undefined :: #{tCtor}|]
            , [i|    in withTrace (TUnion $ cautName u) $ do|]
            , [i|         tag <- decodeUnionTag u|]
            , [i|         case tag of|]
            ] ++ dsers

          sers = map serBranch allfs
          serBranch (Spec.DataField n ix _)    = [i|                    #{tCtor}#{identToHsName n} v -> genUnionFieldSerialize r #{ix} "#{unpackIdent n}" v|]
          serBranch (Spec.EmptyField n ix)     = [i|                    #{tCtor}#{identToHsName n} -> genUnionFieldSerializeEmpty r #{ix}|]

          dsers = map deserBranch allfs ++        ["           v -> failUnionTag v"]
          deserBranch (Spec.DataField n ix _)  = [i|           #{ix} -> genUnionFieldDeserialize "#{unpackIdent n}" #{tCtor}#{identToHsName n}|]
          deserBranch (Spec.EmptyField n ix)   = [i|           #{ix} -> return #{tCtor}#{identToHsName n}|]
      in intercalate "\n" inst

    unionFieldToHsType (Spec.DataField n _ r) = tCtor ++ identToHsName n ++ " " ++ identToHsName r
    unionFieldToHsType (Spec.EmptyField n _) = tCtor ++ identToHsName n

combinationTempl :: CT.Identifier -> [Spec.Field] -> CT.Tag -> String
combinationTempl _ [] _ = error "SPECIFICATION ERROR: combinations must have at least one field"
combinationTempl tn allfs@(f:fs) t = intercalate "\n" parts
  where
    tCtor = identToHsName tn
    tVar = identToHsVar tn

    maximumIndex = foldr (max) 0 (map Spec.fieldIndex allfs)

    parts =
      [ dataType
      , combInst
      , seriInst
      ]
    dataType =
      let ty = "data " ++ tCtor ++ " = " ++ tCtor
          fCtor = combFieldToHsType f
          fsCtors = map combFieldToHsType fs
          ctors = ("  { " ++ fCtor) : map ("  , " ++ ) fsCtors
          deriv = "  } deriving (Data, Typeable, Show, Ord, Eq)"
      in intercalate "\n" ((ty:ctors) ++ [deriv])
    combInst = unindent [i|
      instance CautCombination #{tCtor} where
        combinationTagWidth = const #{tagToTagWidth t}
        combinationMaxIndex = const #{maximumIndex}|]

    seriInst =
      let ef:efs = map genPresent allfs
          dfs = map genDeser allfs
          inst =
            [ [i|instance Serializable CautResult #{tCtor} where|]
            , [i|  serialize r = withTrace (TCombination $ cautName r) $ do|]
            , [i|    encodeCombTag r [ #{ef}|]
            ] ++ serLines ++
            [ [i|  deserialize =|]
            , [i|    let u = undefined :: #{tCtor}|]
            , [i|    in withTrace (TCombination $ cautName u) $ do|]
            , [i|        flags <- decodeCombTag u|]
            , [i|        return #{tCtor}|]
            ] ++ dserLines

          serLines = let indent = "                    "
                     in map (\x -> indent ++ ", " ++ x) efs ++ [indent ++ "]"]
          dserLines = let indent = "          "
                      in map (\x -> indent ++ "`ap` " ++ x) dfs

          genPresent fld = let n' = identToHsName (Spec.fieldName fld)
                           in [i|fieldPresent $ #{tVar}#{n'} r|]
          genDeser (Spec.DataField n ix _)  = [i|genCombFieldDeserialize "#{unpackIdent n}" (flags `isFlagSet` #{ix})|]
          genDeser (Spec.EmptyField _ ix)   = [i|return (if flags `isFlagSet` #{ix} then Just () else Nothing)|]

      in intercalate "\n" inst


    combFieldToHsType (Spec.DataField n _ r) = tVar ++ identToHsName n ++ " :: Maybe " ++ identToHsName r
    combFieldToHsType (Spec.EmptyField n _) = tVar ++ identToHsName n ++ " :: Maybe ()"

tagToTagWidth :: CT.Tag -> Integer
tagToTagWidth CT.T1 = 1
tagToTagWidth CT.T2 = 2
tagToTagWidth CT.T4 = 4
tagToTagWidth CT.T8 = 8
