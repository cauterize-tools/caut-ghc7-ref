{-# LANGUAGE QuasiQuotes #-}
module Cauterize.GHC7.Generate
       ( caut2hs
       ) where

import Cauterize.GHC7.Options
import Control.Monad
import Data.String.Interpolate.IsString
import Data.String.Interpolate.Util
import Data.Maybe
import Data.List (intercalate)
import System.Directory
import System.FilePath.Posix
import qualified Cauterize.Hash as H
import qualified Cauterize.Specification as Spec
import qualified Cauterize.CommonTypes as CT
import qualified Data.Text as T

caut2hs :: CautGHC7Opts -> IO ()
caut2hs (CautGHC7Opts { specFile = specPath, outputDirectory = outPath }) = createGuard outPath $ do
  spec <- Spec.parseSpecificationFromFile specPath

  case spec of
    Left es -> error $ show es
    Right s' -> generateOutput s' outPath

createGuard :: FilePath -> IO () -> IO ()
createGuard out go = createDirIfNotExist out >> go

createPath :: [FilePath] -> IO FilePath
createPath = go ""
  where
    go p [] = return p
    go root (p:ps) = do
      let combined = root `combine` p
      createDirIfNotExist combined
      go combined ps

createDirIfNotExist :: FilePath -> IO ()
createDirIfNotExist out = do
  fe <- doesFileExist out
  de <- doesDirectoryExist out

  if fe
    then error $ "Error: " ++ out ++ " is a file."
    else unless de $ createDirectory out

generateOutput :: Spec.Specification -> FilePath -> IO ()
generateOutput spec out = do
  libDir <- createPath [out, "src", "Cauterize", "Generated", hsName]
  cabalDir <- createPath [out]
  stackDir <- createPath [out]
  clientDir <- createPath [out, "crucible"]

  let libPath = libDir `combine` "Types.hs"
  let cabalPath = cabalDir `combine` (specName' ++ ".cabal")
  let stackPath = stackDir `combine` "stack.yaml"
  let clientPath = clientDir `combine` "Main.hs"

  let cabalData = cabalTempl specName' hsName
  let stackData = stackYamlTempl
  let clientData = clientTempl
  let libData = libTempl hsName spec

  writeFile cabalPath cabalData
  writeFile stackPath stackData
  writeFile clientPath clientData
  writeFile libPath libData

  where
    specName = Spec.specName spec
    specName' = T.unpack specName
    hsName = nameToHsName specName

nameToHsName :: T.Text -> String
nameToHsName n = T.unpack $ T.concat $ map T.toTitle $ T.split (== '_') n

nameToHsVar :: T.Text -> String
nameToHsVar n =
  let f:rs = T.split (== '_') n
  in T.unpack $ T.concat $ f:(map T.toTitle rs)

identToHsName :: CT.Identifier -> String
identToHsName ident = fromMaybe (nameToHsName (CT.unIdentifier ident))
                                (identToPrimName ident)

unpackIdent :: CT.Identifier -> String
unpackIdent ident = T.unpack (CT.unIdentifier ident)

identToPrimName :: CT.Identifier -> Maybe String
identToPrimName n =
  let n' = T.unpack $ CT.unIdentifier n
  in case n' of
       "u8"   -> Just "Word8"
       "u16"  -> Just "Word16"
       "u32"  -> Just "Word32"
       "u64"  -> Just "Word64"
       "s8"   -> Just "Int8"
       "s16"  -> Just "Int16"
       "s32"  -> Just "Int32"
       "s64"  -> Just "Int64"
       "f32"  -> Just "Float"
       "f64"  -> Just "Double"
       "bool" -> Just "Bool"
       _      -> Nothing

identToHsVar :: CT.Identifier -> String
identToHsVar ident = nameToHsVar (CT.unIdentifier ident)

cabalTempl :: String -> String -> String
cabalTempl name libname = unindent [i|
  name:                #{name}
  version:             0.0.0.1
  build-type:          Simple
  cabal-version:       >= 1.10

  executable #{name}_crucible_client
    main-is:             Main.hs
    ghc-options:         -Wall -O2
    hs-source-dirs:      crucible
    default-language:    Haskell2010
    build-depends:       base < 5,
                         #{name}

  library
    hs-source-dirs:      src
    default-language:    Haskell2010
    exposed-modules:     Cauterize.Generated.#{libname}.Types
    build-depends:       base < 5,
                         caut-ghc7-ref,
                         cereal,
                         cereal-plus,
                         text

  |]

stackYamlTempl :: String
stackYamlTempl = unindent [i|
  flags: {}
  packages:
  - '.'
  - location:
      git: https://github.com/cauterize-tools/cauterize.git
      commit: 18d603078d327bc08e790d7c4ae3e721760058b5
  - location:
      git: https://github.com/aisamanra/s-cargot.git
      commit: 1628a7c2913fc5e72ab6ea9a813762bf86d47d49
  - location:
      git: https://github.com/cauterize-tools/crucible.git
      commit: 81165b659b2ee7631f77c22ab64c97a541c874af
  - location: ../
  extra-deps:
    - hastache-0.6.0
    - cereal-plus-0.4.0
  resolver: lts-2.21|]

clientTempl :: String
clientTempl = unindent [i|
    module Main where

    main :: IO ()
    main = return ()
  |]

libTempl :: String -> Spec.Specification -> String
libTempl libname spec = unlines parts
  where
    libmod = [i|module Cauterize.Generated.#{libname}.Types where\n|]
    imports =
      [ "import Cauterize.GHC7.Support.Cauterize"
      , "import Cauterize.GHC7.Support.Prototypes"
      , "import Cauterize.GHC7.Support.Result"
      ]
    types = map libTypeTempl (Spec.specTypes spec)
    parts = libmod : (imports ++ types)

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
    cautHash = const #{hashToStr hash}|]
  where
    hashToStr h = show (H.hashToBytes h)

synonymTempl :: CT.Identifier -> CT.Identifier -> String
synonymTempl tn r = unindent [i|
  data #{tCtor} = #{tCtor} #{rCtor} deriving (Show, Eq, Ord)
  instance CautSynonym #{tCtor} where
  instance Serializable CautResult #{tCtor} where
    serialize t@(#{tCtor} a) = genSynonymSerialize a t
    deserialize = genSynonymDeserialize (undefined :: #{tCtor}) #{tCtor}|]
  where
    tCtor = identToHsName tn
    rCtor = identToHsName r

rangeTempl :: CT.Identifier -> CT.Offset -> CT.Length -> CT.Tag -> CT.Prim -> String
rangeTempl tn ro rl rt rp = unindent [i|
  data #{tCtor} = #{tCtor} #{rpCtor} deriving (Show, Eq, Ord)
  instance CautRange #{tCtor} where
    rangePrim = const #{rpMetaCtor}
    rangeTagWidth = const #{rtWidth}
    rangeOffset = const #{ro}
    rangeLength = const #{rl}
  instance Serializable CautResult #{tCtor} where
    serialize t@(#{tCtor} a) = genRangeSerialize a t
    deserialize = genRangeDeserialize (undefined :: #{tCtor}) #{tCtor}|]
  where
    tCtor = identToHsName tn
    rpCtor = (identToHsName . CT.primToText) rp
    rpMetaCtor = primToGhc7Prim rp
    rtWidth = tagToTagWidth rt

arrayTempl :: CT.Identifier -> CT.Identifier -> CT.Length -> String
arrayTempl tn r l = unindent [i|
  data #{tCtor} = #{tCtor} (Vector #{rCtor}) deriving (Show, Eq, Ord)
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
  data #{tCtor} = #{tCtor} (Vector #{rCtor}) deriving (Show, Eq, Ord)
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
          deriv = "  deriving (Show, Ord, Eq)"
      in intercalate "\n" ((ty:ctors) ++ [deriv])
    enumInst = unindent [i|
      instance CautEnumeration #{tCtor} where
        enumerationTagWidth = const #{tWidth}
        enumerationMaxIndex = const #{maximumIndex}|]
    seriInst = unindent [i|
      instance Serializable CautResult #{tCtor} where
        serialize t@(#{tCtor} a) = genEnumerationSerialize a t
        deserialize = genEnumerationDeserialize (undefined :: #{tCtor}) #{tCtor}|]

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
          deriv = "  } deriving (Show, Ord, Eq)"
      in intercalate "\n" ((ty:ctors) ++ [deriv])
    recInst = unindent [i|
      instance CautRecord #{tCtor} where|]
    seriInst = unindent [i|
      instance Serializable CautResult #{tCtor} where
        serialize t@(#{tCtor} a) = genRecordSerialize a t
        deserialize = genRecordDeserialize (undefined :: #{tCtor}) #{tCtor}|]
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
          deriv = "  deriving (Show, Ord, Eq)"
      in intercalate "\n" ((ty:ctors) ++ [deriv])
    unionInst = unindent [i|
      instance CautUnion #{tCtor} where
        unionTagWidth = #{tagToTagWidth t}|]
    seriInst =
      let inst = [i|instance Serializable CautResult #{tCtor} where|]

          sdef0  = [i|  serialize r = withTrace (TUnion $ cautName r) $|]
          sdef1  = [i|    case r of|]
          sers = map serBranch allfs
          serBranch (Spec.DataField n ix _) = [i|      #{tCtor}#{identToHsName n} v -> genUnionFieldSerialize r #{ix} "#{unpackIdent n}" v|]
          serBranch (Spec.EmptyField n ix)  = [i|      #{tCtor}#{identToHsName n} v -> genUnionFieldSerializeEmpty r #{ix}|]

          ddef = unindent [i|
            deserialize =
              let u = undefined :: #{tCtor}
              in withTrace (TUnion $ cautName u) $ do
                   tag <- decodeUnionTag u
                   case tag of|]
          dsers = map deserBranch allfs ++ ["         v -> failUnionTag v"]
          deserBranch (Spec.DataField n ix _)  = [i|         #{ix} -> genUnionFieldDeserialize "#{unpackIdent n}" #{tCtor}#{identToHsName n}|]
          deserBranch (Spec.EmptyField n ix)   = [i|         #{ix} -> return #{tCtor}#{identToHsName n}|]
      in intercalate "\n" ((inst:sdef0:sdef1:sers) ++ (ddef:dsers))

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
          deriv = "  } deriving (Show, Ord, Eq)"
      in intercalate "\n" ((ty:ctors) ++ [deriv])
    combInst = unindent [i|
      instance CautCombination #{tCtor} where
        combinationTagWidth = #{tagToTagWidth t}
        combinationMaxIndex = #{maximumIndex}|]
    seriInst = unindent [i|
      instance Serializable CautResult #{tCtor} where
        serialize t@(#{tCtor} a) = genCombinationSerialize a t
        deserialize = genCombinationDeserialize (undefined :: #{tCtor}) #{tCtor}|]
    combFieldToHsType (Spec.DataField n _ r) = tVar ++ identToHsName n ++ " :: Maybe " ++ identToHsName r
    combFieldToHsType (Spec.EmptyField n _) = tVar ++ identToHsName n ++ " :: Maybe ()"

primToGhc7Prim :: CT.Prim -> String
primToGhc7Prim CT.PU8   = "GHC7Word8"
primToGhc7Prim CT.PU16  = "GHC7Word16"
primToGhc7Prim CT.PU32  = "GHC7Word32"
primToGhc7Prim CT.PU64  = "GHC7Word64"
primToGhc7Prim CT.PS8   = "GHC7Int8"
primToGhc7Prim CT.PS16  = "GHC7Int16"
primToGhc7Prim CT.PS32  = "GHC7Int32"
primToGhc7Prim CT.PS64  = "GHC7Int64"
primToGhc7Prim CT.PF32  = "GHC7Float"
primToGhc7Prim CT.PF64  = "GHC7Double"
primToGhc7Prim CT.PBool = "GHC7Bool"

tagToTagWidth :: CT.Tag -> Integer
tagToTagWidth CT.T1 = 1
tagToTagWidth CT.T2 = 2
tagToTagWidth CT.T4 = 4
tagToTagWidth CT.T8 = 8
