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
import qualified Data.Text.IO as T

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
  libDir <- createPath [out, "src", "Cauterize", "Generated"]
  cabalDir <- createPath [out]
  clientDir <- createPath [out, "crucible"]

  let libPath = libDir `combine` (hsName ++ ".hs")
  let cabalPath = cabalDir `combine` (specName' ++ ".cabal")
  let clientPath = clientDir `combine` "Main.hs"

  let cabalData = cabalTempl specName' hsName
  let clientData = clientTempl
  let libData = libTempl hsName spec

  writeFile cabalPath cabalData
  writeFile clientPath clientData
  writeFile libPath libData

  where
    specName = Spec.specName spec
    specName' = T.unpack specName
    hsName = nameToHsName specName

nameToHsName :: T.Text -> String
nameToHsName n = T.unpack $ T.concat $ map T.toTitle $ T.split (== '_') n

identToHsName :: CT.Identifier -> String
identToHsName i = nameToHsName (CT.unIdentifier i)

cabalTempl :: String -> String -> String
cabalTempl name libname = unindent [i|
  name:                #{name}
  version:             0.0.0.1
  build-type:          Simple
  cabal-version:       >= 1.10

  executable #{name}_crucible_client
    main-is:             Main.hs
    ghc-options:         -Wall -O2
    hs-source-dirs:      crucible_client
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

clientTempl :: String
clientTempl = unindent [i|
    module Main where

    main :: IO ()
    main = return ()
  |]

-- synonym
-- range
-- array
-- vector
-- enumeration
-- record
-- combination
-- union
libTempl libname spec = unlines parts
  where
    mod = [i|module Cauterize.Generated.#{libname}.Types where\n|]
    types = map libTypeTempl (Spec.specTypes spec)
    parts = mod : types

libTypeTempl :: Spec.Type -> String
libTypeTempl t =  unlines [declinst, transinst, typeinst]
  where
   tn = Spec.typeName t
   tCtor = nameToHsName $ CT.unIdentifier tn
   sz = Spec.typeSize t
   transinst = transcodableTempl tCtor tn (CT.sizeMin sz) (CT.sizeMax sz)
   typeinst = typeTempl tCtor (Spec.typeFingerprint t)
   declinst =
     case Spec.typeDesc t of
       Spec.Synonym { Spec.synonymRef = r } ->
         synonymTempl tCtor r
       Spec.Range { Spec.rangeOffset = ro, Spec.rangeLength = rl, Spec.rangeTag = rt, Spec.rangePrim = rp } ->
         rangeTempl tCtor ro rl rt rp
       Spec.Array { Spec.arrayRef = r, Spec.arrayLength = l } ->
         arrayTempl tCtor r l
       Spec.Vector { Spec.vectorRef = r, Spec.vectorLength = l, Spec.vectorTag = t } ->
         vectorTempl tCtor r l t
       Spec.Enumeration { Spec.enumerationValues = ev, Spec.enumerationTag = et } ->
         enumerationTempl tCtor ev et -- okay, tCtor here is actually a ctor prefix
       Spec.Union { Spec.unionFields = fs, Spec.unionTag = t } ->
         unionTempl tCtor t fs
       Spec.Record { Spec.recordFields = rf } ->
         recordTempl tCtor rf
  {-
    S.Combination { S.combinationFields = fs, S.combinationTag = fr } ->
      combinationEncoderBody n fs fr
  -}

transcodableTempl ctor n szMin szMax = unindent [i|
  instance CautTranscodable #{ctor} where
    cautName = const "#{CT.unIdentifier n}"
    cautSize = const (#{szMin},#{szMax})|]

typeTempl ctor hash = unindent [i|
  instance CautType #{ctor} where
    cautHash = const #{hashToStr hash}|]
  where
    hashToStr h = show (H.hashToBytes h)

synonymTempl :: String -> CT.Identifier -> String
synonymTempl tCtor r = unindent [i|
  data #{tCtor} = #{tCtor} #{rCtor} deriving (Show, Eq, Ord)
  instance CautSynonym #{tCtor} where
  instance Serializable CautResult #{tCtor} where
    serialize t@(#{tCtor} a) = genSynonymSerialize a t
    deserialize = genSynonymDeserialize (undefined :: #{tCtor}) #{tCtor}|]
  where
    rCtor = identToHsName r

rangeTempl :: String -> CT.Offset -> CT.Length -> CT.Tag -> CT.Prim -> String
rangeTempl tCtor ro rl rt rp = unindent [i|
  data #{tCtor} = #{tCtor} #{rpCtor} deriving (Show, Eq, Ord)
  instance CautRange #{tCtor} where
    rangePrim = const #{rpCtor}
    rangeTag = const #{rtWidth}
    rangeOffset = const #{ro}
    rangeLength = const #{rl}
  instance Serializable CautResult #{tCtor} where
    serialize t@(#{tCtor} a) = genRangeSerialize a t
    deserialize = genRangeDeserialize (undefined :: #{tCtor}) #{tCtor}|]
  where
    rpCtor = primToGhc7Prim rp
    rtWidth = tagToTagWidth rt

arrayTempl tCtor r l = unindent [i|
  data #{tCtor} = #{tCtor} (Vector #{rCtor}) deriving (Show, Eq, Ord)
  instance CautArray #{tCtor} where
    arrayLength = const #{l}
  instance Serializable CautResult #{tCtor} where
    serialize t@(#{tCtor} a) = genArraySerialize a t
    deserialize = genArrayDeserialize (undefined :: #{tCtor}) #{tCtor}|]
  where
    rCtor = identToHsName r

vectorTempl :: String -> CT.Identifier -> CT.Length -> CT.Tag -> String
vectorTempl tCtor r l t = unindent [i|
  data #{tCtor} = #{tCtor} (Vector #{rCtor}) deriving (Show, Eq, Ord)
  instance CautVector #{tCtor} where
    vectorMaxLength = const #{l}
    vectorTagWidth = const #{tWidth}
  instance Serializable CautResult #{tCtor} where
    serialize t@(#{tCtor} a) = genVectorSerialize a t
    deserialize = genVectorDeserialize (undefined :: #{tCtor}) #{tCtor}|]
  where
    rCtor = identToHsName r
    tWidth = tagToTagWidth t

enumerationTempl :: String -> [Spec.EnumVal] -> CT.Tag -> String
enumerationTempl tCtor allevs@(ev:evs) et = intercalate "\n" parts
  where
    tWidth = tagToTagWidth et
    maxIx = fromMaybe (error "INVALID SPEC: enumerations must have at least one value.")
                      (maximumIndex allevs)
    enumValToHsName = identToHsName . Spec.enumValName
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
        enumerationMaxIndex = const #{maxIx}|]
    seriInst = unindent [i|
      instance Serializable CautResult #{tCtor} where
        serialize t@(#{tCtor} a) = genEnumerationSerialize a t
        deserialize = genEnumerationDeserialize (undefined :: #{tCtor}) #{tCtor}|]

recordTempl tCtor (f:fs) = intercalate "\n" parts
  where
    parts =
      [ dataType
      , recInst
      , seriInst
      ]
    dataType =
      let ty = "data " ++ tCtor
          fCtor = fieldToHsType f
          fsCtors = map fieldToHsType fs
          ctors = ("  { " ++ fCtor) : map ("  , " ++ ) fsCtors
          deriv = "  } deriving (Show, Ord, Eq)"
      in intercalate "\n" ((ty:ctors) ++ [deriv])
    recInst = unindent [i|
      instance CautRecord #{tCtor} where|]
    seriInst = unindent [i|
      instance Serializable CautResult #{tCtor} where
        serialize t@(#{tCtor} a) = genRecordSerialize a t
        deserialize = genRecordDeserialize (undefined :: #{tCtor}) #{tCtor}|]

unionTempl tCtor t (f:fs) = intercalate "\n" parts
  where
    parts =
      [ dataType
      , unionInst
      , seriInst
      ]
    dataType =
      let ty = "data " ++ tCtor
          fCtor = fieldToHsType f
          fsCtors = map fieldToHsType fs
          ctors = ("  = " ++ fCtor) : map ("  | " ++) fsCtors
          deriv = "  deriving (Show, Ord, Eq)"
      in intercalate "\n" ((ty:ctors) ++ [deriv])
    unionInst = unindent [i|
      instance CautUnion #{tCtor} where
        unionTagWidth = #{tagToTagWidth t}|]
    seriInst = unindent [i|
      instance Serializable CautResult #{tCtor} where
        serialize t@(#{tCtor} a) = genUnionSerialize a t
        deserialize = genUnionDeserialize (undefined :: #{tCtor}) #{tCtor}|]

fieldToHsType (Spec.DataField n i r) = identToHsName n ++ " " ++ identToHsName r
fieldToHsType (Spec.EmptyField n i) = identToHsName n

maximumIndex [] = Nothing
maximumIndex xs = Just $ foldr (max) 0 (map Spec.enumValIndex xs)

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

tagToTagWidth CT.T1 = 1
tagToTagWidth CT.T2 = 2
tagToTagWidth CT.T4 = 4
tagToTagWidth CT.T8 = 8
