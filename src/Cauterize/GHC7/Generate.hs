{-# LANGUAGE QuasiQuotes #-}
module Cauterize.GHC7.Generate
       ( caut2hs
       ) where

import Cauterize.GHC7.Options
import Control.Monad
import Data.String.Interpolate.IsString
import Data.String.Interpolate.Util
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
  {-
    S.Range { S.rangeOffset = o, S.rangeLength = l, S.rangeTag = rt, S.rangePrim = rp } ->
      rangeEncoderBody o l rt rp
    S.Array { S.arrayRef = r } ->
      arrayEncoderBody (ident2str r)
    S.Vector { S.vectorRef = r, S.vectorTag = lr } ->
      vectorEncoderBody n (ident2str r) lr
    S.Enumeration { S.enumerationValues = vs, S.enumerationTag = et } ->
      enumerationEncoderBody n vs et
    S.Record { S.recordFields = fs } ->
      recordEncoderBody fs
    S.Combination { S.combinationFields = fs, S.combinationTag = fr } ->
      combinationEncoderBody n fs fr
    S.Union { S.unionFields = fs, S.unionTag = tr } ->
      unionEncoderBody n fs tr
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

rangeTempl tCtor ro rl rt rp = unindent [i|
  data #{tCtor} = #{tCtor} #{rpCtor} deriving (Show, Eq, Ord)
  instance CautRange #{tCtor} where
    rangePrim = const #{rpCtor}
    rangeTag = const #{rtCtor}
    rangeOffset = const #{ro}
    rangeLength = const #{rl}
  instance Serializable CautResult #{tCtor} where
    serialize t@(#{tCtor} a) = genRangeSerialize a t
    deserialize = genRangeDeserialize (undefined :: #{tCtor}) #{tCtor}|]
  where
    rpCtor = primToGhc7Prim rp
    rtCtor = tagToGhc7Prim rt


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

tagToGhc7Prim CT.T1 = "GHC7Tag1"
tagToGhc7Prim CT.T2 = "GHC7Tag2"
tagToGhc7Prim CT.T4 = "GHC7Tag4"
tagToGhc7Prim CT.T8 = "GHC7Tag8"
