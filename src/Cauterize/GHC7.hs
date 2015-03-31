{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Cauterize.GHC7
  ( caut2hs
  ) where

import Cauterize.GHC7.Options

import Control.Monad
import Data.Data
import Data.Word
import System.Directory
import System.FilePath.Posix
import Text.Hastache
import Text.Hastache.Context
import qualified Cauterize.Common.Types as C
import qualified Cauterize.FormHash as H
import qualified Cauterize.Meta as Meta
import qualified Cauterize.Specification as Spec
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import Paths_caut_ghc7_ref

caut2hs :: CautGHC7Opts -> IO ()
caut2hs (CautGHC7Opts { specFile = specPath, metaFile = metaPath, outputDirectory = outPath }) = createGuard outPath $ do
  spec <- Spec.parseFile specPath
  meta <- Meta.parseFile metaPath

  case spec of
    Left es -> error $ show es
    Right s' ->
      case meta of
        Left em -> error $ show em
        Right m' -> generateOutput s' m' outPath

generateOutput :: Spec.Spec -> Meta.Meta -> FilePath -> IO ()
generateOutput spec meta out = do
  createPath [out, "src", "Cauterize", "Generated"]
  renderFiles
  where
    specName = Spec.specName spec
    hsName = nameToHsName specName

    renderFiles = do
      cf_tmpl <- getDataFileName "cabal_file.cabal.tmpl"
      mod_tmpl <- getDataFileName "Module.hs"

      let cabalPath = T.unpack specName ++ ".cabal"
      let modPath = foldl combine "" ["src", "Cauterize", "Generated", T.unpack hsName ++ ".hs"]

      renderTo spec meta cf_tmpl (out `combine` cabalPath)
      renderTo spec meta mod_tmpl (out `combine` modPath)

nameToHsName :: T.Text -> T.Text
nameToHsName n = T.concat $ map T.toTitle $ T.split (== '_') n

data HsCtx = HsCtx
  { hscLibName :: T.Text
  , hscMeta :: HsMetaCtx
  , hscTypes :: [HsTypeCtx]
  } deriving (Show, Eq, Data, Typeable)

data HsMetaCtx = HsMetaCtx
  { hsmLengthWidth :: Integer
  , hsmTypeWidth :: Integer
  } deriving (Show, Eq, Data, Typeable)

data HsTypeInfo = HsTypeInfo
  { hstPrototype :: T.Text
  , hstConstructor :: T.Text
  , hstName :: T.Text
  , hstHash :: [Word8]
  , hstSize :: HsTSizeCtx
  } deriving (Show, Eq, Data, Typeable)

data HsBuiltIn
  = HsU8 | HsU16 | HsU32 | HsU64
  | HsS8 | HsS16 | HsS32 | HsS64
  | HsF32 | HsF64
  | HsCu8 | HsCu16 | HsCu32
  | HsBool
  deriving (Show, Eq, Data, Typeable)

data HsTFieldInfo = HsTDataField
                    { hstdfName :: T.Text
                    , hstdfIndex :: Integer
                    , hstdfRefCtor :: T.Text }
                  | HsTEmptyField
                    { hstefName :: T.Text
                    , hstefIndex :: Integer }
  deriving (Show, Eq, Data, Typeable)

data HsTypeCtx
  = HsTBuiltIn { hstDetail :: HsTypeInfo, hstBIInstance :: HsBuiltIn }
  | HsTSynonym { hstDetail :: HsTypeInfo, hstSynnedCtor :: T.Text }
  | HsTArray { hstDetail :: HsTypeInfo, hstArrayRefCtor :: T.Text, hstArrayLen :: Integer }
  | HsTVector { hstDetail :: HsTypeInfo, hstVectorRefCtor :: T.Text, hstVectorMaxLen :: Integer, hstVectorLenWidth :: Integer  }
  | HsTRecord { hstDetail :: HsTypeInfo, hstRecordFields :: [HsTFieldInfo] }
  | HsTCombination { hstDetail :: HsTypeInfo, hstCombinationFields :: [HsTFieldInfo], hstCombinationFlagsWidth :: Integer }
  | HsTUnion { hstDetail :: HsTypeInfo, hstUnionFields :: [HsTFieldInfo], hstUnionTagWidth :: Integer }
  deriving (Show, Eq, Data, Typeable)

data HsTSizeCtx = HsTSizeCtx { hstMinSize :: Integer, hstMaxSize :: Integer }
  deriving (Show, Eq, Data, Typeable)


renderTo :: Spec.Spec -> Meta.Meta -> FilePath -> FilePath -> IO ()
renderTo spec meta templatePath destPath = do
  template <- T.readFile templatePath
  cfg <- mkCfg
  rendered <- hastacheStr cfg (encodeStr $ T.unpack template)
                              (mkGenericContext $ mkHsCtx spec meta)
  T.writeFile destPath rendered
  where
    mkCfg = do
      tpath <- getDataFileName "templates/sub"
      return $ defaultConfig { muEscapeFunc = id
                             , muTemplateFileDir = Just tpath } :: IO (MuConfig IO)

mkHsCtx :: Spec.Spec -> Meta.Meta -> HsCtx
mkHsCtx spec meta = HsCtx
  { hscLibName = nameToHsName $ Spec.specName spec
  , hscMeta =
      HsMetaCtx
        { hsmLengthWidth = Meta.metaDataLength meta
        , hsmTypeWidth = Meta.metaTypeLength meta
        }
  , hscTypes = map mkHsType (Spec.specTypes spec)
  }

mkHsType :: Spec.SpType -> HsTypeCtx
mkHsType t =
  case t of
    Spec.BuiltIn { Spec.unBuiltIn = (C.TBuiltIn b) }
      -> HsTBuiltIn { hstDetail = mkTypeInfo "builtin"
                    , hstBIInstance = biConv b }
    Spec.Synonym { Spec.unSynonym = (C.TSynonym { C.synonymRepr = r } ) }
      -> HsTSynonym { hstDetail = mkTypeInfo "synonym"
                    , hstSynnedCtor = nameToHsName (T.pack . show $ r)}
    Spec.Array { Spec.unArray = (C.TArray { C.arrayRef = ar, C.arrayLen = al }) }
      -> HsTArray { hstDetail = mkTypeInfo "array"
                  , hstArrayRefCtor = nameToHsName ar
                  , hstArrayLen = al }
    Spec.Vector { Spec.unVector = (C.TVector { C.vectorRef = vr, C.vectorMaxLen = vml })
                , Spec.lenRepr = (Spec.LengthRepr { Spec.unLengthRepr = lr } ) }
      -> HsTVector { hstDetail = mkTypeInfo "vector"
                   , hstVectorRefCtor = nameToHsName vr
                   , hstVectorMaxLen = vml
                   , hstVectorLenWidth = C.builtInSize lr }
    Spec.Record { Spec.unRecord = (C.TRecord { C.recordFields = C.Fields rfs }) }
      -> HsTRecord { hstDetail = mkTypeInfo "record"
                   , hstRecordFields = map mkFieldInfo rfs }
    Spec.Combination { Spec.unCombination = (C.TCombination { C.combinationFields = C.Fields cfs })
                     , Spec.flagsRepr = (Spec.FlagsRepr fr) }
      -> HsTCombination { hstDetail = mkTypeInfo "combination"
                        , hstCombinationFields = map mkFieldInfo cfs
                        , hstCombinationFlagsWidth = C.builtInSize fr }
    Spec.Union { Spec.unUnion = (C.TUnion { C.unionFields = C.Fields ufs })
               , Spec.tagRepr = (Spec.TagRepr tr) }
      -> HsTUnion { hstDetail = mkTypeInfo "union"
                  , hstUnionFields = map mkFieldInfo ufs
                  , hstUnionTagWidth = C.builtInSize tr }
  where
    mkTypeInfo p =
      HsTypeInfo
        { hstName = Spec.typeName t
        , hstHash = H.hashToBytes . Spec.spHash $ t
        , hstSize = HsTSizeCtx { hstMinSize = Spec.minSize t, hstMaxSize = Spec.maxSize t }
        , hstPrototype = p
        , hstConstructor = nameToHsName $ Spec.typeName t
        }

    mkFieldInfo C.Field { C.fName = n, C.fRef = r, C.fIndex = i } =
      HsTDataField { hstdfName = n, hstdfRefCtor = nameToHsName r, hstdfIndex = i }
    mkFieldInfo C.EmptyField { C.fName = n, C.fIndex = i } =
      HsTEmptyField { hstefName = n, hstefIndex = i }

    biConv C.BIu8  = HsU8
    biConv C.BIu16 = HsU16
    biConv C.BIu32 = HsU32
    biConv C.BIu64 = HsU64
    biConv C.BIs8 = HsS8
    biConv C.BIs16 = HsS16
    biConv C.BIs32 = HsS32
    biConv C.BIs64 = HsS64
    biConv C.BIf32 = HsF32
    biConv C.BIf64 = HsF64
    biConv C.BIbool = HsBool

createGuard :: FilePath -> IO () -> IO ()
createGuard out go = createDirIfNotExist out >> go

createPath :: [FilePath] -> IO ()
createPath = go ""
  where
    go _ [] = return ()
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
