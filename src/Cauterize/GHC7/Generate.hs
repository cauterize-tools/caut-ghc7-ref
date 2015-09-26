{-# LANGUAGE QuasiQuotes #-}
module Cauterize.GHC7.Generate
       ( caut2hs
       ) where

import Cauterize.GHC7.Options
import qualified Cauterize.Specification as Spec

import Cauterize.GHC7.Generate.Utils

import qualified Cauterize.GHC7.Generate.GenTypes as GenTypes
import qualified Cauterize.GHC7.Generate.GenCabal as GenCabal
import qualified Cauterize.GHC7.Generate.GenStack as GenStack
import qualified Cauterize.GHC7.Generate.GenCrucibleClient as GenCrucibleClient

caut2hs :: CautGHC7Opts -> IO ()
caut2hs (CautGHC7Opts { specFile = specPath, outputDirectory = outPath }) = createGuard outPath $ do
  spec <- Spec.parseSpecificationFromFile specPath

  case spec of
    Left es -> error $ show es
    Right s' -> generateOutput s' outPath

generateOutput :: Spec.Specification -> FilePath -> IO ()
generateOutput spec out = do
  GenStack.generateOutput spec out
  GenCabal.generateOutput spec out
  GenCrucibleClient.generateOutput spec out
  GenTypes.generateOutput spec out
