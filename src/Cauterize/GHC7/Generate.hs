{-# LANGUAGE RecordWildCards #-}
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
caut2hs (opts@CautGHC7Opts{..}) = createGuard outputDirectory $ do
  spec <- Spec.parseSpecificationFromFile specFile

  case spec of
    Left es -> error $ show es
    Right s' -> generateOutput s' opts

generateOutput :: Spec.Specification -> CautGHC7Opts -> IO ()
generateOutput spec opts = do
  GenStack.generateOutput spec opts
  GenCabal.generateOutput spec opts
  GenCrucibleClient.generateOutput spec opts
  GenTypes.generateOutput spec opts
