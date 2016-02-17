{-# LANGUAGE QuasiQuotes #-}
module Cauterize.GHC7.Generate.GenStack
       ( generateOutput
       ) where

import Cauterize.GHC7.Options
import Cauterize.GHC7.Generate.Utils
import qualified Cauterize.Specification as Spec
import System.FilePath.Posix
import Data.String.Interpolate.Util
import Data.String.Interpolate.IsString

generateOutput :: Spec.Specification -> CautGHC7Opts -> IO ()
generateOutput _ opts = do
    stackDir <- createPath [out]
    let stackPath = stackDir `combine` "stack.yaml"
    let stackData = stackYamlTempl
    writeFile stackPath stackData
  where
    out = outputDirectory opts

stackYamlTempl :: String
stackYamlTempl = unindent [i|
  flags: {}
  packages:
  - '.'
  - location:
      git: https://github.com/cauterize-tools/cauterize.git
      commit: cff794399744a4038c0f2b2dfbc4c43593d2bcbd
  - location:
      git: https://github.com/aisamanra/s-cargot.git
      commit: 1628a7c2913fc5e72ab6ea9a813762bf86d47d49
  - location:
      git: https://github.com/cauterize-tools/crucible.git
      commit: 7f8147e0fbfe210df9e6c83f4c0d48c3de4ed9f7
  - location:
      git: https://github.com/cauterize-tools/caut-ghc7-ref.git
      commit: 36f28786cd97cf9e810649d75270b2ac0cb8d1a5
  extra-deps:
    - cereal-plus-0.4.0
  resolver: lts-2.21|]
