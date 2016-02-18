{-# LANGUAGE QuasiQuotes #-}
module Cauterize.GHC7.Generate.GenCabal
       ( generateOutput
       ) where

import Cauterize.GHC7.Generate.Utils
import Cauterize.GHC7.Options
import Data.String.Interpolate.IsString
import Data.String.Interpolate.Util
import System.FilePath.Posix
import qualified Cauterize.Specification as Spec
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Data.List (intercalate)

generateOutput :: Spec.Specification -> CautGHC7Opts -> IO ()
generateOutput spec opts = do
  genDir <- createPath [out]
  let genPath = genDir `combine` (specName ++ ".cabal")
  let genData = genTempl specName hsName
  writeFile genPath genData
  where
    specName = T.unpack (Spec.specName spec)
    hsName = fromMaybe
      (intercalate "." ["Cauterize", "Generated", nameToHsName (Spec.specName spec)])
      (modulePath opts)
    out = outputDirectory opts

genTempl :: String -> String -> String
genTempl name modname = unindent [i|
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
    exposed-modules:     #{modname}.Types
    build-depends:       base < 5,
                         caut-ghc7-ref,
                         cereal,
                         cereal-plus,
                         text

  |]
