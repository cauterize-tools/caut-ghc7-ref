{-# LANGUAGE QuasiQuotes #-}
module Cauterize.GHC7.Generate.GenCabal
       ( generateOutput
       ) where

import Cauterize.GHC7.Generate.Utils
import Data.String.Interpolate.IsString
import Data.String.Interpolate.Util
import System.FilePath.Posix
import qualified Cauterize.Specification as Spec
import qualified Data.Text as T

generateOutput :: Spec.Specification -> FilePath -> IO ()
generateOutput spec out = do
  genDir <- createPath [out]
  let genPath = genDir `combine` (specName ++ ".cabal")
  let genData = genTempl specName hsName
  writeFile genPath genData
  where
    specName = T.unpack (Spec.specName spec)
    hsName = nameToHsName (Spec.specName spec)

genTempl :: String -> String -> String
genTempl name libname = unindent [i|
  name:                #{hyphenName}
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
  where
    hyphenName = map (\c -> if '_' == c then '-' else c) name
