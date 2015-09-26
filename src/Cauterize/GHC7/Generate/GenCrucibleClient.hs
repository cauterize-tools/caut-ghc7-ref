{-# LANGUAGE QuasiQuotes #-}
module Cauterize.GHC7.Generate.GenCrucibleClient
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
  genDir <- createPath [out, "crucible"]
  let genPath = genDir `combine` "Main.hs"
  let genData = genTempl hsName specName
  writeFile genPath genData
  where
    specName = T.unpack (Spec.specName spec)
    hsName = nameToHsName (Spec.specName spec)

genTempl :: String -> String -> String
genTempl _ _ = unindent [i|
    module Main where

    main :: IO ()
    main = return ()
  |]
