module Main
  ( main
  ) where

import Cauterize.GHC7.Generate
import Cauterize.GHC7.Options
import Cauterize.GHC7.Version

import Options.Applicative (execParser)

main :: IO ()
main = runWithOptions caut2hs

runWithOptions :: (CautGHC7Opts -> IO ()) -> IO ()
runWithOptions fn = do
  mopts <- execParser options
  case mopts of
    Just opts -> fn opts
    Nothing -> do
      putStr versionString
      putStrLn "dependencies:"
      putStr dependencyString

