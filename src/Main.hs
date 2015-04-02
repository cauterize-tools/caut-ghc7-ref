module Main
  ( main
  ) where

import Cauterize.GHC7
import Cauterize.GHC7.Options

import Options.Applicative

main :: IO ()
main = runWithOptions caut2hs

runWithOptions :: (CautGHC7Opts -> IO ()) -> IO ()
runWithOptions fn = execParser options >>= fn

optParser :: Parser CautGHC7Opts
optParser = CautGHC7Opts
  <$> strOption
    ( long "spec"
   <> metavar "SPEC_PATH"
   <> help "Cauterize specification file"
    )
  <*> strOption
    ( long "output"
   <> metavar "OUTPUT_PATH"
   <> help "Directory to save output files"
    )

options :: ParserInfo CautGHC7Opts
options = info (optParser <**> helper)
            ( fullDesc
           <> progDesc "Translate a Cauterize specification and meta file into a Haskell library."
            )
