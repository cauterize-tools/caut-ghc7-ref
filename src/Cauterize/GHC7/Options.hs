module Cauterize.GHC7.Options
  ( CautGHC7Opts(..)
  , modulePathAsList
  , options
  ) where

import Options.Applicative

data CautGHC7Opts = CautGHC7Opts
  { specFile :: FilePath
  , outputDirectory :: FilePath
  , modulePath :: Maybe String
  } deriving (Show)

modulePathAsList :: CautGHC7Opts -> Maybe [String]
modulePathAsList opts = splitWith (== '.') <$> modulePath opts
  where
  splitWith :: (a -> Bool) -> [a] -> [[a]]
  splitWith _    [] = []
  splitWith cond xs = first : splitWith cond (safeTail rest)
      where (first, rest) = break cond xs
  safeTail [] = []
  safeTail (_:ys) = ys

optParser :: Parser CautGHC7Opts
optParser = CautGHC7Opts
  <$> argument str
    ( metavar "SPEC"
   `mappend` help "Cauterize specification"
    )
  <*> argument str
    ( metavar "OUTDIR"
   `mappend` help "Output directory"
    )
  <*> option (Just <$> str)
    ( long "module-path"
   `mappend` metavar "Module.Path"
   `mappend` help "override default haskell module path"
   `mappend` value Nothing
    )

options :: ParserInfo (Maybe CautGHC7Opts)
options = info (helper <*> o)
   ( fullDesc
  `mappend` progDesc "Translate a Cauterize specification into a Haskell implementation"
   )
  where
  o = flag' Nothing (long "version" `mappend` hidden)
   <|> (Just <$> optParser)
