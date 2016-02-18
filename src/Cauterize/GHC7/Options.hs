module Cauterize.GHC7.Options
  ( CautGHC7Opts(..)
  , modulePathAsList
  ) where

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

