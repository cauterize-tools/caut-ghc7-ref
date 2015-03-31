module Cauterize.GHC7.Options
  ( CautGHC7Opts(..)
  ) where

data CautGHC7Opts = CautGHC7Opts
  { specFile :: FilePath
  , metaFile :: FilePath
  , outputDirectory :: FilePath
  } deriving (Show)
