module Cauterize.GHC7.Options
  ( CautGHC7Opts(..)
  ) where

data CautGHC7Opts = CautGHC7Opts
  { specFile :: FilePath
  , outputDirectory :: FilePath
  , modulePath :: Maybe String
  } deriving (Show)
