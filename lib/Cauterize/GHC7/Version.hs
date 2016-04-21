{-# LANGUAGE TemplateHaskell #-}

module Cauterize.GHC7.Version
  ( cabalVersion
  , versionInfo
  , versionString
  , dependencyString
  ) where

import Development.GitRev
import qualified Paths_caut_ghc7_ref as C (version)
import qualified Cauterize.Version as Caut
import Data.Version (showVersion)

cabalVersion :: String
cabalVersion = showVersion C.version

versionInfo :: [(String, String)]
versionInfo =
  [ ("version", cabalVersion)
  , ("git branch", $(gitBranch))
  , ("git commit", $(gitHash))
  , ("git dirty", dirty)
  , ("git commit date", $(gitCommitDate))
  ]
  where
  dirty | $(gitDirty) = "yes"
        | otherwise = "no"

dependencyInfo :: [(String, [(String, String)])]
dependencyInfo =
  [("cauterize", Caut.versionInfo)]

versionString :: String
versionString = unlines
  [ k ++ ": " ++ v | (k, v) <- versionInfo]

dependencyString :: String
dependencyString = unlines
  [ d ++ " " ++ k ++ ": " ++ v
  | (d, kvs) <- dependencyInfo
  , (k, v) <- kvs
  ]

