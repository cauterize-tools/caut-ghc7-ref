module Cauterize.GHC7.Support.Prototypes
  ( CautType(..)
  ) where

import qualified Data.Text as T

class CautType a where
  cautName :: a -> T.Text
  -- cautHash :: a -> Hash
  -- cautSize :: a -> (MinSize, MaxSize)
