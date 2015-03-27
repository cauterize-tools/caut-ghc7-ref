module Cauterize.GHC7.Support.Cauterize
  ( Transcodable(..)
  ) where

import Cauterize.GHC7.Support.Result
import Cauterize.GHC7.Support.Prototypes
import qualified Data.ByteString.Lazy as B

class CautType a => Transcodable a where
  encode :: a -> Either CautError B.ByteString
  decode :: B.ByteString -> Either CautError a
