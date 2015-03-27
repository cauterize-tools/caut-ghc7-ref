module Main
  ( main
  ) where

import Cauterize.GHC7.Support.BuiltIn
import Cauterize.GHC7.Support.Prototypes
import Cauterize.GHC7.Support.Result
import qualified Data.ByteString.Lazy as B

main :: IO ()
main = do
  case encode (U64 54) of
    Left e -> print e
    Right b -> print $ B.unpack b

  print (decode (B.pack [5]) :: Either CautError (CBool, B.ByteString))
