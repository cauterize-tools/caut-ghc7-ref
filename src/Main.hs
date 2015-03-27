{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}
module Main
  ( main
  ) where

import Cauterize.GHC7.Support.BuiltIn
import Cauterize.GHC7.Support.Prototypes
import Cauterize.GHC7.Support.Result
import CerealPlus.Serializable
import Control.Monad
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T

data ABool = ABool CBool deriving (Show)
instance CautType ABool where; cautName _ = "a_bool"
instance CautSynonym ABool where
tABool :: Trace
tABool = TSynonym $ cautName (undefined :: ABool)
instance Serializable CautResult ABool where
  serialize (ABool a) = withTrace tABool (serialize a)
  deserialize = withTrace tABool (liftM ABool deserialize)

data AnArray = AnArray (V.Vector CBool) deriving (Show)
instance CautType AnArray where; cautName _ = "an_array"
instance CautArray AnArray where; arrayLength _ = 4
tAnArray :: Trace
tAnArray = TArray $ cautName (undefined :: AnArray)
lAnArray :: Int
lAnArray = arrayLength (undefined :: AnArray)
instance Serializable CautResult AnArray where
  serialize (AnArray vs) = withTrace tAnArray $ if V.length vs /= lAnArray then fwt else V.mapM_ go vsWithIx
    where
      fwt = failWithTrace $ T.concat ["Unexpected length: ", tshow (V.length vs), ". Expected ", tshow lAnArray, "."]
      vsWithIx = V.zip (V.fromList [0..(V.length vs - 1)]) vs
      go (ix, v) = withTrace (TArrayIndex ix) (serialize v)
  deserialize = withTrace tAnArray $ liftM (AnArray . V.fromList) (mapM go ixs)
    where
      ixs = [0..lAnArray - 1]
      go ix = withTrace (TArrayIndex ix) deserialize


main :: IO ()
main = do
  case encode (U64 54) of
    Left e -> print e
    Right b -> print $ B.unpack b

  print (decode (B.pack [5]) :: Either CautError (CBool, B.ByteString))
  print (decode (B.pack [5]) :: Either CautError (ABool, B.ByteString))

  print (encode (AnArray (V.fromList [CBool False,CBool True,CBool False,CBool True])))
  print (decode (B.pack [0,1,0,1]) :: Either CautError (AnArray, B.ByteString))

  print (encode (AnArray (V.fromList [CBool False, CBool False,CBool True,CBool False,CBool True])))
  print (decode (B.pack [0,1,2,1]) :: Either CautError (AnArray, B.ByteString))

-- Make a showable into Text
tshow :: Show a => a -> T.Text
tshow = T.pack . show
