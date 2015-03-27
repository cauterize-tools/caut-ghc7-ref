{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, OverloadedStrings #-}
module Main
  ( main
  ) where

import Cauterize.GHC7.Support.BuiltIn
import Cauterize.GHC7.Support.Prototypes
import Cauterize.GHC7.Support.Result
import CerealPlus.Serializable
import CerealPlus.Serialize
import CerealPlus.Deserialize
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
instance Serializable CautResult AnArray where
  serialize t@(AnArray vs) = genArraySerialize vs t
  deserialize = genArrayDeserialize (undefined :: AnArray) AnArray

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
