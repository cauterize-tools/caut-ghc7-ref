{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, OverloadedStrings #-}
module Main
  ( main
  ) where

import Cauterize.GHC7.Support.BuiltIn
import Cauterize.GHC7.Support.Prototypes
import Cauterize.GHC7.Support.Result
import CerealPlus.Serializable
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as B

data ABool = ABool CBool deriving (Show)
instance CautType ABool where; cautName _ = "a_bool"
instance CautSynonym ABool where
instance Serializable CautResult ABool where
  serialize t@(ABool a) = genSynonymSerialize a t
  deserialize = genSynonymDeserialize (undefined :: ABool) ABool

data AnArray = AnArray (V.Vector CBool) deriving (Show)
instance CautType AnArray where; cautName _ = "an_array"
instance CautArray AnArray where; arrayLength _ = 4
instance Serializable CautResult AnArray where
  serialize t@(AnArray vs) = genArraySerialize vs t
  deserialize = genArrayDeserialize (undefined :: AnArray) AnArray

data AVector = AVector (V.Vector CBool) deriving (Show)
instance CautType AVector where; cautName _ = "a_vector"
instance CautVector AVector where; vectorMaxLength _ = 4; vectorTagWidth _ = 1
instance Serializable CautResult AVector where
  serialize t@(AVector vs) = genVectorSerialize vs t
  deserialize = genVectorDeserialize (undefined :: AVector) AVector

data ARecord
  = ARecord { aRecordFieldBool :: CBool
            , aRecordFieldU8 :: U8
            } deriving (Show, Ord, Eq)
instance CautType ARecord where; cautName _ = "a_record"
instance CautRecord ARecord where
instance Serializable CautResult ARecord where
  serialize r = traceRecord $ do
    genFieldSerialize "field_bool" (aRecordFieldBool r)
    genFieldSerialize "field_u8" (aRecordFieldU8 r)
    where
      traceRecord = withTrace (TRecord $ cautName r)
  deserialize = traceRecord $ do
    a0 <- genFieldDeserialize "field_bool"
    a1 <- genFieldDeserialize "field_u8"
    return $ ARecord a0 a1
    where
      traceRecord = withTrace (TRecord $ cautName (undefined :: ARecord))

data ACombination
  = ACombination { aCombinationFieldBool :: Maybe CBool
                 , aCombinationFieldU8 :: Maybe U8
                 } deriving (Show, Ord, Eq)
instance CautType ACombination where; cautName _ = "a_combination"
instance CautCombination ACombination where; combinationTagWidth _ = 1
instance Serializable CautResult ACombination where
  serialize r = traceComb $ do
    encodeCombTag r [ fieldPresent $ aCombinationFieldBool r
                    , fieldPresent $ aCombinationFieldU8 r
                    ]
    genCombFieldSerialize "field_bool" $ aCombinationFieldBool r
    genCombFieldSerialize "field_u8" $ aCombinationFieldU8 r
    where
      traceComb = withTrace (TCombination $ cautName r)
  deserialize = traceComb $ do
    flags <- decodeCombTag (undefined :: ACombination)
    a0 <- genCombFieldDeserialize "field_bool" (flags `isFlagSet` 0)
    a1 <- genCombFieldDeserialize "field_u8" (flags `isFlagSet` 1)
    return $ ACombination a0 a1
    where
      traceComb = withTrace (TCombination $ cautName (undefined :: ACombination))

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

  print (encode (AVector (V.fromList [CBool True])))
  print (decode (B.pack [3,1,0,1]) :: Either CautError (AVector, B.ByteString))

  print (encode (AVector (V.fromList [CBool True])))
  print (decode (B.pack [9,1,0,1]) :: Either CautError (AVector, B.ByteString))

  print (encode (ARecord (CBool True) (U8 10)))
  print (decode (B.pack [0, 10]) :: Either CautError (ARecord, B.ByteString))
  print (decode (B.pack [3, 10]) :: Either CautError (ARecord, B.ByteString))

  print (encode (ACombination (Just $ CBool False) (Just $ U8 5)))
  print (decode (B.pack [1,2]) :: Either CautError (ACombination, B.ByteString))

