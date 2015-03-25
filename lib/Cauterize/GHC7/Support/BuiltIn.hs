{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
module Cauterize.GHC7.Support.BuiltIn
  ( U8(..) , U16(..) , U32(..) , U64(..)
  , S8(..) , S16(..) , S32(..) , S64(..)
  , F32(..), F64(..)
  , CBool(..)
  ) where

import CerealPlus.Deserialize
import CerealPlus.Serializable
import CerealPlus.Serialize
import Data.Serialize.IEEE754
import Control.Monad
import Data.Int
import Data.Word
import qualified Data.Text as T

import Cauterize.GHC7.Support.Types

tshow :: Show a => a -> T.Text
tshow = T.pack . show

newtype U8  = U8  { unU8  :: Word8 } deriving (Show, Eq, Ord)
newtype U16 = U16 { unU16 :: Word16 } deriving (Show, Eq, Ord)
newtype U32 = U32 { unU32 :: Word32 } deriving (Show, Eq, Ord)
newtype U64 = U64 { unU64 :: Word64 } deriving (Show, Eq, Ord)

newtype S8  = S8  { unS8  :: Int8 } deriving (Show, Eq, Ord)
newtype S16 = S16 { unS16 :: Int16 } deriving (Show, Eq, Ord)
newtype S32 = S32 { unS32 :: Int32 } deriving (Show, Eq, Ord)
newtype S64 = S64 { unS64 :: Int64 } deriving (Show, Eq, Ord)

newtype F32 = F32 { unF32 :: Float } deriving (Show, Eq, Ord)
newtype F64 = F64 { unF64 :: Double } deriving (Show, Eq, Ord)

newtype CBool = CBool { unCBool :: Bool } deriving (Show, Eq, Ord)

instance Serializable CautResult U8  where; serialize (U8  w) = serialize w; deserialize = liftM U8  deserialize
instance Serializable CautResult U16 where; serialize (U16 w) = serialize w; deserialize = liftM U16 deserialize
instance Serializable CautResult U32 where; serialize (U32 w) = serialize w; deserialize = liftM U32 deserialize
instance Serializable CautResult U64 where; serialize (U64 w) = serialize w; deserialize = liftM U64 deserialize

instance Serializable CautResult S8  where; serialize (S8  w) = serialize w; deserialize = liftM S8  deserialize
instance Serializable CautResult S16 where; serialize (S16 w) = serialize w; deserialize = liftM S16 deserialize
instance Serializable CautResult S32 where; serialize (S32 w) = serialize w; deserialize = liftM S32 deserialize
instance Serializable CautResult S64 where; serialize (S64 w) = serialize w; deserialize = liftM S64 deserialize

instance Serializable CautResult F32 where; serialize (F32 w) = (liftPut . putFloat32le) w; deserialize = liftGet $ liftM F32 getFloat32le
instance Serializable CautResult F64 where; serialize (F64 w) = (liftPut . putFloat64le) w; deserialize = liftGet $ liftM F64 getFloat64le

instance Serializable CautResult CBool where
  serialize (CBool True) = serialize (1 :: Word8)
  serialize (CBool False) = serialize (0 :: Word8)
  deserialize = do v <- deserialize :: Deserialize CautResult Word8
                   case v of
                      1 -> return $ CBool True
                      0 -> return $ CBool False
                      x -> failWithTrace ("Invalid boolean value: " `T.append` tshow x)
