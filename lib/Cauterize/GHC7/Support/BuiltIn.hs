{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, OverloadedStrings #-}
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

import Cauterize.GHC7.Support.Result
import Cauterize.GHC7.Support.Prototypes

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

instance CautType U8  where; cautName _ = "u8"
instance CautType U16 where; cautName _ = "u16"
instance CautType U32 where; cautName _ = "u32"
instance CautType U64 where; cautName _ = "u64"

instance CautType S8  where; cautName _ = "s8"
instance CautType S16 where; cautName _ = "s16"
instance CautType S32 where; cautName _ = "s32"
instance CautType S64 where; cautName _ = "s64"

instance CautType F32 where; cautName _ = "f32"
instance CautType F64 where; cautName _ = "f64"

instance CautType CBool where; cautName _ = "bool"

instance Serializable CautResult U8  where; serialize (U8  w) = traceSerializeBI tU8  w; deserialize = traceDeserializeBI tU8  U8
instance Serializable CautResult U16 where; serialize (U16 w) = traceSerializeBI tU16 w; deserialize = traceDeserializeBI tU16 U16
instance Serializable CautResult U32 where; serialize (U32 w) = traceSerializeBI tU32 w; deserialize = traceDeserializeBI tU32 U32
instance Serializable CautResult U64 where; serialize (U64 w) = traceSerializeBI tU64 w; deserialize = traceDeserializeBI tU64 U64

instance Serializable CautResult S8  where; serialize (S8  w) = traceSerializeBI tS8  w; deserialize = traceDeserializeBI tS8  S8
instance Serializable CautResult S16 where; serialize (S16 w) = traceSerializeBI tS16 w; deserialize = traceDeserializeBI tS16 S16
instance Serializable CautResult S32 where; serialize (S32 w) = traceSerializeBI tS32 w; deserialize = traceDeserializeBI tS32 S32
instance Serializable CautResult S64 where; serialize (S64 w) = traceSerializeBI tS64 w; deserialize = traceDeserializeBI tS64 S64

instance Serializable CautResult F32 where
  serialize (F32 w) = withTrace tF32 $ (liftPut . putFloat32le) w
  deserialize = liftGet $ liftM F32 getFloat32le
instance Serializable CautResult F64 where
  serialize (F64 w) = withTrace tF64 $ (liftPut . putFloat64le) w
  deserialize = liftGet $ liftM F64 getFloat64le

instance Serializable CautResult CBool where
  serialize (CBool True)  = traceSerializeBI tCBool (1 :: Word8)
  serialize (CBool False) = traceSerializeBI tCBool (0 :: Word8)
  deserialize = withTrace tCBool $ do
    v <- deserialize :: Deserialize CautResult Word8
    case v of
       1 -> return $ CBool True
       0 -> return $ CBool False
       x -> failWithTrace ("Invalid boolean value: " `T.append` tshow x)

-- Make a showable into Text
tshow :: Show a => a -> T.Text
tshow = T.pack . show

traceSerializeBI :: Serializable CautResult a => Trace -> a -> Serialize CautResult ()
traceSerializeBI t v = withTrace t (serialize v)

traceDeserializeBI :: Serializable CautResult b => Trace -> (b -> a) -> Deserialize CautResult a
traceDeserializeBI t c = withTrace t (liftM c deserialize)

traceBI :: CautType a => a -> Trace
traceBI t = TBuiltIn (cautName t)

tU8, tU16, tU32, tU64 :: Trace
tU8 = traceBI (undefined :: U8)
tU16 = traceBI (undefined :: U16)
tU32 = traceBI (undefined :: U32)
tU64 = traceBI (undefined :: U64)

tS8, tS16, tS32, tS64 :: Trace
tS8 = traceBI (undefined :: S8)
tS16 = traceBI (undefined :: S16)
tS32 = traceBI (undefined :: S32)
tS64 = traceBI (undefined :: S64)

tF32, tF64 :: Trace
tF32 = traceBI (undefined :: F32)
tF64 = traceBI (undefined :: F64)

tCBool :: Trace
tCBool = traceBI (undefined :: CBool)
