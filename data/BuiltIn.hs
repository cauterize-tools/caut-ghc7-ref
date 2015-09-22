{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, OverloadedStrings #-}
module Cauterize.Generated.{{hscLibName}}.BuiltIn
  ( U8(..) , U16(..) , U32(..) , U64(..)
  , S8(..) , S16(..) , S32(..) , S64(..)
  , F32(..), F64(..)
  , CBool(..)
  ) where

import CerealPlus.Serialize
import CerealPlus.Deserialize
import CerealPlus.Serializable
import Data.Serialize.Get
import Data.Serialize.Put
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

instance Serializable CautResult U8  where
  serialize (U8 w) = traceSerializeBI putWord8 tU8 w
  deserialize = traceDeserializeBI getWord8 tU8 U8
instance Serializable CautResult U16 where
  serialize (U16 w) = traceSerializeBI putWord16le tU16 w
  deserialize = traceDeserializeBI getWord16le tU16 U16
instance Serializable CautResult U32 where
  serialize (U32 w) = traceSerializeBI putWord32le tU32 w
  deserialize = traceDeserializeBI getWord32le tU32 U32
instance Serializable CautResult U64 where
  serialize (U64 w) = traceSerializeBI putWord64le tU64 w
  deserialize = traceDeserializeBI getWord64le tU64 U64

instance Serializable CautResult S8  where
  serialize (S8  w) = traceSerializeBI putWord8 tS8 (fromIntegral w)
  deserialize = traceDeserializeBI getWord8 tS8 (S8 . fromIntegral)
instance Serializable CautResult S16 where
  serialize (S16 w) = traceSerializeBI putWord16le tS16 (fromIntegral w)
  deserialize = traceDeserializeBI getWord16le tS16 (S16 . fromIntegral)
instance Serializable CautResult S32 where
  serialize (S32 w) = traceSerializeBI putWord32le tS32 (fromIntegral w)
  deserialize = traceDeserializeBI getWord32le tS32 (S32 . fromIntegral)
instance Serializable CautResult S64 where
  serialize (S64 w) = traceSerializeBI putWord64le tS64 (fromIntegral w)
  deserialize = traceDeserializeBI getWord64le tS64 (S64 . fromIntegral)

instance Serializable CautResult F32 where
  serialize (F32 w) = traceSerializeBI putFloat32le tF32 w
  deserialize = traceDeserializeBI getFloat32le tF32 F32
instance Serializable CautResult F64 where
  serialize (F64 w) = traceSerializeBI putFloat64le tF64 w
  deserialize = traceDeserializeBI getFloat64le tF64 F64

instance Serializable CautResult CBool where
  serialize (CBool True)  = traceSerializeBI putWord8 tCBool (1 :: Word8)
  serialize (CBool False) = traceSerializeBI putWord8 tCBool (0 :: Word8)
  deserialize = withTrace tCBool $ do
    v <- deserialize :: Deserialize CautResult Word8
    case v of
       1 -> return $ CBool True
       0 -> return $ CBool False
       x -> failWithTrace ("Invalid boolean value: " `T.append` tshow x)

{{#hscTypes}}
{{#HsTBuiltIn}}
{{#hstDetail}}
instance CautType {{hstConstructor}} where
  cautName _ = "{{hstName}}"
  cautHash _ = {{hstHashListStr}}
  cautSize _ = ({{hstSize.hstMinSize}},{{hstSize.hstMaxSize}})
{{/hstDetail}}

{{/HsTBuiltIn}}
{{/hscTypes}}

-- Make a showable into Text
tshow :: Show a => a -> T.Text
tshow = T.pack . show

traceSerializeBI :: (s -> Put) -> Trace -> s -> Serialize CautResult ()
traceSerializeBI m t v = withTrace t (liftPut . m $ v)

traceDeserializeBI :: Get b -> Trace -> (b -> a) -> Deserialize CautResult a
traceDeserializeBI m t c = withTrace t $ liftGet (liftM c m)

tU8, tU16, tU32, tU64 :: Trace
tU8 = TBuiltIn "u8"
tU16 = TBuiltIn "u16"
tU32 = TBuiltIn "u32"
tU64 = TBuiltIn "u64"

tS8, tS16, tS32, tS64 :: Trace
tS8 = TBuiltIn "s8"
tS16 = TBuiltIn "s16"
tS32 = TBuiltIn "s32"
tS64 = TBuiltIn "s64"

tF32, tF64 :: Trace
tF32 = TBuiltIn "f32"
tF64 = TBuiltIn "f64"

tCBool :: Trace
tCBool = TBuiltIn "bool"