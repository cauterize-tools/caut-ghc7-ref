{-# LANGUAGE FlexibleContexts, OverloadedStrings, MultiParamTypeClasses #-}
module Cauterize.GHC7.Support.Prototypes
  ( CautType(..)
  , CautTranscodable(..)

  , CautSynonym
  , CautRange(..)
  , CautArray(..)
  , CautVector(..)
  , CautEnumeration(..)
  , CautResult(..)
  , CautRecord
  , CautCombination(..)
  , CautUnion(..)

  , Hash
  , MinSize
  , MaxSize

  , genSynonymSerialize
  , genSynonymDeserialize
  , genRangeSerialize
  , genRangeDeserialize
  , genArraySerialize
  , genArrayDeserialize
  , genVectorSerialize
  , genVectorDeserialize
  , genEnumerationSerialize
  , genEnumerationDeserialize
  , genFieldSerialize
  , genFieldDeserialize
  , genCombFieldSerialize
  , genCombFieldDeserialize
  , genUnionFieldSerialize
  , genUnionFieldSerializeEmpty
  , genUnionFieldDeserialize

  , encodeCombTag
  , decodeCombTag
  , decodeUnionTag
  , failUnionTag
  , fieldPresent
  , isFlagSet

-- exports for convenience
  , V.Vector
  , Serializable(..)
  , ap

  , Int8, Int16, Int32, Int64
  , Word8, Word16, Word32, Word64
  ) where

import Cauterize.GHC7.Support.Result
import CerealPlus.Deserialize
import CerealPlus.Serializable
import CerealPlus.Serialize
import qualified Data.Serialize.Put as P
import qualified Data.Serialize.Get as G
import qualified Data.Serialize.IEEE754 as F
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Data.Word
import Data.Int
import Data.Maybe
import Data.Bits
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified Data.Vector as V

class Serializable CautResult a => CautTranscodable a where
  cautName :: a -> T.Text
  cautSize :: a -> (MinSize, MaxSize)

  custSerialize :: a -> Serialize CautResult ()
  custSerialize = serialize

  custDeserialize :: Deserialize CautResult a
  custDeserialize = deserialize

  encode :: a -> Either CautError B.ByteString
  encode a = let CautResult r = runLazy $ custSerialize a
             in case runReaderT r [] of
                  Left e -> Left e
                  Right ((), b) -> Right b

  decode :: B.ByteString -> Either CautError (a, B.ByteString)
  decode b = let b' = B.toStrict b
                 CautResult r = runPartial custDeserialize b'
             in case runReaderT r [] of
                  Left e -> Left e
                  Right (Fail msg _) -> Left (CautError msg [])
                  Right (Partial _) -> Left (CautError "Ran out of bytes." [])
                  Right (Done a bs) -> Right (a, B.fromStrict bs)

traceSerializePrim :: (s -> P.Put) -> Trace -> s -> Serialize CautResult ()
traceSerializePrim m t v = withTrace t (liftPut . m $ v)

traceDeserializePrim :: G.Get b -> Trace -> (b -> a) -> Deserialize CautResult a
traceDeserializePrim m t c = withTrace t $ liftGet (liftM c m)

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

instance CautTranscodable Word8 where
  cautName = const "u8"
  cautSize = const (1,1)
  custSerialize w = traceSerializePrim P.putWord8 tU8 w
  custDeserialize = traceDeserializePrim G.getWord8 tU8 id
instance CautTranscodable Word16 where
  cautName = const "u16"
  cautSize = const (2,2)
  custSerialize w = traceSerializePrim P.putWord16le tU16 w
  custDeserialize = traceDeserializePrim G.getWord16le tU16 id
instance CautTranscodable Word32 where
  cautName = const "u32"
  cautSize = const (4,4)
  custSerialize w = traceSerializePrim P.putWord32le tU32 w
  custDeserialize = traceDeserializePrim G.getWord32le tU32 id
instance CautTranscodable Word64 where
  cautName = const "u64"
  cautSize = const (8,8)
  custSerialize w = traceSerializePrim P.putWord64le tU8 w
  custDeserialize = traceDeserializePrim G.getWord64le tU64 id

instance CautTranscodable Int8 where
  cautName = const "s8"
  cautSize = const (1,1)
  custSerialize w = traceSerializePrim P.putWord8 tS8 (fromIntegral w)
  custDeserialize = traceDeserializePrim G.getWord8 tS8 fromIntegral
instance CautTranscodable Int16 where
  cautName = const "s16"
  cautSize = const (2,2)
  custSerialize w = traceSerializePrim P.putWord16le tS16 (fromIntegral w)
  custDeserialize = traceDeserializePrim G.getWord16le tS16 fromIntegral
instance CautTranscodable Int32 where
  cautName = const "s32"
  cautSize = const (4,4)
  custSerialize w = traceSerializePrim P.putWord32le tS32 (fromIntegral w)
  custDeserialize = traceDeserializePrim G.getWord32le tS32 fromIntegral
instance CautTranscodable Int64 where
  cautName = const "s64"
  cautSize = const (8,8)
  custSerialize w = traceSerializePrim P.putWord64le tS64 (fromIntegral w)
  custDeserialize = traceDeserializePrim G.getWord64le tS64 fromIntegral

instance CautTranscodable Float where
  cautName = const "f32"
  cautSize = const (4,4)
  custSerialize w = traceSerializePrim F.putFloat32le tF32 w
  custDeserialize = traceDeserializePrim F.getFloat32le tF32 id
instance CautTranscodable Double where
  cautName = const "f64"
  cautSize = const (8,8)
  custSerialize w = traceSerializePrim F.putFloat64le tF64 w
  custDeserialize = traceDeserializePrim F.getFloat64le tF64 id

instance CautTranscodable Bool where
  cautName = const "bool"
  cautSize = const (1,1)
  custSerialize True = traceSerializePrim P.putWord8 tCBool (1 :: Word8)
  custSerialize False = traceSerializePrim P.putWord8 tCBool (0 :: Word8)
  custDeserialize = withTrace tCBool $ do
    v <- deserialize :: Deserialize CautResult Word8
    case v of
       1 -> return True
       0 -> return False
       x -> failWithTrace ("Invalid boolean value: " `T.append` tshow x)

class CautTranscodable a => CautType a where
  cautFingerprint :: a -> Hash

-- The following are instances for each of the Cauterize prototypes
class CautTranscodable a => CautSynonym a where

class CautTranscodable a => CautRange a where
  rangeTagWidth :: a -> Integer
  rangeOffset :: a -> Integer
  rangeLength :: a -> Integer

class CautTranscodable a => CautArray a where
  arrayLength :: a -> Integer

class CautTranscodable a => CautVector a where
  vectorMaxLength :: a -> Integer
  vectorTagWidth :: a -> Integer

class CautTranscodable a => CautEnumeration a where
  enumerationTagWidth :: a -> Integer
  enumerationMaxVal :: a -> Integer

class CautTranscodable a => CautRecord a where

class CautTranscodable a => CautCombination a where
  combinationTagWidth :: a -> Integer
  combinationMaxIndex :: a -> Integer

class CautTranscodable a => CautUnion a where
  unionTagWidth :: a -> Integer

type Hash = [Word8]
type MinSize = Integer
type MaxSize = Integer

genSynonymSerialize :: (CautSynonym a, CautTranscodable b)
                    => b -> a -> Serialize CautResult ()
genSynonymSerialize v t = withTrace (TSynonym $ cautName t) (custSerialize v)

genSynonymDeserialize :: (CautSynonym a, CautTranscodable b)
                      => a -> (b -> a) -> Deserialize CautResult a
genSynonymDeserialize t ctor = withTrace (TSynonym $ cautName t) (liftM ctor custDeserialize)

genRangeSerialize :: (Show b, Integral b, CautRange a, Serializable CautResult b)
                  => b -> a -> Serialize CautResult ()
genRangeSerialize v t = withTrace (TRange $ cautName t) $ do
  if fromIntegral v < rmin || rmax < fromIntegral v
     then failWithTrace $ T.concat ["Unexpected range value ", tshow v, ". Should be between ", tshow rmin, " and ", tshow rmax, "."]
     else tagEncode (fromIntegral $ rangeTagWidth t) tag
   where
    rmin = rangeOffset t
    rmax = rangeOffset t + rangeLength t
    tag = fromIntegral ((fromIntegral v) - (rangeOffset t)) :: Word64

genRangeDeserialize :: (Integral b, CautRange a, Serializable CautResult b)
                    => a -> (b -> a) -> Deserialize CautResult a
genRangeDeserialize t ctor = withTrace (TRange $ cautName t) $ do
  tag <- tagDecode (fromIntegral $ rangeTagWidth t)
  let val = fromIntegral tag + (rangeOffset t) :: Integer
  if val < rmin || rmax < val
     then failWithTrace $ T.concat ["Unexpected range value ", tshow val, ". Should be between ", tshow rmin, " and ", tshow rmax, "."]
     else (return . ctor . fromIntegral) val
  where
    rmin = rangeOffset t
    rmax = rangeOffset t + rangeLength t

genArraySerialize :: (CautArray a, CautTranscodable b)
                  => V.Vector b -> a -> Serialize CautResult ()
genArraySerialize vs t = withTrace tArray $ if V.length vs /= aLength then fwt else V.mapM_ go (vsWithIx vs)
  where
    tArray = TArray $ cautName t
    aLength = fromIntegral $ arrayLength t
    fwt = failWithTrace $ T.concat ["Unexpected length: ", tshow (V.length vs), ". Expected ", tshow aLength, "."]
    go (ix, v) = withTrace (TArrayIndex ix) (custSerialize v)

genArrayDeserialize :: (CautArray a, CautTranscodable b)
                    => a -> (V.Vector b -> a) -> Deserialize CautResult a
genArrayDeserialize t ctor = withTrace tArray $ liftM (ctor . V.fromList) (mapM go ixs)
  where
    tArray = TArray $ cautName t
    aLength = fromIntegral $ arrayLength t
    ixs = [0..aLength - 1]
    go ix = withTrace (TArrayIndex ix) custDeserialize

genVectorSerialize :: (CautVector a, CautTranscodable b)
                   => V.Vector b -> a -> Serialize CautResult ()
genVectorSerialize vs t = withTrace (TVector . cautName $ t) $ if vl > maxLen
                              then fwt
                              else withTrace TVectorTag $ do
                                    tagEncode vTagWidth (fromIntegral vl)
                                    V.mapM_ go (vsWithIx vs)
  where
    vl = V.length vs
    maxLen = fromIntegral $ vectorMaxLength t
    vTagWidth = fromIntegral $ vectorTagWidth t
    fwt = failWithTrace $ T.concat ["Unexpected length: ", tshow (V.length vs), ". Expected <= ", tshow maxLen, "."]
    go (ix, v) = withTrace (TVectorIndex ix) (custSerialize v)

genVectorDeserialize :: (CautVector a, CautTranscodable b)
                     => a -> (V.Vector b -> a) -> Deserialize CautResult a
genVectorDeserialize t ctor = withTrace (TVector . cautName $ t) $ do
                                vlen <- withTrace TVectorTag $ do
                                  tag <- tagDecode vTagWidth
                                  if tag < 0 || maxLen < tag
                                    then failWithTrace $ "Unexpected length: " `T.append` tshow tag
                                    else return $ fromIntegral tag
                                liftM (ctor . V.fromList) (mapM go [0..vlen - 1])
  where
    maxLen = fromIntegral $ vectorMaxLength t
    vTagWidth = fromIntegral $ vectorTagWidth t
    go ix = withTrace (TArrayIndex ix) custDeserialize

genEnumerationSerialize :: (Enum a, CautEnumeration a, CautTranscodable a)
                        => a -> Serialize CautResult ()
genEnumerationSerialize v = withTrace (TEnumeration . cautName $ v) $ tagEncode (fromIntegral $ enumerationTagWidth v) tag
  where
    tag = (toEnum . fromEnum) v

genEnumerationDeserialize :: (Enum a, CautEnumeration a)
                          => a -> Deserialize CautResult a
genEnumerationDeserialize t = withTrace (TEnumeration . cautName $ t) $ do
  tag <- tagDecode (fromIntegral $ enumerationTagWidth t)
  if fromIntegral tag > (enumerationMaxVal t)
     then failWithTrace $ T.concat ["Enumeration value out of range. ", tshow tag, " > ", tshow (enumerationMaxVal t), "."]
     else (return . toEnum . fromEnum) tag

genFieldSerialize :: CautTranscodable a
                  => Trace -> a -> Serialize CautResult ()
genFieldSerialize t v = withTrace t (custSerialize v)

genFieldDeserialize :: CautTranscodable a
                    => Trace -> Deserialize CautResult a
genFieldDeserialize t = withTrace t custDeserialize

genCombFieldSerialize :: CautTranscodable a
                      => T.Text -> Maybe a -> Serialize CautResult ()
genCombFieldSerialize _ Nothing = return ()
genCombFieldSerialize t (Just f) = genFieldSerialize (TCombinationField t) f

genCombFieldDeserialize :: CautTranscodable a
                        => T.Text -> Bool -> Deserialize CautResult (Maybe a)
genCombFieldDeserialize _ False = return Nothing
genCombFieldDeserialize n True = liftM Just (genFieldDeserialize $ TCombinationField n)

vsWithIx :: V.Vector a -> V.Vector (Int, a)
vsWithIx vs = V.zip (V.fromList [0..(V.length vs - 1)]) vs

fieldPresent :: Maybe a -> Bool
fieldPresent = isJust

isFlagSet :: Word64 -> Int -> Bool
isFlagSet v ix = v `testBit` ix

calcCombTag :: [Bool] -> Word64
calcCombTag bits = foldl go zeroBits indexBits
  where
    go v (ix, True) = v `setBit` ix
    go v (_, False) = v
    indexBits = zip [0..] bits

encodeCombTag :: CautCombination a
              => a -> [Bool] -> Serialize CautResult ()
encodeCombTag t flags = withTrace TCombinationTag $ tagEncode (fromIntegral $ combinationTagWidth t) (calcCombTag flags)

decodeCombTag :: CautCombination a
              => a -> Deserialize CautResult Word64
decodeCombTag t = withTrace TCombinationTag $ do
  flags <- tagDecode (fromIntegral $ combinationTagWidth t)
  if flags < 2^maxIxP1
    then return flags
    else failWithTrace $ T.concat ["Expected combination flags < (2^", tshow maxIxP1, "). Got: ", tshow flags, "."]
  where
    maxIxP1 = combinationMaxIndex t + 1

genUnionFieldSerialize :: (CautUnion a, CautTranscodable b)
                       => a      -- ^ the union type
                       -> Word64 -- ^ the union tag value
                       -> T.Text -- ^ the union field name
                       -> b      -- ^ the union field value
                       -> Serialize CautResult ()
genUnionFieldSerialize u ix n v = do
  withTrace TUnionTag $ tagEncode (fromIntegral $ unionTagWidth u) ix
  genFieldSerialize (TUnionField n) v

genUnionFieldSerializeEmpty :: CautUnion a
                            => a -> Word64 -> Serialize CautResult ()
genUnionFieldSerializeEmpty u ix = withTrace TUnionTag $ tagEncode (fromIntegral $ unionTagWidth u) ix

genUnionFieldDeserialize :: CautTranscodable a
                         => T.Text -> (a -> r) -> Deserialize CautResult r
genUnionFieldDeserialize n ctor = liftM ctor $ genFieldDeserialize (TUnionField n)

decodeUnionTag :: CautUnion a
               => a -> Deserialize CautResult Word64
decodeUnionTag t = withTrace TUnionTag (tagDecode . fromIntegral $ unionTagWidth t)

failUnionTag :: (MonadTrans t, Show b, Monad (t CautResult))
             => b -> t CautResult a
failUnionTag v = failWithTrace $ "Unexpected tag: " `T.append` tshow v

tagEncode :: Int -> Word64 -> Serialize CautResult ()
tagEncode w i | i < 0 || (2^(8*w) - 1) < i = error $ "Value out of bounds for tag width " ++ show w ++ ": " ++ show i ++ "."
              | otherwise = case w of
                              1 -> let i' = fromIntegral i :: Word8  in custSerialize i'
                              2 -> let i' = fromIntegral i :: Word16 in custSerialize i'
                              4 -> let i' = fromIntegral i :: Word32 in custSerialize i'
                              8 -> let i' = fromIntegral i :: Word64 in custSerialize i'
                              _ -> error $ "Invalid tag width: " ++ show w

tagDecode :: Int -> Deserialize CautResult Word64
tagDecode w = case w of
               1 -> liftM fromIntegral (custDeserialize :: Deserialize CautResult Word8)
               2 -> liftM fromIntegral (custDeserialize :: Deserialize CautResult Word16)
               4 -> liftM fromIntegral (custDeserialize :: Deserialize CautResult Word32)
               8 -> liftM fromIntegral (custDeserialize :: Deserialize CautResult Word64)
               _ -> error $ "Invalid tag width: " ++ show w

-- Make a showable into Text
tshow :: Show a => a -> T.Text
tshow = T.pack . show
