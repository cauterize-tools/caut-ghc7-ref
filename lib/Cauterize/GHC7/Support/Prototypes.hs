{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Cauterize.GHC7.Support.Prototypes
  ( CautType(..)

  , CautSynonym(..)
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
  ) where

import Cauterize.GHC7.Support.Result
import CerealPlus.Deserialize
import CerealPlus.Serializable
import CerealPlus.Serialize
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Data.Word
import Data.Maybe
import Data.Bits
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified Data.Vector as V

class CautTranscodable a where
  cautName :: a -> T.Text
  cautSize :: a -> (MinSize, MaxSize)

  encode :: Serializable CautResult a => a -> Either CautError B.ByteString
  encode a = let CautResult r = runLazy $ serialize a
             in case runReaderT r [] of
                  Left e -> Left e
                  Right ((), b) -> Right b

  decode :: Serializable CautResult a => B.ByteString -> Either CautError (a, B.ByteString)
  decode b = let b' = B.toStrict b
                 CautResult r = runPartial deserialize b'
             in case runReaderT r [] of
                  Left e -> Left e
                  Right (Fail msg _) -> Left (CautError msg [])
                  Right (Partial _) -> Left (CautError "Ran out of bytes." [])
                  Right (Done a bs) -> Right (a, B.fromStrict bs)

class CautTranscodable a => CautType a where
  cautHash :: a -> Hash

-- The following are instances for each of the Cauterize prototypes
class CautTranscodable a => CautSynonym a where

class CautTranscodable a => CautRange a where
  rangePrim :: a -> GHC7Prim
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

data GHC7Prim = GHC7Word8
              | GHC7Word16
              | GHC7Word32
              | GHC7Word64
              | GHC7Int8
              | GHC7Int16
              | GHC7Int32
              | GHC7Int64
              | GHC7Bool
              | GHC7Float32
              | GHC7Float64
  deriving (Show, Eq)

genSynonymSerialize :: (CautSynonym a, Serializable CautResult b) => b -> a -> Serialize CautResult ()
genSynonymSerialize v t = withTrace (TSynonym $ cautName t) (serialize v)

genSynonymDeserialize :: (CautSynonym a, Serializable CautResult b) => a -> (b -> a) -> Deserialize CautResult a
genSynonymDeserialize t ctor = withTrace (TSynonym $ cautName t) (liftM ctor deserialize)

genRangeSerialize :: (CautRange a, Serializable CautResult b) => b -> a -> Serialize CautResult ()
genRangeSerialize v t = undefined -- withTrace (TRange $ cautName t) (serialize v)

genRangeDeserialize :: (CautRange a, Serializable CautResult b) => a -> (b -> a) -> Deserialize CautResult a
genRangeDeserialize t ctor = undefined -- withTrace (TRange $ cautName t) (liftM ctor deserialize)

genArraySerialize :: (CautArray a, Serializable CautResult b)
                  => V.Vector b -> a -> Serialize CautResult ()
genArraySerialize vs t = withTrace tArray $ if V.length vs /= aLength then fwt else V.mapM_ go (vsWithIx vs)
  where
    tArray = TArray $ cautName t
    aLength = fromIntegral $ arrayLength t
    fwt = failWithTrace $ T.concat ["Unexpected length: ", tshow (V.length vs), ". Expected ", tshow aLength, "."]
    go (ix, v) = withTrace (TArrayIndex ix) (serialize v)

genArrayDeserialize :: (CautArray a, Serializable CautResult b)
                    => a -> (V.Vector b -> a) -> Deserialize CautResult a
genArrayDeserialize t ctor = withTrace tArray $ liftM (ctor . V.fromList) (mapM go ixs)
  where
    tArray = TArray $ cautName t
    aLength = fromIntegral $ arrayLength t
    ixs = [0..aLength - 1]
    go ix = withTrace (TArrayIndex ix) deserialize

genVectorSerialize :: (CautVector a, Serializable CautResult b)
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
    go (ix, v) = withTrace (TVectorIndex ix) (serialize v)

genVectorDeserialize :: (CautVector a, Serializable CautResult b)
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
    go ix = withTrace (TArrayIndex ix) deserialize

genEnumerationSerialize :: (CautEnumeration a, Serializable CautResult b) => b -> a -> Serialize CautResult ()
genEnumerationSerialize v t = undefined -- withTrace (TEnumeration $ cautName t) (serialize v)

genEnumerationDeserialize :: (CautEnumeration a, Serializable CautResult b) => a -> (b -> a) -> Deserialize CautResult a
genEnumerationDeserialize t ctor = undefined -- withTrace (TEnumeration $ cautName t) (liftM ctor deserialize)

-- genFieldSerialize :: Serializable CautResult a => T.Text -> a -> Serialize CautResult ()
genFieldSerialize :: Serializable CautResult a => Trace -> a -> Serialize CautResult ()
genFieldSerialize t v = withTrace t (serialize v)

-- genFieldDeserialize :: Serializable CautResult a => T.Text -> Deserialize CautResult a
genFieldDeserialize :: Serializable CautResult a => Trace -> Deserialize CautResult a
genFieldDeserialize t = withTrace t deserialize

genCombFieldSerialize :: Serializable CautResult a => T.Text -> Maybe a -> Serialize CautResult ()
genCombFieldSerialize _ Nothing = return ()
genCombFieldSerialize t (Just f) = genFieldSerialize (TCombinationField t) f

genCombFieldDeserialize :: Serializable CautResult a => T.Text -> Bool -> Deserialize CautResult (Maybe a)
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

encodeCombTag :: CautCombination a => a -> [Bool] -> Serialize CautResult ()
encodeCombTag t flags = withTrace TCombinationTag $ tagEncode (fromIntegral $ combinationTagWidth t) (calcCombTag flags)

decodeCombTag :: CautCombination a => a -> Deserialize CautResult Word64
decodeCombTag t = withTrace TCombinationTag $ do
  flags <- tagDecode (fromIntegral $ combinationTagWidth t)
  if flags < 2^maxIxP1
    then return flags
    else failWithTrace $ T.concat ["Expected combination flags < (2^", tshow maxIxP1, "). Got: ", tshow flags, "."]
  where
    maxIxP1 = combinationMaxIndex t + 1

genUnionFieldSerialize :: (CautUnion a, Serializable CautResult b)
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

genUnionFieldDeserialize :: Serializable CautResult a
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
                              1 -> let i' = fromIntegral i :: Word8 in serialize i'
                              2 -> let i' = fromIntegral i :: Word16 in serialize i'
                              4 -> let i' = fromIntegral i :: Word32 in serialize i'
                              8 -> let i' = fromIntegral i :: Word64 in serialize i'
                              _ -> error $ "Invalid tag width: " ++ show w

tagDecode :: Int -> Deserialize CautResult Word64
tagDecode w = case w of
               1 -> liftM fromIntegral (deserialize :: Deserialize CautResult Word8)
               2 -> liftM fromIntegral (deserialize :: Deserialize CautResult Word16)
               4 -> liftM fromIntegral (deserialize :: Deserialize CautResult Word32)
               8 -> liftM fromIntegral (deserialize :: Deserialize CautResult Word64)
               _ -> error $ "Invalid tag width: " ++ show w

-- Make a showable into Text
tshow :: Show a => a -> T.Text
tshow = T.pack . show
