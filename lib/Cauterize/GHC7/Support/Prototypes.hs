{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Cauterize.GHC7.Support.Prototypes
  ( CautType(..)

  , CautSynonym
  , CautArray(..)
  , CautVector(..)
  , CautResult(..)
  , CautRecord
  , CautCombination
  , CautUnion

  , genArraySerialize
  , genArrayDeserialize
  ) where

import Cauterize.GHC7.Support.Result

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import qualified Data.Vector as V

import CerealPlus.Serializable
import Control.Monad
import CerealPlus.Serializable
import CerealPlus.Serialize
import CerealPlus.Deserialize
import Control.Monad.Trans.Reader

class CautType a where
  cautName :: a -> T.Text
  -- cautHash :: a -> Hash
  -- cautSize :: a -> (MinSize, MaxSize)

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

class CautType a => CautSynonym a where

class CautType a => CautArray a where
  arrayLength :: a -> Int

class CautType a => CautVector a where
  vectorMaxLength :: a -> Int

class CautType a => CautRecord a where

class CautType a => CautCombination a where

class CautType a => CautUnion a where

genArraySerialize :: (CautArray a, Serializable CautResult b)
                  => V.Vector b -> a -> CerealPlus.Serialize.Serialize CautResult ()
genArraySerialize vs t = withTrace tArray $ if V.length vs /= aLength then fwt else V.mapM_ go vsWithIx
  where
    tArray = TArray $ cautName t
    aLength = arrayLength t
    fwt = failWithTrace $ T.concat ["Unexpected length: ", tshow (V.length vs), ". Expected ", tshow aLength, "."]
    vsWithIx = V.zip (V.fromList [0..(V.length vs - 1)]) vs
    go (ix, v) = withTrace (TArrayIndex ix) (serialize v)

genArrayDeserialize :: (CautArray a, Serializable CautResult b)
                    => a -> (V.Vector b -> a) -> Deserialize CautResult a
genArrayDeserialize t ctor = withTrace tArray $ liftM (ctor . V.fromList) (mapM go ixs)
  where
    tArray = TArray $ cautName t
    aLength = arrayLength t
    ixs = [0..aLength - 1]
    go ix = withTrace (TArrayIndex ix) deserialize

-- Make a showable into Text
tshow :: Show a => a -> T.Text
tshow = T.pack . show
