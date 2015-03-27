{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Cauterize.GHC7.Support.Result
  ( Trace(..)
  , CautResult(..)
  , CautError(..)

  , failWithTrace
  , withTrace
  ) where

import Control.Applicative
import Control.Monad.Morph
import Control.Monad.Trans.Reader
import qualified Data.Text as T

data CautError
  = CautError { errorMsg :: T.Text    -- Description of the error.
              , errorTrace :: [Trace] -- Trace to the error. Head of the list is most recent trace.
              } deriving (Show)

-- Identifies the path through the cauterize structure.
data Trace = TBuiltIn T.Text
           | TSynonym T.Text

           | TArray T.Text
           | TArrayIndex Int

           | TVector T.Text
           | TVectorTag
           | TVectorIndex Int
  deriving (Show)

-- Insert information into the trace stack and fail.
failWithTrace :: (MonadTrans t, Monad (t CautResult))
              => T.Text -> t CautResult a
failWithTrace msg = do
  currentTrace <- lift $ CautResult ask
  lift $ CautResult $ lift $ Left $ CautError msg currentTrace

-- Insert new information into the trace stack.
withTrace :: (MonadTrans t, Monad (t CautResult), MFunctor t)
          => Trace -> t CautResult a -> t CautResult a
withTrace trace = hoist (CautResult . local (trace:) . unCautResult)

newtype CautResult a =
  CautResult {
    unCautResult :: ReaderT [Trace] (Either CautError) a
  } deriving (Functor, Applicative, Monad)
