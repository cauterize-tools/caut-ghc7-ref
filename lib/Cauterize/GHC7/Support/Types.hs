module Cauterize.GHC7.Support.Types
  ( Trace(..)
  , TypeHash
  , mkTypeHash

  , CautResult

  , failWithTrace
  ) where

import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Data.Word
import qualified Data.Text as T

data Trace = Trace T.Text
  deriving (Show)

data TypeHash = TypeHash
  deriving (Show)

type CautResult = ReaderT [Trace] (Either [Trace])

mkTypeHash :: [Word8] -> TypeHash
mkTypeHash _ = TypeHash

failWithTrace :: (MonadTrans t, Monad (t (ReaderT [Trace] (Either [Trace])))) =>
                 T.Text -> t (ReaderT [Trace] (Either [Trace])) b
failWithTrace t = do
  current <- lift ask
  lift $ lift $ Left $ Trace t : current
