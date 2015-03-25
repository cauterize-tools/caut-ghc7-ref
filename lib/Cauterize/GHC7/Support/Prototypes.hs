module Cauterize.GHC7.Support.Prototypes where

class CautType a where
  encode :: a -> Serialize (Either [Trace]) B.ByteString
  decode :: B.ByteString -> Deserialize (Either [Trace]) a
  typeName :: a -> T.Text
  typeHash :: a -> TypeHash
