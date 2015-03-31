{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, OverloadedStrings #-}
module Cauterize.GHC7.Support.PrototypesSpec
  ( spec
  ) where

import Cauterize.GHC7.Support.BuiltIn
import Cauterize.GHC7.Support.Prototypes
import Cauterize.GHC7.Support.Result
import CerealPlus.Serializable
import qualified Data.Vector as V (fromList)
import qualified Data.ByteString.Lazy as B

import Test.Hspec

-- Interface for synonyms
data ABool = ABool CBool deriving (Show, Eq)
instance CautType ABool where; cautName _ = "a_bool"
instance CautSynonym ABool where
instance Serializable CautResult ABool where
  serialize t@(ABool a) = genSynonymSerialize a t
  deserialize = genSynonymDeserialize (undefined :: ABool) ABool

-- Interface for arrays
data AnArray = AnArray (Vector CBool) deriving (Show, Eq)
instance CautType AnArray where; cautName _ = "an_array"
instance CautArray AnArray where; arrayLength _ = 4
instance Serializable CautResult AnArray where
  serialize t@(AnArray vs) = genArraySerialize vs t
  deserialize = genArrayDeserialize (undefined :: AnArray) AnArray

-- Interface for vectors
data AVector = AVector (Vector CBool) deriving (Show, Eq)
instance CautType AVector where; cautName _ = "a_vector"
instance CautVector AVector where; vectorMaxLength _ = 4; vectorTagWidth _ = 1
instance Serializable CautResult AVector where
  serialize t@(AVector vs) = genVectorSerialize vs t
  deserialize = genVectorDeserialize (undefined :: AVector) AVector

-- Interface for records
data ARecord
  = ARecord { aRecordFieldBool :: CBool
            , aRecordFieldU8 :: U8
            } deriving (Show, Ord, Eq)
instance CautType ARecord where; cautName _ = "a_record"
instance CautRecord ARecord where
instance Serializable CautResult ARecord where
  serialize r = traceRecord $ do
    genFieldSerialize (TRecordField "field_bool") (aRecordFieldBool r)
    genFieldSerialize (TRecordField "field_u8") (aRecordFieldU8 r)
    where
      traceRecord = withTrace (TRecord $ cautName r)
  deserialize = traceRecord $ do
    a0 <- genFieldDeserialize (TRecordField "field_bool")
    a1 <- genFieldDeserialize (TRecordField "field_u8")
    return $ ARecord a0 a1
    where
      u = undefined :: ARecord
      traceRecord = withTrace (TRecord $ cautName u)

-- Interface for combinations
data ACombination
  = ACombination { aCombinationFieldBool :: Maybe CBool
                 , aCombinationFieldU8 :: Maybe U8
                 } deriving (Show, Ord, Eq)
instance CautType ACombination where; cautName _ = "a_combination"
instance CautCombination ACombination where; combinationTagWidth _ = 1; combinationMaxIndex _ = 1
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
    flags <- decodeCombTag u
    a0 <- genCombFieldDeserialize "field_bool" (flags `isFlagSet` 0)
    a1 <- genCombFieldDeserialize "field_u8" (flags `isFlagSet` 1)
    return $ ACombination a0 a1
    where
      u = undefined :: ACombination
      traceComb = withTrace (TCombination $ cautName u)

-- Interface for unions
data AUnion = AUnionCBool CBool
            | AUnionU8 U8
  deriving (Show, Eq)
instance CautType AUnion where; cautName _ = "a_union"
instance CautUnion AUnion where; unionTagWidth _ = 1
instance Serializable CautResult AUnion where
  serialize r = traceUnion $
    case r of
      AUnionCBool v -> genUnionFieldSerialize r 0 "field_bool" v
      AUnionU8 v -> genUnionFieldSerialize r 1 "field_u8" v
    where
      traceUnion = withTrace (TUnion $ cautName r)
  deserialize = traceUnion $ do
    tag <- decodeUnionTag u
    case tag of
      0 -> genUnionFieldDeserialize "field_bool" AUnionCBool
      1 -> genUnionFieldDeserialize "field_u8" AUnionU8
      v -> failUnionTag v
    where
      u = undefined :: AUnion
      traceUnion = withTrace (TUnion $ cautName u)

spec :: Spec
spec = describe "Prototypes" $ do
  context "works over arrays" $ do
    it "can encode" $ do
      let a = encode (AnArray (V.fromList [CBool False,CBool True,CBool False,CBool True]))
      Right (B.pack [0,1,0,1]) `shouldBe` a

    it "can decode" $ do
      let b = decode $ B.pack [3,0,0,0]
      Right (AVector $ V.fromList [CBool False, CBool False, CBool False], B.empty) `shouldBe` b

    it "can detect incorrect lengths" $ do
      let e = encode (AnArray $ V.fromList [CBool False, CBool False])
      Left (CautError "Unexpected length: 2. Expected 4." [TArray "an_array"]) `shouldBe` e

  context "works over vectors" $ do
    it "can encode" $ do
      let v0 = encode (AVector (V.fromList [CBool True]))
      Right (B.pack [1,1]) `shouldBe` v0

      let v1 = encode (AVector (V.fromList [CBool False, CBool True]))
      Right (B.pack [2,0,1]) `shouldBe` v1

    it "can decode" $ do
      let b = decode $ B.pack [3,0,0,0]
      Right (AVector $ V.fromList [CBool False, CBool False, CBool False], B.empty) `shouldBe` b

    it "can detect incorrect lengths" $ do
      let e = encode (AVector $ V.fromList [CBool False, CBool False, CBool False, CBool False, CBool False])
      Left (CautError "Unexpected length: 5. Expected <= 4." [TVector "a_vector"]) `shouldBe` e

  context "works over records" $ do
    it "can encode" $ do
      let r = encode (ARecord (CBool True) (U8 10))
      Right (B.pack [1,10]) `shouldBe` r

    it "can decode" $ do
      let b = decode $ B.pack [1, 20]
      Right (ARecord (CBool True) (U8 20), B.empty) `shouldBe` b

  context "works over combinations" $ do
    it "can encode" $ do
      let c = encode (ACombination (Just $ CBool False) (Just $ U8 5))
      Right (B.pack [3,0,5]) `shouldBe` c

    it "can decode" $ do
      let b0 = decode $ B.pack [0]
      Right (ACombination Nothing Nothing, B.empty) `shouldBe` b0

      let b1 = decode $ B.pack [2, 12]
      Right (ACombination Nothing (Just $ U8 12), B.empty) `shouldBe` b1

    it "can detect incorrect bit flags" $ do
      let e = decode $ B.pack [7, 0, 0] :: Either CautError (ACombination, B.ByteString)
      Left (CautError "Expected combination flags < (2^2). Got: 7." [TCombinationTag,TCombination "a_combination"]) `shouldBe` e

  context "works over unions" $ do
    it "can encode" $ do
      let u = encode (AUnionU8 (U8 45))
      Right (B.pack [1,45]) `shouldBe` u

    it "can decode" $ do
      let b = decode $ B.pack [0, 1]
      Right (AUnionCBool (CBool True), B.empty) `shouldBe` b

    it "can detect incorrect tags" $ do
      let e = decode $ B.pack [7, 0, 0] :: Either CautError (AUnion, B.ByteString)
      Left (CautError "Unexpected tag: 7" [TUnion "a_union"]) `shouldBe` e
