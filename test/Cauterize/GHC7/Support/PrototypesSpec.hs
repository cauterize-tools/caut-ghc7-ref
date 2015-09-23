{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, OverloadedStrings #-}
module Cauterize.GHC7.Support.PrototypesSpec
  ( spec
  ) where

import Cauterize.GHC7.Support.Prototypes
import Cauterize.GHC7.Support.Result
import qualified Data.Vector as V (fromList)
import qualified Data.ByteString.Lazy as B

import Test.Hspec

-- Interface for synonyms
data ABool = ABool Bool deriving (Show, Eq)
instance CautTranscodable ABool where; cautName = const "a_bool"; cautSize = const (1,1)
instance CautSynonym ABool where
instance Serializable CautResult ABool where
  serialize t@(ABool a) = genSynonymSerialize a t
  deserialize = genSynonymDeserialize (undefined :: ABool) ABool

-- Interface for arrays
data AnArray = AnArray (Vector Bool) deriving (Show, Eq)
instance CautTranscodable AnArray where; cautName = const "an_array"; cautSize = const (4,4)
instance CautArray AnArray where; arrayLength _ = 4
instance Serializable CautResult AnArray where
  serialize t@(AnArray vs) = genArraySerialize vs t
  deserialize = genArrayDeserialize (undefined :: AnArray) AnArray

-- Interface for vectors
data AVector = AVector (Vector Bool) deriving (Show, Eq)
instance CautTranscodable AVector where; cautName = const "a_vector"; cautSize = const (1,5)
instance CautVector AVector where; vectorMaxLength _ = 4; vectorTagWidth _ = 1
instance Serializable CautResult AVector where
  serialize t@(AVector vs) = genVectorSerialize vs t
  deserialize = genVectorDeserialize (undefined :: AVector) AVector

-- Interface for records
data ARecord
  = ARecord { aRecordFieldBool :: Bool
            , aRecordFieldU8 :: Word8
            } deriving (Show, Ord, Eq)
instance CautTranscodable ARecord where; cautName = const "a_record"; cautSize = const (2,2)
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
  = ACombination { aCombinationFieldBool :: Maybe Bool
                 , aCombinationFieldU8 :: Maybe Word8
                 } deriving (Show, Ord, Eq)
instance CautTranscodable ACombination where; cautName _ = "a_combination"; cautSize = const (1,3)
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
data AUnion = AUnionCBool Bool
            | AUnionU8 Word8
            | AUnionEmpty
  deriving (Show, Eq)
instance CautTranscodable AUnion where; cautName _ = "a_union"; cautSize = const (1,2)
instance CautUnion AUnion where; unionTagWidth _ = 1
instance Serializable CautResult AUnion where
  serialize r = traceUnion $
    case r of
      AUnionCBool v -> genUnionFieldSerialize r 0 "field_bool" v
      AUnionU8 v -> genUnionFieldSerialize r 1 "field_u8" v
      AUnionEmpty -> genUnionFieldSerializeEmpty r 2
    where
      traceUnion = withTrace (TUnion $ cautName r)
  deserialize = traceUnion $ do
    tag <- decodeUnionTag u
    case tag of
      0 -> genUnionFieldDeserialize "field_bool" AUnionCBool
      1 -> genUnionFieldDeserialize "field_u8" AUnionU8
      2 -> return AUnionEmpty
      v -> failUnionTag v
    where
      u = undefined :: AUnion
      traceUnion = withTrace (TUnion $ cautName u)

spec :: Spec
spec = describe "Prototypes" $ do
  context "works over arrays" $ do
    it "can encode" $ do
      let a = encode (AnArray (V.fromList [False, True, False, True]))
      a `shouldBe` Right (B.pack [0,1,0,1])

    it "can decode" $ do
      let b = decode $ B.pack [3,0,0,0]
      Right (AVector $ V.fromList [False, False, False], B.empty) `shouldBe` b

    it "can detect incorrect lengths" $ do
      let e = encode (AnArray $ V.fromList [False, False])
      e `shouldBe` Left (CautError "Unexpected length: 2. Expected 4." [TArray "an_array"])

  context "works over vectors" $ do
    it "can encode" $ do
      let v0 = encode (AVector (V.fromList [True]))
      v0 `shouldBe` Right (B.pack [1,1])

      let v1 = encode (AVector (V.fromList [False, True]))
      v1 `shouldBe` Right (B.pack [2,0,1])

    it "can decode" $ do
      let b = decode $ B.pack [3,0,0,0]
      b `shouldBe` Right (AVector $ V.fromList [False, False, False], B.empty)

    it "can detect incorrect lengths" $ do
      let e = encode (AVector $ V.fromList [False, False, False, False, False])
      e `shouldBe` Left (CautError "Unexpected length: 5. Expected <= 4." [TVector "a_vector"])

  context "works over records" $ do
    it "can encode" $ do
      let r = encode (ARecord True 10)
      r `shouldBe` Right (B.pack [1,10])

    it "can decode" $ do
      let b = decode $ B.pack [1, 20]
      b `shouldBe` Right (ARecord True 20, B.empty)

  context "works over combinations" $ do
    it "can encode" $ do
      let c = encode (ACombination (Just False) (Just 5))
      c `shouldBe` Right (B.pack [3,0,5])

    it "can decode" $ do
      let b0 = decode $ B.pack [0]
      b0 `shouldBe` Right (ACombination Nothing Nothing, B.empty)

      let b1 = decode $ B.pack [2, 12]
      b1 `shouldBe` Right (ACombination Nothing (Just 12), B.empty)

    it "can detect incorrect bit flags" $ do
      let e = decode $ B.pack [7, 0, 0] :: Either CautError (ACombination, B.ByteString)
      e `shouldBe` Left (CautError "Expected combination flags < (2^2). Got: 7." [TCombinationTag,TCombination "a_combination"])

  context "works over unions" $ do
    it "can encode" $ do
      let u = encode (AUnionU8 45)
      u `shouldBe` Right (B.pack [1,45])

    it "can encode empty alternatives" $ do
      let u = encode AUnionEmpty
      u `shouldBe` Right (B.pack [2])

    it "can decode" $ do
      let b = decode $ B.pack [0, 1]
      b `shouldBe` Right (AUnionCBool True, B.empty)

    it "can decode empty alternatives" $ do
      let b = decode $ B.pack [2]
      b `shouldBe` Right (AUnionEmpty, B.empty)

    it "can detect incorrect tags" $ do
      let e = decode $ B.pack [7, 0, 0] :: Either CautError (AUnion, B.ByteString)
      e `shouldBe` Left (CautError "Unexpected tag: 7" [TUnion "a_union"])
