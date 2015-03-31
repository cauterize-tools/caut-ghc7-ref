module Cauterize.GHC7.Support.BuiltInSpec
  ( spec
  ) where

import Test.Hspec

import Cauterize.GHC7.Support.BuiltIn
import Cauterize.GHC7.Support.Result
import Cauterize.GHC7.Support.Prototypes
import CerealPlus.Serializable

import qualified Data.ByteString.Lazy as B

spec :: Spec
spec = describe "BuiltIns" $ do
  context "works with unsigneds" $ do
    it "can encode u8" $ do
      let a = encode (U8 20)
      a `shouldBe` Right (B.pack [20])
    it "can encode u16" $ do
      let a = encode (U16 20)
      a `shouldBe` Right (B.pack [20,0])
    it "can encode u32" $ do
      let a = encode (U32 20)
      a `shouldBe` Right (B.pack [20,0,0,0])
    it "can encode u64" $ do
      let a = encode (U64 20)
      a `shouldBe` Right (B.pack [20,0,0,0,0,0,0,0])

  context "works with signeds" $ do
    it "can encode u8" $ do
      let a = encode (S8 (-2))
      a `shouldBe` Right (B.pack [254])
    it "can encode u16" $ do
      let a = encode (S16 (-2))
      a `shouldBe` Right (B.pack [254,255])
    it "can encode u32" $ do
      let a = encode (S32 (-2))
      a `shouldBe` Right (B.pack [254,255,255,255])
    it "can encode u64" $ do
      let a = encode (S64 (-2))
      a `shouldBe` Right (B.pack [254,255,255,255,255,255,255,255])

  context "works with bool" $ do
    it "can encode True" $ do
      let a = encode (CBool True)
      a `shouldBe` Right (B.pack [1])
    it "can encode False" $ do
      let a = encode (CBool False)
      a `shouldBe` Right (B.pack [0])

  context "works with floats" $ do
    it "can encode IEEE754S/F32" $ do
      let a = encode (F32 (0.5))
      a `shouldBe` Right (B.pack [0,0,0,63])

    it "can encode IEEE754D/F64" $ do
      let a = encode (F64 (0.5))
      a `shouldBe` Right (B.pack [0,0,0,0,0,0,224,63])
