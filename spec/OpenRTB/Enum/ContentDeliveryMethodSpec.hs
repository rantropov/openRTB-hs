{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module OpenRTB.Enum.ContentDeliveryMethodSpec where

import Control.Applicative
import Data.Aeson
import Data.Aeson.TH
import Test.Hspec
import Test.QuickCheck

import Test.Instances
import OpenRTB.Enum.ContentDeliveryMethod

data Mock = Mock { cdm :: ContentDeliveryMethod } deriving (Eq, Show)
$(deriveJSON defaultOptions ''Mock)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "ContentDeliveryMethod" $ do
  context "JSON" $ do
    it "should convert back and forth" $ property $ do
      \m -> (decode . encode) m == Just (m :: Mock)

    context "ToJSON" $ do
      it "should properly encode a value" $ do
        encode (Mock Streaming) `shouldBe` "{\"cdm\":1}"
        encode (Mock Progressive) `shouldBe` "{\"cdm\":2}"

    context "FromJSON" $ do
      it "should properly decode a value" $ do
        decode "{\"cdm\":1}" `shouldBe` (Just (Mock Streaming))
        decode "{\"cdm\":2}" `shouldBe` (Just (Mock Progressive))

      it "should fail when out of range" $ do
        decode "{\"cdm\":0}" `shouldBe` (Nothing :: Maybe Mock)
        decode "{\"cdm\":3}" `shouldBe` (Nothing :: Maybe Mock)


instance Arbitrary Mock where
  arbitrary = Mock <$> arbitrary
