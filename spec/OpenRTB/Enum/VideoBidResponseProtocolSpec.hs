{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module OpenRTB.Enum.VideoBidResponseProtocolSpec where

import Control.Applicative
import Data.Aeson
import Data.Aeson.TH
import Test.Hspec
import Test.QuickCheck
import OpenRTB.Enum.VideoBidResponseProtocol

data Mock = Mock { vbrp :: VideoBidResponseProtocol } deriving (Eq, Show)
$(deriveJSON defaultOptions ''Mock)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "VideoBidResponseProtocol" $ do
  context "JSON" $ do
    it "should convert back and forth" $ property $ do
      \m -> (decode . encode) m == Just (m :: Mock)

    context "ToJSON" $ do
      it "should properly encode a value" $ do
        encode (Mock Vast10) `shouldBe` "{\"vbrp\":1}"
        encode (Mock Vast30Wrapper) `shouldBe` "{\"vbrp\":6}"

    context "FromJSON" $ do
      it "should properly decode a value" $ do
        decode "{\"vbrp\":3}" `shouldBe` (Just (Mock Vast30))
        decode "{\"vbrp\":4}" `shouldBe` (Just (Mock Vast10Wrapper))

      it "should fail when out of range" $ do
        decode "{\"vbrp\":0}" `shouldBe` (Nothing :: Maybe Mock)
        decode "{\"vbrp\":7}" `shouldBe` (Nothing :: Maybe Mock)


instance Arbitrary Mock where
  arbitrary = Mock <$> arbitrary

instance Arbitrary VideoBidResponseProtocol where
  arbitrary = toEnum <$> choose (1, 6)
