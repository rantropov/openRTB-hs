{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module OpenRTB.Enum.BannerAdTypeSpec where

import Control.Applicative
import Data.Aeson
import Data.Aeson.TH
import Test.Hspec
import Test.QuickCheck

import Test.Instances
import OpenRTB.Enum.BannerAdType

data Mock = Mock { bat :: BannerAdType } deriving (Eq, Show)
$(deriveJSON defaultOptions ''Mock)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "BannerAdTypes" $ do
  context "JSON" $ do
    it "should convert back and forth" $ property $ do
      \m -> (decode . encode) m == Just (m :: Mock)

    context "ToJSON" $ do
      it "should properly encode a value" $ do
        encode (Mock XHTMLTextAd) `shouldBe` "{\"bat\":1}"
        encode (Mock IFrame) `shouldBe` "{\"bat\":4}"

    context "FromJSON" $ do
      it "should properly decode a value" $ do
        decode "{\"bat\":3}" `shouldBe` (Just (Mock JavaScriptAd))
        decode "{\"bat\":2}" `shouldBe` (Just (Mock XHTMLBannerAd))

      it "should fail when out of range" $ do
        decode "{\"bat\":0}" `shouldBe` (Nothing :: Maybe Mock)
        decode "{\"bat\":17}" `shouldBe` (Nothing :: Maybe Mock)


instance Arbitrary Mock where
  arbitrary = Mock <$> arbitrary
