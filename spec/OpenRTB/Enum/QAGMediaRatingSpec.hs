{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module OpenRTB.Enum.QAGMediaRatingSpec where

import Control.Applicative
import Data.Aeson
import Data.Aeson.TH
import Test.Hspec
import Test.QuickCheck
import OpenRTB.Enum.QAGMediaRating

data Mock = Mock { qag :: QAGMediaRating } deriving (Eq, Show)
$(deriveJSON defaultOptions ''Mock)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "QAGMediaRating" $ do
  context "JSON" $ do
    it "should convert back and forth" $ property $ do
      \m -> (decode . encode) m == Just (m :: Mock)

    context "ToJSON" $ do
      it "should properly encode a value" $ do
        encode (Mock AllAudiences) `shouldBe` "{\"qag\":1}"
        encode (Mock MatureAudiences) `shouldBe` "{\"qag\":3}"

    context "FromJSON" $ do
      it "should properly decode a value" $ do
        decode "{\"qag\":3}" `shouldBe` (Just (Mock MatureAudiences))
        decode "{\"qag\":2}" `shouldBe` (Just (Mock EveryoneOver12))

      it "should fail when out of range" $ do
        decode "{\"qag\":0}" `shouldBe` (Nothing :: Maybe Mock)
        decode "{\"qag\":4}" `shouldBe` (Nothing :: Maybe Mock)


instance Arbitrary Mock where
  arbitrary = Mock <$> arbitrary

instance Arbitrary QAGMediaRating where
  arbitrary = toEnum <$> choose (1, 3)
