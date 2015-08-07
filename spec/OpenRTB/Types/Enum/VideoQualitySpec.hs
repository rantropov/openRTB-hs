{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module OpenRTB.Types.Enum.VideoQualitySpec where

import Control.Applicative
import Data.Aeson
import Data.Aeson.TH
import Test.Hspec
import Test.QuickCheck

import Test.Instances
import OpenRTB.Types.Enum.VideoQuality

data Mock = Mock { vct :: VideoQuality } deriving (Eq, Show)
$(deriveJSON defaultOptions ''Mock)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "VideoQuality" $ do
  context "JSON" $ do
    it "should convert back and forth" $ property $ do
       \m -> (decode . encode) m == Just (m :: Mock)

    context "ToJSON" $ do
      it "should properly encode a value" $ do
        encode (Mock ProfessionallyProduced) `shouldBe` "{\"vct\":1}"
        encode (Mock Prosumer) `shouldBe` "{\"vct\":2}"

    context "FromJSON" $ do
      it "should properly decode a value" $ do
        decode "{\"vct\":0}" `shouldBe` (Just (Mock Unknown))
        decode "{\"vct\":3}" `shouldBe` (Just (Mock UserGenerated))

      it "should fail when out of range" $ do
        decode "{\"vct\":-1}" `shouldBe` (Nothing :: Maybe Mock)
        decode "{\"vct\":4}" `shouldBe` (Nothing :: Maybe Mock)


instance Arbitrary Mock where
  arbitrary = Mock <$> arbitrary