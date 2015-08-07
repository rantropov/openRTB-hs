{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module OpenRTB.Types.Enum.VideoLinearitySpec where

import Control.Applicative
import Data.Aeson
import Data.Aeson.TH
import Test.Hspec
import Test.QuickCheck

import Test.Instances
import OpenRTB.Types.Enum.VideoLinearity

data Mock = Mock { vl :: VideoLinearity } deriving (Eq, Show)
$(deriveJSON defaultOptions ''Mock)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "VideoLinearity" $ do
  context "JSON" $ do
    it "should convert back and forth" $ property $ do
      \m -> (decode . encode) m == Just (m :: Mock)

    context "ToJSON" $ do
      it "should properly encode a value" $ do
        encode (Mock Linear) `shouldBe` "{\"vl\":1}"
        encode (Mock NonLinear) `shouldBe` "{\"vl\":2}"

    context "FromJSON" $ do
      it "should properly decode a value" $ do
        decode "{\"vl\":1}" `shouldBe` (Just (Mock Linear))
        decode "{\"vl\":2}" `shouldBe` (Just (Mock NonLinear))

      it "should fail when out of range" $ do
        decode "{\"vl\":0}" `shouldBe` (Nothing :: Maybe Mock)
        decode "{\"vl\":3}" `shouldBe` (Nothing :: Maybe Mock)


instance Arbitrary Mock where
  arbitrary = Mock <$> arbitrary
