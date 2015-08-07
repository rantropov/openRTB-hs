{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module OpenRTB.Types.Enum.VideoStartDelaySpec where

import Control.Applicative
import Data.Aeson
import Data.Aeson.TH
import Test.Hspec
import Test.QuickCheck

import Test.Instances
import OpenRTB.Types.Enum.VideoStartDelay

data Mock = Mock { vst :: VideoStartDelay } deriving (Eq, Show)
$(deriveJSON defaultOptions ''Mock)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "VideoStartDelay" $ do
  context "JSON" $ do
    it "should convert back and forth" $ property $ do
       \m -> (decode . encode) m == Just (m :: Mock)

    context "ToJSON" $ do
      it "should properly encode a value" $ do
        encode (Mock (MidRoll 1)) `shouldBe` "{\"vst\":1}"
        encode (Mock GenericMidRoll) `shouldBe` "{\"vst\":-1}"

    context "FromJSON" $ do
      it "should properly decode a value" $ do
        decode "{\"vst\":60}" `shouldBe` (Just (Mock (MidRoll 60)))
        decode "{\"vst\":0}" `shouldBe` (Just (Mock PreRoll))

      it "should fail when out of range" $ do
        decode "{\"vst\":-3}" `shouldBe` (Nothing :: Maybe Mock)


instance Arbitrary Mock where
  arbitrary = Mock <$> arbitrary
