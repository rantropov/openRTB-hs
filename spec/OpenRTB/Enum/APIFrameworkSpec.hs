{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module OpenRTB.Enum.APIFrameworkSpec where

import Control.Applicative
import Data.Aeson
import Data.Aeson.TH
import Test.Hspec
import Test.QuickCheck

import Test.Instances
import OpenRTB.Enum.APIFramework

data Mock = Mock { api :: APIFramework } deriving (Eq, Show)
$(deriveJSON defaultOptions ''Mock)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "APIFramework" $ do
  context "JSON" $ do
    it "should convert back and forth" $ property $ do
      \m -> (decode . encode) m == Just (m :: Mock)

    context "ToJSON" $ do
      it "should properly encode a value" $ do
        encode (Mock VPAID20) `shouldBe` "{\"api\":2}"
        encode (Mock MRAID1) `shouldBe` "{\"api\":3}"

    context "FromJSON" $ do
      it "should properly decode a value" $ do
        decode "{\"api\":1}" `shouldBe` (Just (Mock VPAID10))
        decode "{\"api\":5}" `shouldBe` (Just (Mock MRAID2))

      it "should fail when out of range" $ do
        decode "{\"api\":0}" `shouldBe` (Nothing :: Maybe Mock)
        decode "{\"api\":6}" `shouldBe` (Nothing :: Maybe Mock)


instance Arbitrary Mock where
  arbitrary = Mock <$> arbitrary
