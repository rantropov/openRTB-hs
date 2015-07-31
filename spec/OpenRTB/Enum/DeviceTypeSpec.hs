{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module OpenRTB.Enum.DeviceTypeSpec where

import Control.Applicative
import Data.Aeson
import Data.Aeson.TH
import Test.Hspec
import Test.QuickCheck
import OpenRTB.Enum.DeviceType

data Mock = Mock { dt :: DeviceType } deriving (Eq, Show)
$(deriveJSON defaultOptions ''Mock)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "DeviceType" $ do
  context "JSON" $ do
    it "should convert back and forth" $ property $ do
      \m -> (decode . encode) m == Just (m :: Mock)

    context "ToJSON" $ do
      it "should properly encode a value" $ do
        encode (Mock MobileTablet) `shouldBe` "{\"dt\":1}"
        encode (Mock Tablet) `shouldBe` "{\"dt\":5}"

    context "FromJSON" $ do
      it "should properly decode a value" $ do
        decode "{\"dt\":3}" `shouldBe` (Just (Mock ConnectedTV))
        decode "{\"dt\":7}" `shouldBe` (Just (Mock SetTopBox))

      it "should fail when out of range" $ do
        decode "{\"dt\":0}" `shouldBe` (Nothing :: Maybe Mock)
        decode "{\"dt\":8}" `shouldBe` (Nothing :: Maybe Mock)


instance Arbitrary Mock where
  arbitrary = Mock <$> arbitrary

instance Arbitrary DeviceType where
  arbitrary = toEnum <$> choose (1, 7)
