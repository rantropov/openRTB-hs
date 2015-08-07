{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module OpenRTB.Types.Enum.LocationTypeSpec where

import Control.Applicative
import Data.Aeson
import Data.Aeson.TH
import Test.Hspec
import Test.QuickCheck

import Test.Instances
import OpenRTB.Types.Enum.LocationType

data Mock = Mock { lt :: LocationType } deriving (Eq, Show)
$(deriveJSON defaultOptions ''Mock)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "LocationType" $ do
  context "JSON" $ do
    it "should convert back and forth" $ property $ do
      \m -> (decode . encode) m == Just (m :: Mock)

    context "ToJSON" $ do
      it "should properly encode a value" $ do
        encode (Mock GPS) `shouldBe` "{\"lt\":1}"
        encode (Mock UserProvided) `shouldBe` "{\"lt\":3}"

    context "FromJSON" $ do
      it "should properly decode a value" $ do
        decode "{\"lt\":3}" `shouldBe` (Just (Mock UserProvided))
        decode "{\"lt\":2}" `shouldBe` (Just (Mock IP))

      it "should fail when out of range" $ do
        decode "{\"lt\":0}" `shouldBe` (Nothing :: Maybe Mock)
        decode "{\"lt\":4}" `shouldBe` (Nothing :: Maybe Mock)


instance Arbitrary Mock where
  arbitrary = Mock <$> arbitrary
