{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module OpenRTB.Enum.NoBidReasonCodeSpec where

import Control.Applicative
import Data.Aeson
import Data.Aeson.TH
import Test.Hspec
import Test.QuickCheck

import Test.Instances
import OpenRTB.Enum.NoBidReasonCode

data Mock = Mock { nbrc :: NoBidReasonCode } deriving (Eq, Show)
$(deriveJSON defaultOptions ''Mock)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "NoBidReasonCode" $ do
  context "JSON" $ do
    it "should convert back and forth" $ property $ do
       \m -> (decode . encode) m == Just (m :: Mock)

    context "ToJSON" $ do
      it "should properly encode a value" $ do
        encode (Mock Unknown) `shouldBe` "{\"nbrc\":0}"
        encode (Mock Blocked) `shouldBe` "{\"nbrc\":7}"

    context "FromJSON" $ do
      it "should properly decode a value" $ do
        decode "{\"nbrc\":1}" `shouldBe` (Just (Mock Technical))
        decode "{\"nbrc\":3}" `shouldBe` (Just (Mock Spider))

      it "should fail when out of range" $ do
        decode "{\"nbrc\":-1}" `shouldBe` (Nothing :: Maybe Mock)
        decode "{\"nbrc\":9}" `shouldBe` (Nothing :: Maybe Mock)


instance Arbitrary Mock where
  arbitrary = Mock <$> arbitrary
