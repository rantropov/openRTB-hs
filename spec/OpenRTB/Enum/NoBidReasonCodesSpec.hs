{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module OpenRTB.Enum.NoBidReasonCodesSpec where

import Control.Applicative
import Data.Aeson
import Data.Aeson.TH
import Test.Hspec
import Test.QuickCheck
import OpenRTB.Enum.NoBidReasonCodes

data Mock = Mock { nbrc :: NoBidReasonCode } deriving (Eq, Show)
$(deriveJSON defaultOptions ''Mock)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "NoBidReasonCodes" $ do
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


instance Arbitrary Mock where
  arbitrary = Mock <$> arbitrary

instance Arbitrary NoBidReasonCode where
  arbitrary = toEnum <$> choose (0, (length [Unknown ..]) - 1)
