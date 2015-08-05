{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module OpenRTB.Enum.ConnectionTypeSpec where

import Control.Applicative
import Data.Aeson
import Data.Aeson.TH
import Test.Hspec
import Test.QuickCheck

import Test.Instances
import OpenRTB.Enum.ConnectionType

data Mock = Mock { ct :: ConnectionType } deriving (Eq, Show)
$(deriveJSON defaultOptions ''Mock)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "ConnectionType" $ do
  context "JSON" $ do
    it "should convert back and forth" $ property $ do
       \m -> (decode . encode) m == Just (m :: Mock)

    context "ToJSON" $ do
      it "should properly encode a value" $ do
        encode (Mock Unknown) `shouldBe` "{\"ct\":0}"
        encode (Mock CNUnknown) `shouldBe` "{\"ct\":3}"

    context "FromJSON" $ do
      it "should properly decode a value" $ do
        decode "{\"ct\":4}" `shouldBe` (Just (Mock CN2G))
        decode "{\"ct\":6}" `shouldBe` (Just (Mock CN4G))

      it "should fail when out of range" $ do
        decode "{\"ct\":-1}" `shouldBe` (Nothing :: Maybe Mock)
        decode "{\"ct\":7}" `shouldBe` (Nothing :: Maybe Mock)


instance Arbitrary Mock where
  arbitrary = Mock <$> arbitrary
