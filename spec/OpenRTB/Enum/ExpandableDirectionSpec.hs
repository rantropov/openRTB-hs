{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module OpenRTB.Enum.ExpandableDirectionSpec where

import Control.Applicative
import Data.Aeson
import Data.Aeson.TH
import Test.Hspec
import Test.QuickCheck
import OpenRTB.Enum.ExpandableDirection as ED

data Mock = Mock { ed :: ExpandableDirection } deriving (Eq, Show)
$(deriveJSON defaultOptions ''Mock)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "ExpandableDirection" $ do
  context "JSON" $ do
    it "should convert back and forth" $ property $ do
      \m -> (decode . encode) m == Just (m :: Mock)

    context "ToJSON" $ do
      it "should properly encode a value" $ do
        encode (Mock ED.Left) `shouldBe` "{\"ed\":1}"
        encode (Mock FullScreen) `shouldBe` "{\"ed\":5}"

    context "FromJSON" $ do
      it "should properly decode a value" $ do
        decode "{\"ed\":2}" `shouldBe` (Just (Mock ED.Right))
        decode "{\"ed\":4}" `shouldBe` (Just (Mock Down))

      it "should fail when out of range" $ do
        decode "{\"ed\":0}" `shouldBe` (Nothing :: Maybe Mock)
        decode "{\"ed\":6}" `shouldBe` (Nothing :: Maybe Mock)


instance Arbitrary Mock where
  arbitrary = Mock <$> arbitrary

instance Arbitrary ExpandableDirection where
  arbitrary = toEnum <$> choose (1, 5)
