{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module OpenRTB.Enum.CreativeAttributeSpec where

import Control.Applicative
import Data.Aeson
import Data.Aeson.TH
import Test.Hspec
import Test.QuickCheck
import OpenRTB.Enum.CreativeAttribute

data Mock = Mock { ca :: CreativeAttribute } deriving (Eq, Show)
$(deriveJSON defaultOptions ''Mock)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "CreativeAttributes" $ do
  context "JSON" $ do
    it "should convert back and forth" $ property $ do
      \m -> (decode . encode) m == Just (m :: Mock)

    context "ToJSON" $ do
      it "should properly encode a value" $ do
        encode (Mock AudioAdAutoPlay) `shouldBe` "{\"ca\":1}"
        encode (Mock InBannerVideoAdAutoPlay) `shouldBe` "{\"ca\":6}"

    context "FromJSON" $ do
      it "should properly decode a value" $ do
        decode "{\"ca\":3}" `shouldBe` (Just (Mock ExpandableAuto))
        decode "{\"ca\":2}" `shouldBe` (Just (Mock AudioAdUserInitiated))

      it "should fail when out of range" $ do
        decode "{\"ca\":0}" `shouldBe` (Nothing :: Maybe Mock)
        decode "{\"ca\":17}" `shouldBe` (Nothing :: Maybe Mock)


instance Arbitrary Mock where
  arbitrary = Mock <$> arbitrary

instance Arbitrary CreativeAttribute where
  arbitrary = toEnum <$> choose (1, 16)
