{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module OpenRTB.Types.Enum.AdPositionSpec where

import Control.Applicative
import Data.Aeson
import Data.Aeson.TH
import Test.Hspec
import Test.QuickCheck

import Test.Instances
import OpenRTB.Types.Enum.AdPosition

data Mock = Mock { ap :: AdPosition } deriving (Eq, Show)
$(deriveJSON defaultOptions ''Mock)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "AdPosition" $ do
  context "JSON" $ do
    it "should convert back and forth" $ property $ do
      \m -> (decode . encode) m == Just (m :: Mock)

    context "ToJSON" $ do
      it "should properly encode a value" $ do
        encode (Mock Unknown) `shouldBe` "{\"ap\":0}"
        encode (Mock FullScreen) `shouldBe` "{\"ap\":7}"

    context "FromJSON" $ do
      it "should properly decode a value" $ do
        decode "{\"ap\":3}" `shouldBe` (Just (Mock BelowTheFold))
        decode "{\"ap\":2}" `shouldBe` (Just (Mock DEPRECATED))

      it "should fail when out of range" $ do
        decode "{\"ap\":-1}" `shouldBe` (Nothing :: Maybe Mock)
        decode "{\"ap\":8}" `shouldBe` (Nothing :: Maybe Mock)


instance Arbitrary Mock where
  arbitrary = Mock <$> arbitrary
