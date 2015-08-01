{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module OpenRTB.Enum.VideoPlaybackMethodSpec where

import Control.Applicative
import Data.Aeson
import Data.Aeson.TH
import Test.Hspec
import Test.QuickCheck
import OpenRTB.Enum.VideoPlaybackMethod

data Mock = Mock { vpm :: VideoPlaybackMethod } deriving (Eq, Show)
$(deriveJSON defaultOptions ''Mock)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "VideoPlaybackMethod" $ do
  context "JSON" $ do
    it "should convert back and forth" $ property $ do
      \m -> (decode . encode) m == Just (m :: Mock)

    context "ToJSON" $ do
      it "should properly encode a value" $ do
        encode (Mock AutoPlaySoundOn) `shouldBe` "{\"vpm\":1}"
        encode (Mock MouseOver) `shouldBe` "{\"vpm\":4}"

    context "FromJSON" $ do
      it "should properly decode a value" $ do
        decode "{\"vpm\":2}" `shouldBe` (Just (Mock AutoPlaySoundOff))
        decode "{\"vpm\":3}" `shouldBe` (Just (Mock ClickToPlay))

      it "should fail when out of range" $ do
        decode "{\"vpm\":0}" `shouldBe` (Nothing :: Maybe Mock)
        decode "{\"vpm\":5}" `shouldBe` (Nothing :: Maybe Mock)


instance Arbitrary Mock where
  arbitrary = Mock <$> arbitrary

instance Arbitrary VideoPlaybackMethod where
  arbitrary = toEnum <$> choose (1, 4)
