{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module OpenRTB.Enum.VASTCompanionTypeSpec where

import Control.Applicative
import Data.Aeson
import Data.Aeson.TH
import Test.Hspec
import Test.QuickCheck
import OpenRTB.Enum.VASTCompanionType

data Mock = Mock { vct :: VASTCompanionType } deriving (Eq, Show)
$(deriveJSON defaultOptions ''Mock)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "VASTCompanionTypes" $ do
  context "JSON" $ do
    it "should convert back and forth" $ property $ do
       \m -> (decode . encode) m == Just (m :: Mock)

    context "ToJSON" $ do
      it "should properly encode a value" $ do
        encode (Mock Static) `shouldBe` "{\"vct\":1}"
        encode (Mock HTML) `shouldBe` "{\"vct\":2}"

    context "FromJSON" $ do
      it "should properly decode a value" $ do
        decode "{\"vct\":1}" `shouldBe` (Just (Mock Static))
        decode "{\"vct\":3}" `shouldBe` (Just (Mock IFrame))

      it "should fail when out of range" $ do
        decode "{\"vct\":-1}" `shouldBe` (Nothing :: Maybe Mock)
        decode "{\"vct\":4}" `shouldBe` (Nothing :: Maybe Mock)


instance Arbitrary Mock where
  arbitrary = Mock <$> arbitrary

instance Arbitrary VASTCompanionType where
  arbitrary = toEnum <$> choose (1, 3)
