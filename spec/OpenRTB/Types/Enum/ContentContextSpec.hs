{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module OpenRTB.Types.Enum.ContentContextSpec where

import Control.Applicative
import Data.Aeson
import Data.Aeson.TH
import Test.Hspec
import Test.QuickCheck

import Test.Instances
import OpenRTB.Types.Enum.ContentContext

data Mock = Mock { cc :: ContentContext } deriving (Eq, Show)
$(deriveJSON defaultOptions ''Mock)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "ContentContext" $ do
  context "JSON" $ do
    it "should convert back and forth" $ property $ do
      \m -> (decode . encode) m == Just (m :: Mock)

    context "ToJSON" $ do
      it "should properly encode a value" $ do
        encode (Mock Application) `shouldBe` "{\"cc\":4}"
        encode (Mock Other) `shouldBe` "{\"cc\":6}"

    context "FromJSON" $ do
      it "should properly decode a value" $ do
        decode "{\"cc\":3}" `shouldBe` (Just (Mock Music))
        decode "{\"cc\":2}" `shouldBe` (Just (Mock Game))

      it "should fail when out of range" $ do
        decode "{\"cc\":0}" `shouldBe` (Nothing :: Maybe Mock)
        decode "{\"cc\":8}" `shouldBe` (Nothing :: Maybe Mock)


instance Arbitrary Mock where
  arbitrary = Mock <$> arbitrary
