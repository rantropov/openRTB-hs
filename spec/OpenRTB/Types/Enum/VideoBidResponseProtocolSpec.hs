{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module OpenRTB.Types.Enum.VideoBidResponseProtocolSpec where

import Control.Applicative
import Data.Aeson
import Data.Aeson.TH
import Test.Hspec
import Test.QuickCheck

import Test.Instances
import OpenRTB.Types.Enum.VideoBidResponseProtocol

data Mock = Mock { vbrp :: VideoBidResponseProtocol } deriving (Eq, Show)
$(deriveJSON defaultOptions ''Mock)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "VideoBidResponseProtocol" $ do
  context "JSON" $ do
    it "should convert back and forth" $ property $ do
      \m -> (decode . encode) m == Just (m :: Mock)

instance Arbitrary Mock where
  arbitrary = Mock <$> arbitrary
