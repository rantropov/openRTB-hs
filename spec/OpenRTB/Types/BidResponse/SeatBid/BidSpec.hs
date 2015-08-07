{-# LANGUAGE OverloadedStrings #-}
module OpenRTB.Types.BidResponse.SeatBid.BidSpec where

import Prelude hiding (id)

import Control.Applicative
import Data.Aeson
import Data.ByteString.Lazy
import Test.Hspec
import Test.QuickCheck

import Test.Instances
import OpenRTB.Types.BidResponse.SeatBid.Bid
import qualified OpenRTB.Types.Enum.CreativeAttribute as CA

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Bid" $ do
  context "JSON" $ do
    it "should convert back and fourth" $ property $ do
      \b -> (decode . encode) b == Just (b :: Bid)
