{-# LANGUAGE OverloadedStrings #-}
module OpenRTB.Types.BidResponse.SeatBidSpec where

import Prelude hiding (id)

import Control.Applicative
import Data.Aeson
import Test.Hspec
import Test.QuickCheck

import Test.Instances
import OpenRTB.Types.BidResponse.SeatBid

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Bid" $ do
  context "JSON" $ do
    it "should convert back and fourth" $ property $ do
      \b -> (decode . encode) b == Just (b :: SeatBid)
