{-# LANGUAGE OverloadedStrings #-}
module OpenRTB.Types.BidResponse.SeatBid.BidSpec where

import Prelude hiding (id)

import Control.Applicative
import Data.Aeson
import Test.Hspec
import Test.QuickCheck

import Test.Helper
import Test.Instances
import OpenRTB.Types.BidResponse.SeatBid.Bid
import OpenRTB.Types.Enum.CreativeAttribute

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Bid" $ do
  context "JSON" $ do
    it "should convert back and fourth" $ property $ do
      \b -> (decode . encode) b == Just (b :: Bid)

    context "ToJSON" $ do
      it "should properly encode a value" $ do
        encode bid1 `shouldBe` bid1Answer
        encode bid2 `shouldBe` bid2Answer

    context "FromJSON" $ do
      it "should properly decode the example value" $ do
        (decode "{\"id\": \"1\", \"impid\": \"102\", \"price\": 9.43,\"nurl\": \"http://adserver.com/winnotice?impid=102\",\"iurl\": \"http://adserver.com/pathtosampleimage\",\"adomain\": [ \"advertiserdomain.com\" ],\"cid\": \"campaign111\",\"crid\": \"creative112\",\"attr\": [ 1, 2, 3, 4, 5, 6, 7, 12 ]}" :: Maybe Bid)
          `shouldBe` Just (Bid {id = "1", impID = "102", price = 9.43, adID = Nothing, nURL = Just "http://adserver.com/winnotice?impid=102", adm = Nothing, aDomain = Just ["advertiserdomain.com"], bundle = Nothing, iURL = Just "http://adserver.com/pathtosampleimage", cID = Just "campaign111", crID = Just "creative112", cat = Nothing, attr = Just [AudioAdAutoPlay,AudioAdUserInitiated,ExpandableAuto,ExpandableUserClick,ExpandableUserRoll,InBannerVideoAdAutoPlay,InBannerVideoAdUserInitiated,Text], dealID = Nothing, h = Nothing, w = Nothing, ext = Nothing})
