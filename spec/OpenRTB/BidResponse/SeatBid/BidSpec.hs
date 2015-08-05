{-# LANGUAGE OverloadedStrings #-}
module OpenRTB.BidResponse.SeatBid.BidSpec where

import Prelude hiding (id)

import Control.Applicative
import Data.Aeson
import Test.Hspec
import Test.QuickCheck

import Test.Instances
import OpenRTB.BidResponse.SeatBid.Bid
import OpenRTB.Enum.CreativeAttribute

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Bid" $ do
  context "JSON" $ do
    it "should convert back and fourth" $ property $ do
      \b -> (decode . encode) b == Just (b :: Bid)

    context "ToJSON" $ do
      it "should properly encode a value" $ do
        encode bid1 `shouldBe` "{\"id\":\"id\",\"price\":42.2,\"impid\":\"impid\"}"
        encode bid2 `shouldBe` "{\"crid\":\"crid\",\"w\":5,\"iurl\":\"iurl\",\"attr\":[1,8],\"cat\":[[]],\"dealid\":\"dealid\",\"adomain\":[\"adomain\",\"bdomain\"],\"bundle\":\"bundle\",\"id\":\"id\",\"price\":42.2,\"nurl\":\"nurl\",\"h\":4,\"adid\":\"adid\",\"adm\":\"adm\",\"cid\":\"cid\",\"impid\":\"impid\",\"ext\":{\"key\":\"value\"}}"

    context "FromJSON" $ do
      it "should properly decode the example value" $ do
        (decode "{\"id\": \"1\", \"impid\": \"102\", \"price\": 9.43,\"nurl\": \"http://adserver.com/winnotice?impid=102\",\"iurl\": \"http://adserver.com/pathtosampleimage\",\"adomain\": [ \"advertiserdomain.com\" ],\"cid\": \"campaign111\",\"crid\": \"creative112\",\"attr\": [ 1, 2, 3, 4, 5, 6, 7, 12 ]}" :: Maybe Bid)
          `shouldBe` Just (Bid {id = "1", impID = "102", price = 9.43, adID = Nothing, nURL = Just "http://adserver.com/winnotice?impid=102", adm = Nothing, aDomain = Just ["advertiserdomain.com"], bundle = Nothing, iURL = Just "http://adserver.com/pathtosampleimage", cID = Just "campaign111", crID = Just "creative112", cat = Nothing, attr = Just [AudioAdAutoPlay,AudioAdUserInitiated,ExpandableAuto,ExpandableUserClick,ExpandableUserRoll,InBannerVideoAdAutoPlay,InBannerVideoAdUserInitiated,Text], dealID = Nothing, h = Nothing, w = Nothing, ext = Nothing})



bid1 :: Bid
bid1 = Bid "id" "impid" 42.2 Nothing Nothing Nothing Nothing Nothing Nothing
         Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

bid2 :: Bid
bid2 = Bid "id" "impid" 42.2 (Just "adid") (Just "nurl") (Just "adm")
         (Just ["adomain", "bdomain"]) (Just "bundle") (Just "iurl")
         (Just "cid") (Just "crid") (Just [()]) (Just [AudioAdAutoPlay, Pop])
         (Just "dealid") (Just 4) (Just 5)
         (Just (object ["key" .= ("value" :: String)]))
