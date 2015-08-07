{-# LANGUAGE OverloadedStrings #-}
module Test.Helper where

import Data.Aeson
import Data.ByteString.Lazy

import OpenRTB.Types.BidRequest.App
import OpenRTB.Types.BidRequest.Device
import OpenRTB.Types.BidRequest.DistributionChannel.Content.Producer
import OpenRTB.Types.BidRequest.DistributionChannel.Content
import OpenRTB.Types.BidRequest.DistributionChannel.Publisher
import OpenRTB.Types.BidRequest.Geo
import OpenRTB.Types.BidRequest.Imp.Banner
import OpenRTB.Types.BidRequest.Imp.Native
import OpenRTB.Types.BidRequest.Imp.Pmp.Deal
import OpenRTB.Types.BidRequest.Imp.Pmp
import OpenRTB.Types.BidRequest.Imp.Video
import OpenRTB.Types.BidRequest.Imp
import OpenRTB.Types.BidRequest.Regs
import OpenRTB.Types.BidRequest.Site
import OpenRTB.Types.BidRequest.User.Data.Segment
import OpenRTB.Types.BidRequest.User.Data
import OpenRTB.Types.BidRequest.User
import OpenRTB.Types.BidRequest
import OpenRTB.Types.BidResponse.SeatBid.Bid
import OpenRTB.Types.BidResponse.SeatBid
import OpenRTB.Types.BidResponse
import OpenRTB.Types.Enum.AdPosition
import OpenRTB.Types.Enum.APIFramework
import OpenRTB.Types.Enum.BannerAdType
import OpenRTB.Types.Enum.ConnectionType
import OpenRTB.Types.Enum.ContentContext
import OpenRTB.Types.Enum.ContentDeliveryMethod
import qualified OpenRTB.Types.Enum.CreativeAttribute as CA
import OpenRTB.Types.Enum.DeviceType
import OpenRTB.Types.Enum.ExpandableDirection
import OpenRTB.Types.Enum.LocationType
import OpenRTB.Types.Enum.NoBidReasonCode
import OpenRTB.Types.Enum.QAGMediaRating
import OpenRTB.Types.Enum.VASTCompanionType
import OpenRTB.Types.Enum.VideoBidResponseProtocol
import OpenRTB.Types.Enum.VideoLinearity
import OpenRTB.Types.Enum.VideoPlaybackMethod
import OpenRTB.Types.Enum.VideoQuality
import OpenRTB.Types.Enum.VideoStartDelay

bid1 :: Bid
bid1 = Bid "id" "impid" 42.2 Nothing Nothing Nothing Nothing Nothing Nothing
         Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

bid2 :: Bid
bid2 = Bid "id" "impid" 42.2 (Just "adid") (Just "nurl") (Just "adm")
         (Just ["adomain", "bdomain"]) (Just "bundle") (Just "iurl")
         (Just "cid") (Just "crid") (Just [()])
         (Just [CA.AudioAdAutoPlay, CA.Pop]) (Just "dealid") (Just 4) (Just 5)
         (Just (object ["key" .= ("value" :: String)]))

bid1Answer :: ByteString
bid1Answer = "{\"id\":\"id\",\"price\":42.2,\"impid\":\"impid\"}"

bid2Answer :: ByteString
bid2Answer = "{\"crid\":\"crid\",\"w\":5,\"iurl\":\"iurl\",\"attr\":[1,8],\"cat\":[[]],\"dealid\":\"dealid\",\"adomain\":[\"adomain\",\"bdomain\"],\"bundle\":\"bundle\",\"id\":\"id\",\"price\":42.2,\"nurl\":\"nurl\",\"h\":4,\"adid\":\"adid\",\"adm\":\"adm\",\"cid\":\"cid\",\"impid\":\"impid\",\"ext\":{\"key\":\"value\"}}"
