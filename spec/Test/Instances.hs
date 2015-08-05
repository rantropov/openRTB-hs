module Test.Instances where

import Control.Applicative
import Data.Aeson
import Data.Scientific
import qualified Data.Text as T
import Test.QuickCheck

import OpenRTB.BidRequest.App
import OpenRTB.BidRequest.Device
import OpenRTB.BidRequest.DistributionChannel.Content.Producer
import OpenRTB.BidRequest.DistributionChannel.Content
import OpenRTB.BidRequest.DistributionChannel.Publisher
import OpenRTB.BidRequest.Geo
import OpenRTB.BidRequest.Imp.Banner
import OpenRTB.BidRequest.Imp.Native
import OpenRTB.BidRequest.Imp.Pmp.Deal
import OpenRTB.BidRequest.Imp.Pmp
import OpenRTB.BidRequest.Imp.Video
import OpenRTB.BidRequest.Imp
import OpenRTB.BidRequest.Regs
import OpenRTB.BidRequest.Site
import OpenRTB.BidRequest.User.Data.Segment
import OpenRTB.BidRequest.User.Data
import OpenRTB.BidRequest.User
import OpenRTB.BidRequest
import OpenRTB.BidResponse.SeatBid.Bid
import OpenRTB.BidResponse.SeatBid
import OpenRTB.BidResponse
import OpenRTB.Enum.AdPosition
import OpenRTB.Enum.APIFramework
import OpenRTB.Enum.BannerAdType
import OpenRTB.Enum.ConnectionType
import OpenRTB.Enum.ContentContext
import OpenRTB.Enum.ContentDeliveryMethod
import qualified OpenRTB.Enum.CreativeAttribute as CA
import OpenRTB.Enum.DeviceType
import OpenRTB.Enum.ExpandableDirection
import OpenRTB.Enum.LocationType
import OpenRTB.Enum.NoBidReasonCode
import OpenRTB.Enum.QAGMediaRating
import OpenRTB.Enum.VASTCompanionType
import OpenRTB.Enum.VideoBidResponseProtocol
import OpenRTB.Enum.VideoLinearity
import OpenRTB.Enum.VideoPlaybackMethod
import OpenRTB.Enum.VideoQuality
import OpenRTB.Enum.VideoStartDelay

instance Arbitrary Bid where
  arbitrary = Bid <$> arbitrary <*> arbitrary <*> arbitrary <*> maybeA <*>
                maybeA <*> maybeA <*> maybeA <*> maybeA <*> maybeA <*>
                maybeA <*> maybeA <*> maybeA <*> maybeA <*> maybeA <*>
                maybeA <*> maybeA <*> maybeA

maybeA :: (Arbitrary a) => Gen (Maybe a)
maybeA = oneof [ pure Nothing
               , liftA Just arbitrary
               ]

instance Arbitrary Value where
  arbitrary = oneof [ liftA String arbitrary
                    , liftA Number arbitrary
                    , liftA Bool arbitrary
                    ]

instance Arbitrary Scientific where
  arbitrary = scientific <$> arbitrary <*> arbitrary

instance Arbitrary T.Text where
    arbitrary = T.pack <$> arbitrary

instance Arbitrary AdPosition where
  arbitrary = toEnum <$> choose (0, 7)


instance Arbitrary APIFramework where
  arbitrary = toEnum <$> choose (1, 5)


instance Arbitrary BannerAdType where
  arbitrary = toEnum <$> choose (1, 4)


instance Arbitrary ConnectionType where
  arbitrary = toEnum <$> choose (0, 6)


instance Arbitrary ContentContext where
  arbitrary = toEnum <$> choose (1, 7)


instance Arbitrary ContentDeliveryMethod where
  arbitrary = toEnum <$> choose (1, 2)


instance Arbitrary CA.CreativeAttribute where
  arbitrary = toEnum <$> choose (1, 16)


instance Arbitrary DeviceType where
  arbitrary = toEnum <$> choose (1, 7)


instance Arbitrary ExpandableDirection where
  arbitrary = toEnum <$> choose (1, 5)


instance Arbitrary LocationType where
  arbitrary = toEnum <$> choose (1, 3)


instance Arbitrary NoBidReasonCode where
  arbitrary = toEnum <$> choose (0, 8)


instance Arbitrary QAGMediaRating where
  arbitrary = toEnum <$> choose (1, 3)


instance Arbitrary VASTCompanionType where
  arbitrary = toEnum <$> choose (1, 3)


instance Arbitrary VideoBidResponseProtocol where
  arbitrary = toEnum <$> choose (1, 6)


instance Arbitrary VideoLinearity where
  arbitrary = toEnum <$> choose (1, 2)


instance Arbitrary VideoPlaybackMethod where
  arbitrary = toEnum <$> choose (1, 4)

instance Arbitrary VideoQuality where
  arbitrary = toEnum <$> choose (0, 3)


instance Arbitrary VideoStartDelay where
  arbitrary = toEnum <$> choose (0, 3)
