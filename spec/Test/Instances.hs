module Test.Instances where

import Control.Applicative
import Data.Aeson
import Data.Scientific
import qualified Data.Text as T
import Test.QuickCheck

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

instance Arbitrary SeatBid where
  arbitrary = SeatBid <$> arbitrary <*> maybeA <*> arbitrary <*> maybeA

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
