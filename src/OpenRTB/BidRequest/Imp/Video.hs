module OpenRTB.BidRequest.Imp.Video where

import Control.Applicative
import Data.Aeson
import Data.Text
import Data.Word

import OpenRTB.BidRequest.Imp.Banner
import OpenRTB.Enum.VideoBidResponseProtocols
import OpenRTB.Enum.VideoStartDelay
import OpenRTB.Enum.VideoLinearity
import OpenRTB.Enum.CreativeAttributes
import OpenRTB.Enum.VideoPlaybackMethods
import OpenRTB.Enum.ContentDeliveryMethods
import OpenRTB.Enum.AdPosition
import OpenRTB.Enum.APIFrameworks
import OpenRTB.Enum.VASTCompanionTypes

-- | This object represents an in-stream video impression. Many of the fields
--   are non-essential for minimally viable transactions, but are included to
--   offer fine control when needed. Video in OpenRTB generally assumes
--   compliance with the VAST standard. As such, the notion of companion ads is
--   supported by optionally including an array of `Banner` objects (refer to
--   the `Banner` object Section 3.2.3) that defined these companion ads.
--
--   The presence of a `Video` as a subordinate of the `Imp` objects indicates
--   that this impression is offered as a video type impression. At the
--   publishers discretion, that same impression may also be offered as banner
--   and/or native by also including as `Imp` subordinates the `Banner` and/or
--   `Native` objects, respectively. However, any given bid for the impression
--   must conform to one of the offered types.
data Video = Video
  {
    -- | Content MIME types supported. Popular MIME types may include
    --   "video/x-ms-wmv" for Windows Media and "video/x-flv" for Flash Video.
    mimes :: [Text]

    -- | Minimum video ad duration in seconds.
  , minDuration :: Maybe Word16

    -- | Maximum video ad duration in seconds.
  , maxDuration :: Maybe Word16

    -- | *NOTE: Use of `protocols` instead is highly recommended.*
    --
    --   Supported video bid response protocol. Refer to List 5.8. At least one
    --   supported protocol must be specified in either the `protocol` or
    --   `protocols` attribute.
  , protocol :: Maybe VideoBidResponseProtocol

    -- | Array of supported video bid response protocols. Refer to List 5.8. At
    --   least one supported protocol must be specified in either the `protocol`
    --   or `protocols` attribute.
  , protocols :: Maybe [VideoBidResponseProtocol]

    -- | Width of the video player in pixels.
  , w :: Maybe Word16

    -- | Height of the video player in pixels.
  , h :: Maybe Word16

    -- | Indicates the start delay in seconds for pre-roll, mid-roll, or
    --   post-roll ad placements. Refer to List 5.10 for additional generic
    --   values.
  , startDelay :: Maybe VideoStartDelay

    -- | Indicates if the impression must be linear, nonlinear, etc. If none
    --   specified, assume all are allowed. Refer to List 5.7.
  , linearity :: Maybe VideoLinearity

    -- | If multiple ad impressions are offered in the same bid request, the
    --   sequence number will allow for the coordinated delivery of multiple
    --   creatives.
  , sequence :: Maybe Word8

    -- | Blocked creative attributes. Refer to List 5.3.
  , bAttr :: [CreativeAttribute]

    -- | Maximum extended video ad duration if extension is allowed. If blank
    --   or 0, extension is not allowed, if -1, the extension is allowed, and
    --   there is no time limit imposed. If greather than 0, then the value
    --   represents the number of seconds of extended play supported beyond
    --   the `maxDuration` value.
  , maxExtended :: MaxExtended

    -- | Minimum bit rate in Kbps. Exchange may set this dynamically or
    --   universally across their set of publishers.
  , minBitRate :: Maybe Word16

    -- | Maximum bit rate in Kbps. Exchange may set this dynamically or
    --   universally across their set of publishers.
  , maxBitRate :: Maybe Word16

    -- | Indicates if letter-boxing of 4:3 content into 16:9 window is allowed,
    --   where 0 = no, 1 = yes
  , boxingAllowed :: Bool

    -- | Allowed playback methods. If none specified, assume all are allowed.
    --   Refer to List 5.9.
  , playbackMethod :: Maybe [VideoPlaybackMethod]

    -- | Supported delivery methods (e.g., streaming, progressive). If none
    --   specified, assume all are supported. Refer to list 5.13.
  , delivery :: [ContentDeliveryMethod]

    -- | Ad position on screen. Refer to List 5.4.
  , pos :: Maybe AdPosition

    -- | Array of `Banner` objects (Section 3.2.3. if companion ads are
    --   available.
  , companionAd :: Maybe [Banner]

    -- | List of supported API frameworks for this impression. Refer to List
    --   5.6. If an API is not explicitly listed, it is assumed not to be
    --   supported.
  , api :: [APIFramework]

    -- | Supported VAST companion ad types. Refer to List 5.12. Recommended if
    --   companion `Banner` objects are included via the `companionAd` array.
  , companionType :: Maybe [VASTCompanionType]

    -- | Placeholder for exchange-specific extensions to OpenRTB.
  , ext :: Maybe Value
  }

data MaxExtended =
    Unlimited
  | NotAllowed
  | Allowed Integer
