module OpenRTB.BidRequest.DistributionChannel.Content where

import Prelude hiding (id)

import Data.Aeson
import Data.Text
import Data.Word

import OpenRTB.BidRequest.DistributionChannel.Content.Producer
import OpenRTB.Enum.ContentContext
import OpenRTB.Enum.QAGMediaRating
import OpenRTB.Enum.VideoQuality

-- | This object describes the content in which the impression will appear,
--   which may be syndicated or non-syndicated content. This object may be
--   useful when syndicated content contains impressions and does not
--   necessarily match the publisher's general content. The exchange might or
--   might not have knowledge of the page where the content is running, as a
--   result of syndication method. For example might be a video impression
--   embedded in an iframe on an unknown web property or device.
data Content = Content
  {
    -- | ID uniquely identifying the content.
    id :: Maybe Text

    -- | Episode number (typically applied to video content).
  , episode :: Maybe Word16

    -- | Content title.
    --
    --   *Video Examples:* "Search Committee" (television), "A New Hope"
    --   (movie), or "Endgame" (made for web).
    --
    --   *Non-Video Example* "Why an Antarctic Glacier Is Melting So Quickly"
    --   (Time magazine article).
  , series :: Maybe Text

    -- | Content season; typically for video content (e.g., "Season 3").
  , season :: Maybe Text

    -- | Details about the content `Producer` (Section 3.2.10).
  , producer :: Maybe Producer

    -- | URL of the content, for buy-side contextualization or review.
  , url :: Maybe Text

    -- | Array of IAB content categories that describe the content producer.
    --   Refer to List 5.1.
  , cat :: Maybe [()]

    -- | video quality per IAB's classification. Refer to List 5.1.
  , videoQuality :: Maybe VideoQuality

    -- | Type of content (game, video, text, etc.). Refer to List 5.14
  , context :: Maybe ContentContext

    -- | Content rating (e.g., MPAA).
  , contentRating :: Maybe Text

    -- | User rating of the content (e.g., number of stars, likes, etc.).
  , userRating :: Maybe Text

    -- | Media rating per QAG guidelines. Refer to List 5.15.
  , qagMediaRating :: Maybe QAGMediaRating

    -- | Comma separated list of keywords describing the content.
  , keywords :: Maybe Text

    -- | 0 = not live, 1 - content is live (e.g., stream, live blog).
  , liveStream :: Maybe Bool

    -- | 0 = indirect, 1 = direct.
  , sourceRelationship :: Maybe Bool

    -- | Length of content in seconds; appropriate for video or audio.
  , len :: Maybe Word16

    -- | Content language using ISO-639-1-alpha-2.
  , language :: Maybe Text

    -- | Indicator of whether or not the content is embeddable (e.g., an
    --   embeddable video player), where 0 = no, 1 = yes
  , embeddable :: Maybe Bool

    -- | Placeholder for exchange-specific extensions to OpenRTB.
  , ext :: Maybe Value
  }
