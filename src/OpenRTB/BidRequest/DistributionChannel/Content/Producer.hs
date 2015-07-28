module OpenRTB.BidRequest.DistributionChannel.Content.Producer where

import Prelude hiding (id)

import Data.Aeson
import Data.Text

-- | This object defined the producer of the content in which the ad will be
--   shown. This is particularly useful when the content is syndicated and
--   may be distributed through different publishers and thus when the producer
--   and publisher are not necessarily the same entity.
data Producer = Producer
  {
    -- | Content producer or originator ID. Useful if content is syndicated
    --   and may be posted on a site using embed tags.
    id :: Maybe Text

    -- | Content producer or originator name (e.g., "Warner Bros").
  , name :: Maybe Text

    -- | Array of IAB content categories that describe the content producer.
    --   Refer to List 5.1.
  , cat :: Maybe [()]

    -- | Highest level domain of the content producer (e.g., "producer.com").
  , domain :: Maybe Text

    -- | Placeholder for exchange-specific extensions to OpenRTB.
  , ext :: Maybe Value
  }
