module OpenRTB.BidRequest.DistributionChannel.Publisher where

import Prelude hiding (id)

import Data.Aeson
import Data.Text

-- | This object describes the publisher of the media in which the ad will be
--   displayed. The publisher is typically the seller in an OpenRTB
--   transaction.
data Publisher = Publisher
  {
    -- | Exchange-specific publisher ID.
    id :: Maybe Text

    -- | Publisher name (may be aliased at the publisher's request).
  , name :: Maybe Text

    -- | Array of IAB content categories that describe the publisher. Refer
    --   to List 5.1.
  , cat :: Maybe [()]

    -- | Highest level domain of the publisher (e.g., "publisher.com").
  , domain :: Maybe Text

    -- | Placeholder for exchange-specific extensions to OpenRTB.
  , ext :: Maybe Value
  }
