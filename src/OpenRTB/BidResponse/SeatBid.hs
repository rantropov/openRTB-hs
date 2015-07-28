module OpenRTB.BidResponse.SeatBid where

import Data.Aeson
import Data.Text

import OpenRTB.BidResponse.SeatBid.Bid

-- | A bid response can contain multiple `SeatBid` objects, each on behalf of a
--   different bidder seat and each containing one or more individual bids. If
--   multiple impressions are presented in the request, the `group` attribute
--   can be used to specify if a seat is willing to accept any impressions that
--   it can win (default) or if it is only interested in winning if it can win
--   them all as a group.
data SeatBid = SeatBid
  {
    -- | Array of 1+ `Bid` objects (Section 4.2.3) each related to an
    --   impression.
    bid :: [Bid]

    -- | ID of the bidder seat on whose behalf this bid is made.
  , seat :: Maybe Text

    -- | 0 = impressions can be won individually; 1 = impressions must be won or
    --   lose as a group.
  , group :: Bool

    -- | Placeholder for bidder-specific extensions to OpenRTB.
  , ext :: Maybe Value
  }
