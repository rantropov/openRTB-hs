module OpenRTB.BidResponse where

import Prelude hiding (id)

import Data.Aeson
import Data.Text

import OpenRTB.BidResponse.SeatBid

-- | This object is the top-level bid response object (i.e., the unnamed outer
--   JSON object). the `id` is a reflection of the bid request ID for logging
--   purposes. Similarly, `bidID` is an optional response tracking for bidders.
--   If specified, it can be included in the subsequent win notice call if the
--   bidder wins. At least one `seatBid` object is required, which contains at
--   least one bid for an impression. Other attributes are optional.
--
--   To express a "no-bid", the options are to return an empty response with
--   HTTP 204. Alternatively if the bidder wishes to convey to the exchange a
--   reason for not bidding, just a `BidResponse` object is returned with a
--   reason code in the `nbr` attribute.
data BidResponse = BidResponse
  {
    -- | ID of the bid request to which this is a response.
    id :: Text

    -- | Array of seatbid object; 1+ required if a bid is to be made.
  , seatBid :: Maybe [SeatBid]

    -- | Bidder generated response ID to assist with logging/tracking.
  , bidID :: Maybe Text

    -- | Bid currency using ISO-4217 alpha codes.
  , cur :: Text

    -- | Optional feature to allow a bidder to set data in the exchange's
    --   cookie. The string must be base85 cookie safe characters and be in
    --   any format. Proper JSON encoding must be used to include "escaped"
    --   quotation marks.
  , customData :: Maybe Text

    -- | Reason for not bidding. Refer to List 5.19.
  , nbr :: Maybe ()

    -- | Placeholder for bidder-specific extensions to OpenRTB
  , ext :: Maybe Value
  }
