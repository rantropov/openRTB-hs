module OpenRTB.BidRequest.Imp.Pmp.Deal where

import Prelude hiding (id)

import Data.Aeson
import Data.Text
import Data.Word

-- | This object constitutes a specific deal that was struck *a priori* between
--   a buyer and a seller. Its presence with the `Pmp` collection indicates
--   that this impression is available under the terms of that deal. Refer to
--   Section 7.2 for more details.
data Deal = Deal
  {
    -- | A unique identifier for the direct deal.
    id :: Text

    -- | Minimum bid for this impression expressed in CPM.
  , bidFloor :: Double

    -- | Currency specified using ISO-4217 alpha codes. This may be different
    --   from bid currency returned by bidder if this is allowed by the
    --   exchange.
  , bidFloorCur :: Text

    -- | Optional override of the overall auction type of the bid request, where
    --   1 = First Price, 2 = Second Price Plus, 3 = the value passed in
    --   `bidFloor` is the agreed upon deal price. Additional auction type
    --   can be defined by the exchange.
  , at :: Maybe Word16

    -- | Whitelist of buyer seats allowed to bid on this deal. Seat IDs must be
    --   communicated between bidders and the exchange *a priori*. Omission
    --   implies no seat restictions
  , wSeat :: Maybe [Text]

    -- | Array of advertiser domains (e.g., advertiser.com) allowed to bid on
    --   this deal. Omission implies no advertiser restrictions.
  , wADomain :: Maybe [Text]

    -- | Placeholder for exchange-specfic extensions to OpenRTB.
  , ext :: Maybe Value
  }
