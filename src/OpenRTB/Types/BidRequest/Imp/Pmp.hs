module OpenRTB.Types.BidRequest.Imp.Pmp where

import Data.Aeson

import OpenRTB.Types.BidRequest.Imp.Pmp.Deal

-- | This object is a private marketplace container for direct deals between
--   buyers and sellers that may pertain to this impression. The actual deals
--   are represented as a collection of `Deal` objects. Refer to Section 7.2
--   for more details.
data Pmp = Pmp
  {
    -- | Indicator of auction eligibility to seats named in the Direct Deals
    --   object, where 0 = all bids are accepted, 1 = bids are restricted to
    --   the deals specified and the terms thereof.
    privateAuction :: Maybe Bool

    -- | Array of Deal (Section 3.2.18) objects that convey the specific deals
    --   applicable to this impression.
  , deals :: Maybe [Deal]

    -- | Placeholder for exchange-specific extensions to OpenRTB.
  , ext :: Maybe Value
  }
