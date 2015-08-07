module OpenRTB.Types.BidRequest where

import Prelude hiding (id)

import Control.Applicative
import Data.Aeson
import Data.Text
import Data.Word

import OpenRTB.Types.BidRequest.App
import OpenRTB.Types.BidRequest.Device
import OpenRTB.Types.BidRequest.Imp
import OpenRTB.Types.BidRequest.Regs
import OpenRTB.Types.BidRequest.Site
import OpenRTB.Types.BidRequest.User

-- | The top-level bid request object contains a globally unique bid request or
--   auction ID. This `id` attribute is required as is at least one impression
--   object (Section 3.2.2). Other attributes in this top-level object establish
--   rules and restrictions that apply to all impressions being offered.
--
--   There are also several subordinate objects that provide detailed data to
--   potential buyers. Among these are `Site` and `App` objects, which describe
--   the type of published media in which the impression(s) appear. These
--   objects are highly recommended, but only one applies to a given bid request
--   depending on whether the media is browser-based web content or a
--   non-browser application, respectively.
data BidRequest = BidRequest
  {
    -- | Unique ID of the bid request, provided by the exchange
    id      :: Text

    -- | Array of `Imp` objects (Section 3.2.2) representing the impressions
    --   offered. At least 1 `Imp` object is required.
  , imp     :: [Imp]

    -- | Details via a `Site` object (Section 3.2.6) about the publisher's
    --   website. Only applicable and recommended for websites.
  , site    :: Maybe Site

    -- | Details via an `App` object (Section 3.2.7) about the publisher's app
    --   (i.e., non-browser applications). Only applicable and recommended for
    --   apps.
  , app     :: Maybe App

    -- | Details via a `Device` object (Section 3.2.11) about the user's device
    --   to which the impression will be delivered.
  , device  :: Maybe Device

    -- | Details via a `User` object (Section 3.2.13) about the human user of
    --   the device; the advertising audience.
  , user    :: Maybe User

    -- | Indicator of test mode in which auctions are not billable, where 0 =
    --   live mode, 1 = test mode.
  , test    :: Bool

    -- | Auction type, where 1 = First Price, 3 = Second Price Plus.
    --   Excuange-specific auction types can be defined using values greater
    --   Than 500
  , at      :: Word8

    -- | Maximum time in milliseconds to submit a bid to avoid timeout. This
    --   value is commonly communicated offline.
  , tMax    :: Maybe Word16

    -- | Whitelist of buyer seats allowed to bid on this impression. Seat IDs
    --   must be communicated between bidders and the exchange *a priori*.
    --   Omission implies no seat restrictions.
  , wSeat   :: [Text]

    -- | Flag to indicate if Exchange can verify that the impressions offered
    --   represent all of the impressions available in context (e.g., all on
    --   the web page, all video spots such as pre/mid/post roll) to support
    --   road-blocking. 0 = no or unknown, 1 = yes, the impressions offered
    --   represent all that are available.
  , allImps :: Maybe Bool

    -- | Array of allowed currencies for bids on this bid request using ISO-4217
    --   alpha codes. Recommended only if exchange accepts multiple currencies.
  , cur     :: [Maybe Text]

    -- | Blocked advertiser categories using the IAB content categories. Refer
    --   to List 5.1.
  , bCat    :: [Maybe Text]

    -- | Block list of advertisers by their domains (e.g., "ford.com").
  , bAdv    :: [Maybe Text]

    -- | A `Regs` object (Section 3.2.16) that specifies any industry, legel,
    --   or governmental regulations in force for this request.
  , regs    :: Maybe Regs

    -- | Placeholder for exchange-specific extensions to OpenRTB.
  , ext     :: Maybe Value
  }
