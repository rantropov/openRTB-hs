module OpenRTB.BidResponse.SeatBid.Bid where

import Prelude hiding (id)

import Data.Aeson
import Data.Text
import Data.Word

-- | A `SeatBid` object contains one or more `Bid` objects, each of which
--   relates to a specific impression in the bid request via the `impID`
--   attribute and constitutes an offer to buy that impression for a given
--   `price`.
data Bid = Bid
  {
    -- | Bidder generated bid ID to assist with logging/tracking
    id :: Text

    -- | ID of the `Imp` object in the related bid request.
  , impID :: Text

    -- | Bid price expressed as CPM although the actual transaction is for a
    --   unit impression only. Note that while the type indicates float,
    --   integer math is highly recommended when handling currencies
    --   (e.g., BigDecimal in Java).
  , price :: Double

    -- | ID of a preloaded ad to be served if the bid wins.
  , adID :: Maybe Text

    -- | Win notice URL called by the exchange if the bid wins; optional means
    --   of serving ad markup.
  , nURL :: Maybe Text

    -- | Optional means of conveying ad markup in case the bid wins; supersedes
    --   the win notice if markup is included in both.
  , adm :: Maybe Text

    -- | Advertiser domain for block list checking (e.g., "ford.com"). This can
    --   be an array for the case of rotating creatives. Exchanges can mandate
    --   that only one domain is allowed.
  , aDomain :: Maybe [Text]

    -- | Bundle or package name (e.g., com.foo.mygame) of the app being
    --   advertised, if applicable; intended to be a unique ID across exchanges.
  , bundle :: Maybe Text

    -- | URL without cache-busting to an image that is representative of the
    --   content of the campaign for ad quality/safety checking.
  , iURL :: Maybe Text

    -- | Campaign ID to assist with ad quality checking; the collection of
    --   creatives for which `iURL` should be representative.
  , cID :: Maybe Text

    -- | Creative ID to assist with ad quality checking.
  , crID :: Maybe Text

    -- | IAB content categories of the creative. Refer to List 5.1.
  , cat :: Maybe [()]

    -- | Set of attributes describing the creative. Refer to List 5.3.
  , attr :: Maybe [()]

    -- | Reference to the `Deal.id` from the bid request if this bid pertains
    --   to a private marketplace direct deal.
  , dealID :: Maybe Text

    -- | Height of the creative in pixels.
  , h :: Maybe Word16

    -- | Width of the creative in pixels.
  , w :: Maybe Word16

    -- | Placeholder for bidder-specific extensions to OpenRTB.
  , ext :: Maybe Value
  }
