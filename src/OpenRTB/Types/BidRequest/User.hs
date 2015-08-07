module OpenRTB.Types.BidRequest.User where

import Prelude hiding (id)

import Data.Aeson
import Data.Text
import Data.Word

import OpenRTB.Types.BidRequest.User.Data
import OpenRTB.Types.BidRequest.Geo

-- | This object contains information known or derived about the human user of
--   the device (i.e., the audience for advertising). The user `id` is an
--   exchange artifact and may be subject to rotation or other privacy policies.
--   However, this user ID must be stable long enough to serve reasonably as the
--   basis for frequency capping and retargeting.
data User = User
  {
    -- | Exchange-specific ID for the user. At least one of `id` or `buyerID`
    --   is recommended.
    id :: Maybe Text

    -- | Buyer-specific ID for the user as mapped by the exchange for the
    --   buyer. At least one of `buyerID` or `id` is recommended.
  , buyerID :: Maybe Text

    -- | Year of birth as a 4-digit integer.
  , yob :: Maybe Word16

    -- | Gender, where "M" = male, "F" = female, "O" = known to be other
    --   (i.e., omitted is unknown).
  , gender :: Maybe Gender

    -- | Comma separated list of keywords, interests, or intent.
  , keywords :: Maybe Text

    -- | Optional feature to pass bidder data that was set in the exchange's
    --   cookie. The string must be in base85 cookie safe characters and be in
    --   any format. Proper JSON encoding must be used to include "escaped"
    --   quotation marks.
  , customData :: Maybe Text

    -- | Location of the user's home based defined by a `Geo` object (Section
    --   3.2.12). This is not necessarily their current location.
  , geo :: Maybe Geo

    -- | Additional user data. Each `Data` object (Section 3.2.14) represents a
    --   different data source.
  , userData :: Maybe [Data]

    -- | Placeholder for exchange-specific extensions to OpenRTB.
  , ext :: Maybe Value
  }

data Gender = Male | Female | Other
