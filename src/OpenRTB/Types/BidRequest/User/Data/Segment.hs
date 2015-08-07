module OpenRTB.Types.BidRequest.User.Data.Segment where

import Prelude hiding (id)

import Data.Aeson
import Data.Text

-- | Segment objects are essentially key-value pairs that convey specific units
--   of data about the user. The parent `Data` object is a collection of such
--   values from a given data provider. The specific segment names and value
--   options must be published by the exchange *a priori* to its bidders.
data Segment = Segment
  {
    -- | ID of the data segment specific to the data provider.
    id :: Maybe Text

    -- | Name of the data segment specific to the data provider.
  , name :: Maybe Text

    -- | String representation of the data segment value.
  , value :: Maybe Text

    -- | Placeholder for exchange-specific extensions to OpenRTB.
  , ext :: Maybe Value
  }
