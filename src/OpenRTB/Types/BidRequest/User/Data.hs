module OpenRTB.Types.BidRequest.User.Data where

import Prelude hiding (id)

import Data.Aeson
import Data.Text

import OpenRTB.Types.BidRequest.User.Data.Segment

-- | The data and segment objects together allow additional data about the user
--   to be specified. This data may be from multiple sources whether from the
--   exchange itself or third party providers as specified by the `id` field.
--   A bid request can mix data objects from multiple providers. The specific
--   data providers in user should be published by the exchange *a priori*
--   to its bidders.
data Data = Data
  {
    -- | Exchange specific ID for the data provider.
    id :: Maybe Text

    -- | Exchange-specific name for the data provider.
  , name :: Maybe Text

    -- | Array of `Segment` (Section 3.2.15) objects that contain the actual
    --   data values.
  , segment :: Maybe [Segment]

    -- | Placeholder for exchange-specific extensions to OpenRTB.
  , ext :: Maybe Value
  }
