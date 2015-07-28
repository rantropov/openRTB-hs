{-# LANGUAGE OverloadedStrings #-}
module OpenRTB.BidRequest.Imp where

import Prelude hiding (id)

import Control.Applicative
import Data.Aeson
import Data.Text

import OpenRTB.BidRequest.Imp.Banner
import OpenRTB.BidRequest.Imp.Native
import OpenRTB.BidRequest.Imp.Pmp
import OpenRTB.BidRequest.Imp.Video

-- | The object describes an ad placement or impression being auctioned. A
--   single bid request can include multiple `Imp` objects, a use case for which
--   might be an exchange that supports selling all ad positions on a given
--   page. Each `Imp` object has a required ID so that bids can reference them
--   individually.
--
--   The presence of `Banner` (Section 3.2.3), `Video` (Section 3.2.4), and/or
--   `Native` (Section 3.2.5) objects subordinate to the `Imp` object indicates
--   the type of impression being offered. The publisher can choose one such
--   type which is the typical case or mix them at their discretion. However,
--   any given bid for the impression must conform to one of the offered types.
data Imp = Imp
  {
    -- | A unique identifier for this impression within the context of the bid
    --   request (typically, starts with 1 and increments).
    id :: Text

    -- | A `Banner` object (Section 3.2.3); required if this impression is
    --   offered as a banner ad opportunity.
  , banner :: Maybe Banner

    -- | A `Video` object (Section 3.2.4); required if this impressions is
    --   offered as a video ad opportunity.
  , video :: Maybe Video

    -- | A `Native` object (Section 3.2.5); required if this impression is
    --   offered as a native ad opportunity.
  , native :: Maybe Native

    -- | Name of ad mediation partner, SDK technology, or player responsible for
    --   rendering ad (typically video or mobile). Used by some ad servers to
    --   customize ad code by partner.
    --
    --   Recommended for video and/or apps.
  , displayManager :: Maybe Text

    -- | Version of ad mediation partner, SDK technology, or player responsible
    --   for rendering ad (typically video or mobile). Used by some ad servers
    --   to customize ad code by partner.
    --
    --   Recommended for ideo and/or apps.
  , displaymanagerVer :: Maybe Text

    -- | 1 = the ad is interstitial or full screen, 0 = not interstitial.
  , instl :: Bool

    -- | Identifier for specific ad placement or ad tag that was used to
    --   initiate the auction. This can be useful for debugging any issues, or
    --   for optimization of the buyer.
  , tagID :: Maybe Text

    -- | Minimum bid for this impression expressed in CPM.
  , bidFloor :: Double

    -- | Currency specified using ISO-4217 alpha codes. This may be different
    --   from bid currency returned by bidder if this is allowed by the
    --   exchange
  , bidFloorCur :: Text

    -- | Flag to indicate if the impression required secure HTTPS URL creative
    --   assets and markup, where 0 = non-secure, 1 = secure. If omitted, the
    --   secure state is unknown, but non-secure HTTP support can be assumed.
  , secure :: Bool

    -- | Array of exchange-specific names of supported iframe busters.
  , iFrameBuster :: Maybe [Text]

    -- | A `Pmp` object (SEction 3.2.17) containing any private marketplace
    --   deals in effect for this impression.
  , pmp :: Maybe Pmp

    -- | Placeholder for exchange-specific extensions to OpenRTB.
  , ext :: Maybe Value
  }
