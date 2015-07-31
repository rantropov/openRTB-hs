module OpenRTB.BidRequest.Imp.Native where

import Data.Aeson
import Data.Text

import OpenRTB.Enum.APIFramework
import OpenRTB.Enum.CreativeAttribute

-- | The object represents a native type impression. Native ad units are
--   intended to blend seamlessly into the surrounding content (e.g., a
--   sponsored Twitter or Facebook post). As such, the response must be
--   well-structured to afford the publisher fine-grained control over
--   rendering.
--
--   The Native Subcommittee has developed a companion specification to OpenRTB
--   called the Native Ad Specification. It defined the request parameters and
--   markup structure of native ad units. This object provides the means of
--   transporting request parameters as an opaque string so that the specific
--   parameters can evolve separately under the auspices of the Native Ad
--   Specification. Similarly, the ad markup served will be structured
--   according to that specification.
--
--   The presence of a `Native` as a subordinate of the `Imp` object indicates
--   that this impression is offered as a native type impression. At the
--   publisher's discretion, that same impression may also be offered as
--   banner and/or video by also including as `Imp` subordinates the `Banner`
--   and/or `Video` objects, respectively. However, any given bid for the
--   impression must conform to one of the offered types.
data Native = Native
  {
    -- | Request payload complying with the Native Ad Specification.
    request :: Text

    -- | Version of the Native Ad Specification to which `request` complies;
    --   highly recommended for efficient parsing.
  , ver :: Maybe Text

    -- | List of supported API frameworks for this impression. Refer to List
    --   5.6. If an API is not explicitly listed, it is assumed not to be
    --   supported.
  , api :: [APIFramework]

    -- | Blocked creative attributes. Refer to List 5.3.
  , bAttr :: [CreativeAttribute]

    -- | Placeholder for exchange-specific extensions to OpenRTB.
  , ext :: Maybe Value
  }
