module OpenRTB.BidRequest.Imp.Banner where

import Prelude hiding (id)

import Control.Applicative
import Data.Aeson
import Data.Text
import Data.Word

import OpenRTB.Enum.AdPosition
import OpenRTB.Enum.APIFrameworks
import OpenRTB.Enum.BannerAdTypes
import OpenRTB.Enum.CreativeAttributes
import OpenRTB.Enum.ExpandableDirection

-- | This object represents the most general type of impression. Although the
--   term "banner" may have very specific meaning in other contexts, here it can
--   be many things including a simple static image, an expandable ad unit, or
--   even in-banner video (refer to the `Video` object in Section 3.2.4 for the
--   more generalized and full featured video ad units). An array of `Banner`
--   objects can also appear within the `Video` to describe optional companion
--   ads defined in the VAST specification.
--
--   The presence of a `Banner` as a subordinate of the `Imp` object indicates
--   that this impression is offered as a banner type impression. At the
--   publishers discretion, that same impression may also be offered as video
--   ad/or native by also including as `Imp` subordinates the `Video` and/or
--   `Native` objects, respectively. However, any given bid for the impression
--   must conform to one of the offered types.
data Banner = Banner
  {
    -- | Width of the impression in pixels.
    --
    --   if neither `wMin` nor `wMax` are specified, this value is an exact
    --   width requirement. Otherwise it is a preferred width.
    w :: Maybe Word16

    -- | Height of the impression in pixels.
    --
    --   If neither `hMin` not `hMax` are specified, this value is an exact
    --   height requirement. Otherwise it is a preferred height.
  , h :: Maybe Word16

    -- | Maximum width of the impression in pixels.
    --
    --   If included along with a `w` value then `w` should be interpreted as
    --   a recommended or preferred width.
  , wMax :: Maybe Word16

    -- | Maximum height of the impression in pixels.
    --
    --   If included along with an `h` value then `h` should be interpreted as
    --   a recommended of preferred height.
  , hMax :: Maybe Word16

    -- | Minimum width of the impression in pixels.
    --
    --   If included along with a `w` value then `w` should be interpreted as
    --   a recommended or preferred width.
  , wMin :: Maybe Word16

    -- | Minimum height of the impression in pixels.
    --
    --   If included along with an `h` value then `h` should be interpreted as
    --   a recommended or preferred height.
  , hMin :: Maybe Word16

    -- | Unique identifier for this banner object. Recommended when `Banner`
    --   objects are used with a `Video` object (Section 3.2.4) to represent
    --   an array of companion ads. Values usually start at 1 and increase
    --   with each objectl should be unique within an impression.
  , id :: Maybe Text

    -- | Blocked banner ad types. Refer to List 5.2.
  , bType :: [BannerAdType]

    -- | Blocked creative attributes. Refer to List 5.3.
  , bAttr :: [CreativeAttribute]

    -- | Ad position on Screen. Refer to List 5.4.
  , pos :: Maybe AdPosition

    -- | Content MIME types supported. Popular MIME types may include
    --   "application/x-shockwave-flash", "image/jpg", "image/gif".
  , mimes :: Maybe [Text]

    -- | Indicates if the baner is in the top frame as opposed to an iframe,
    --   where 0 = no, 1 = yes.
  , topFrame :: Maybe Bool

    -- | Directions in which the baner may expand. Refer to List 5.5.
  , expDir :: Maybe [ExpandableDirection]

    -- | List of supported API frameworks for this impression. Refer to List
    --   5.6. If an API is not explicitly listed, it is assumed not to be
    --   supported.
  , api :: [APIFramework]

    -- | Placeholder for exchange-specific extensions to OpenRTB.
  , ext :: Maybe Value
  }
