module OpenRTB.Types.BidRequest.App where

import Prelude hiding (id)

import Data.Aeson
import Data.Text

import OpenRTB.Types.BidRequest.DistributionChannel.Publisher
import OpenRTB.Types.BidRequest.DistributionChannel.Content

-- | This object should be included if the ad supported content is a non-browser
--   application (typically in mobile) as opposed to a website. A bid request
--   must not contain both an `App` and a `Site` object. At a minimum, it is
--   useful to provide an App ID or bundle, but this is not strictly required.
data App = App
  {
    -- | Exchange-specific app ID.
    id :: Maybe Text

    -- | App name (may be aliased at the publisher's request).
  , name :: Maybe Text

    -- | Application bundle or package name (e.g., com.foo.mygame); intended to
    --   be a unique ID across exchanges.
  , bundle :: Maybe Text

    -- | Domain of the app (e.g., "mygame.foo.com").
  , domain :: Maybe Text

    -- | App store URL for an installed app; for QAG 1.5 compliance.
  , storeURL :: Maybe Text

    -- | Array of IAB content categories of the app. Refer to List 5.1.
  , cat :: Maybe [()]

    -- | Array of IAB content categories that describe the current section
    --   of the app. Refer to list 5.1.
  , sectionCat :: Maybe [()]

    -- | Array of IAB content categories that describe the current page
    --   or view of the app. Refer to List 5.1.
  , pagecat :: Maybe [()]

    -- | Application version.
  , ver :: Maybe Text

    -- | Indicates if the app has a privacy policy, where 0 = no, 1 = yes.
  , privacyPolicy :: Maybe Bool

    -- | 0 = app is free, 1 = the app is a paid version.
  , paid :: Maybe Bool

    -- | Details about the publisher (Section 3.2.8) of the app.
  , publisher :: Maybe Publisher

    -- | Details about the Content (Section 3.2.9) within the app.
  , content :: Maybe Content

    -- | Comma separated list of keywords about the app.
  , keywords :: Maybe Text

    -- | Placeholder for exchange-specific extensions to OpenRTB.
  , ext :: Maybe Value
  }
