module OpenRTB.Types.BidRequest.Site where

import Prelude hiding (id)

import Data.Aeson
import Data.Text

import OpenRTB.Types.BidRequest.DistributionChannel.Publisher
import OpenRTB.Types.BidRequest.DistributionChannel.Content

-- | This object should be included if the ad supported content is a website as
--   opposed to a non-browser application. A bid request must not contain both
--   a `Site` and an `App` object. At a minimum, it is useful to provide a site
--   ID or page URL, but this is not strictly required.
data Site = Site
  {
    -- | Exchange-specific site ID.
    id :: Maybe Text

    -- | Site name (may be aliased at the publisher's request).
  , name :: Maybe Text

    -- | Domain of the site (e.g., "mysite.foo.com").
  , domain :: Maybe Text

    -- | Array of IAB content categories of the site. Refer to List 5.1.
  , cat :: Maybe [()]

    -- | Array of IAB content categories that describe the current section of
    --   the site. Refer to list 5.1.
  , sectionCat :: Maybe [()]

    -- | Array of IAB content categories that describe the current page or view
    --   of the site. Refer to List 5.1.
  , pageCat :: Maybe [()]

    -- | URL of the page where the impression will be shown.
  , page :: Maybe Text

    -- | Referrer URL that caused navigation to the current page.
  , ref :: Maybe Text

    -- | Search string that caused navigation to the current page.
  , search :: Maybe Text

    -- | Mobile-optimized signal, where 0 = no, 1 = yes.
  , mobile :: Maybe Bool

    -- | Indicates if the site has a privacy policy, where 0 = no, 1 = yes.
  , privacyPolicy :: Maybe Bool

    -- | Details about the Publisher (Section 3.2.8) of the site.
  , publisher :: Maybe Publisher

    -- | Details about the Content (Section 3.2.9) within the site.
  , content :: Maybe Content

    -- | Comma separated list of keywords about the site.
  , keywords :: Maybe Text

    -- | Placeholder for exchange-specific extensions to OpenRTB.
  , ext :: Maybe Site
  }
