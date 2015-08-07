module OpenRTB.Types.BidRequest.Geo where

import Data.Aeson
import Data.Text
import Data.Word

import OpenRTB.Types.Enum.LocationType

-- | This object encapsulates various methods for specifying a geographic
--   location. When subordinate to a `Device` object, it indicates the location
--   of the device which can also be interpreted as the user's current location.
--   When subordinate to a `User` object, it indicates the location of the
--   user's home base (i.e., not necessarily their current location).
--
--   The `lat`/`lon` attributes should only be passed if they conform to the
--   accuracy depicted in the `type` attribute. For example, the centroid of
--   a geographic region such as postal code should not be passed.
data Geo = Geo
  {
    -- | Latitude from -90.0 to +90.0, where negative is south.
    lat :: Maybe Double

    -- | Longitude from -180.0 to +180.0, where negative is west.
  , lon :: Maybe Double

    -- | Source of location data; recommended when passing `lat`/`lon`. Refer
    --   to List 5.16.
  , source :: Maybe LocationType

    -- | Country code using ISO-3166-1-alpha-3.
  , country :: Maybe Text

    -- | Region code using ISO-3166-2; 2-letter state code if USA
  , region :: Maybe Text

    -- | Region of a country using FIPS 10-4 notation. While OpenRTB supports
    --   this attribute, it has been withdrawn by NIST in 2008.
  , regionFIPS104 :: Maybe Text

    -- | Google metro code; similar to but not exactly Nielsen DMAs. See
    --   Appendix A for a link to the codes.
  , metro :: Maybe Text

    -- | City using United Nations Code for Trade & Transport Locations.
    --   See Appendix A for a link to the codes.
  , city :: Maybe Text

    -- | Zip or Postal
  , zip :: Maybe Text

    -- | Local device time as the number +/- of minutes from UTC.
  , utcOffset :: Maybe Word16

    -- | Placeholder for exchange-specific extensions to OpenRTB.
  , ext :: Maybe Value
  }
