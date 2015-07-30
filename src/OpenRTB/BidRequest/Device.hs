module OpenRTB.BidRequest.Device where

import Data.Aeson
import Data.Text
import Data.Word

import OpenRTB.BidRequest.Geo
import OpenRTB.Enum.ConnectionType
import OpenRTB.Enum.DeviceType

-- | This object provides information pertaining to the device through which the
--   user is interacting. Device information includes its hardware, platform,
--   location, and carrier data. The device can refer to a mobile handset, a
--   desktop computer, set top box, or other digital device.
data Device = Device
  {
    -- | Browser user agent string.
    ua :: Maybe Text

    -- | Location of the device assumed to be the user's current location
    --   defined by a `Geo` object (Section 3.2.12).
  , geo :: Maybe Geo

    -- | Standard "Do Not Track" flag as set in the header by the browser,
    --   where 0 = tracking is unrestricted, 1 = do not track.
  , dnt :: Maybe Bool

    -- | "Limit Ad Tracking" signal commercially endorsed (e.g., iOS, Android),
    --   where 0 = tracking is unrestricted, 1 = tracking must be limited per
    --   commercial guidelines.
  , lmt :: Maybe Bool

    -- | IPv4 address closest to device.
  , ip :: Maybe Text

    -- | IP address closest to device as IPv6.
  , ipV6 :: Maybe Text

    -- | The general type of device. Refer to List 5.17.
  , deviceType :: Maybe DeviceType

    -- | Device make (e.g., "Apple").
  , make :: Maybe Text

    -- | Device model (e.g., "iPhone").
  , model :: Maybe Text

    -- | Device operating system (e.g., "iOS").
  , os :: Maybe Text

    -- | Device operating system version (e.g., "3.1.2").
  , osV :: Maybe Text

    -- | Hardware version of the device (e.g., "5S" for iPhone 5S).
  , hwV :: Maybe Text

    -- | Physical height of the screen in pixels.
  , h :: Maybe Word16

    -- | Physical width of the screen in pixels.
  , w :: Maybe Word16

    -- | Screen size as pixels per linear inc
  , ppi :: Maybe Word16

    -- | The ratio of physical pixels to device independent pixels.
  , pxRatio :: Maybe Word16

    -- | Support for JavaScript, where 0 = no, 1 = yes.
  , js :: Maybe Bool

    -- | Version of Flash supported by the browser.
  , flashVer :: Maybe Text

    -- | Browser language using ISO-639-1-alpha-2.
  , language :: Maybe Text

    -- | Carrier or ISP (e.g., "VERIZON"). "WIFI" is often used in mobile to
    --   indicate high bandwidth (e.g., video friendly vs cellular).
  , carrier :: Maybe Text

    -- | Network connection type. Refer to List 5.18.
  , connectionType :: Maybe ConnectionType

    -- | ID sanctioned for advertiser use in the clear (i.e., not hashed).
  , ifA :: Maybe Text

    -- | Hardware device ID (e.g., IMEI); hashed via SHA1.
  , didSHA1 :: Maybe Text

    -- | Hardware device ID (e.g., IMEI); hash via MD5
  , didMD5 ::  Maybe Text

    -- | Platform device ID (e.g., Android ID); hash via SHA1
  , dpidSHA1 :: Maybe Text

    -- | Platform device ID (e.g., Android ID); hash via MD5
  , dpidMD5 :: Maybe Text

    -- | MAC address of the device; hash via SHA1
  , macSHA1 :: Maybe Text

    -- | MAC address of the devicel hashed via MD5
  , macMD5 :: Maybe Text

    -- | Placeholder for exchange-specific extensions to OpenRTB.
  , ext :: Maybe Value
  }
