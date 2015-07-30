module OpenRTB.Enum.ConnectionType where

-- | The following table lists the various options for the type of device
--   connectivity.
data ConnectionType =
    -- | Unknown
    Unknown

    -- | Ethernet
  | Ethernet

    -- | WIFI
  | WIFI

    -- | Cellular Network - Unknown Generation
  | CNUnknown

    -- | Cellular Network - 2G
  | CN2G

    -- | Cellular Network - 3G
  | CN3G

    -- | Cellular Network - 4G
  | CN4G
