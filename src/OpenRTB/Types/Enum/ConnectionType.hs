module OpenRTB.Types.Enum.ConnectionType where

import Control.Monad
import Data.Aeson
import Data.Scientific

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
  deriving (Enum, Show, Eq)

instance FromJSON ConnectionType where
  parseJSON (Number i) =
    case floatingOrInteger i of
      Right n | 0 <= n && n <= 6  -> return (toEnum n)
      _ -> mzero

instance ToJSON ConnectionType where
  toJSON ct = Number (scientific (fromIntegral $ fromEnum ct) 0)
