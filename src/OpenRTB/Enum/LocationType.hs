module OpenRTB.Enum.LocationType where

import Control.Monad
import Data.Aeson
import Data.Scientific

-- | The following table lists the options to indicate how the geographic
--   information was determined.
data LocationType =
    -- | GPS/Location Services
    GPS

    -- | IP Address
  | IP

    -- | User provided (e.g., registration data)
  | UserProvided
  deriving (Show, Eq)

instance Enum LocationType where
  toEnum 1 = GPS
  toEnum 2 = IP
  toEnum 3 = UserProvided

  fromEnum GPS = 1
  fromEnum IP = 2
  fromEnum UserProvided = 3

instance FromJSON LocationType where
  parseJSON (Number i) =
    case floatingOrInteger i of
      Right n | 1 <= n && n <= 3 -> return (toEnum n)
      _ -> mzero

instance ToJSON LocationType where
  toJSON lt = Number (scientific (fromIntegral $ fromEnum lt) 0)
