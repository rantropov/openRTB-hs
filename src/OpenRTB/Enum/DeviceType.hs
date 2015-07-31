module OpenRTB.Enum.DeviceType where

import Control.Monad
import Data.Aeson
import Data.Scientific

-- | The following table lists the type of device from which the impression
--   originated.
--
--   OpenRTB version 2.2 of the specification added distinct values for Mobile
--   and Tablet. It is recommended that any bidder adding support for 2.2 treat
--   a value of 1 as an acceptable alias of 4 & 5.
--
--   This OpenRTB table has values derived from the IAB Quality Assurance
--   Guidelines (QAG). Practitioners should keep in sync with updates to the
--   QAG values as published on IAB.net
data DeviceType =
    -- | Mobile/Tablet
    --
    --   Version 2.0
    MobileTablet

    -- | Personal Computer
    --
    --   Version 2.0
  | PersonalComputer

    -- | Connected TV
    --
    --   Version 2.0
  | ConnectedTV

    -- | Phone
    --
    --   Version 2.2
  | Phone

    -- | Tablet
    --
    --   Version 2.2
  | Tablet

    -- | Connected Device
    --
    --   Version 2.2
  | ConnectedDevice

    -- | Set Top Box
    --
    --   Version 2.2
  | SetTopBox
  deriving (Show, Eq)

instance Enum DeviceType where
  toEnum 1 = MobileTablet
  toEnum 2 = PersonalComputer
  toEnum 3 = ConnectedTV
  toEnum 4 = Phone
  toEnum 5 = Tablet
  toEnum 6 = ConnectedDevice
  toEnum 7 = SetTopBox

  fromEnum MobileTablet = 1
  fromEnum PersonalComputer = 2
  fromEnum ConnectedTV = 3
  fromEnum Phone = 4
  fromEnum Tablet = 5
  fromEnum ConnectedDevice = 6
  fromEnum SetTopBox = 7

instance FromJSON DeviceType where
  parseJSON (Number i) =
    case floatingOrInteger i of
      Right n | 0 < n && n <= 7 -> return (toEnum n)
      _ -> mzero

instance ToJSON DeviceType where
  toJSON dt = Number (scientific (fromIntegral $ fromEnum dt) 0)
