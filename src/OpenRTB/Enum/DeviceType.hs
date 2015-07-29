module OpenRTB.Enum.DeviceType where

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
