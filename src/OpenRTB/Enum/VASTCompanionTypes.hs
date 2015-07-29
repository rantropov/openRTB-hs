module OpenRTB.Enum.VASTCompanionTypes where

-- | The following table lists the options to indicate markup types allowed for
--   video companion ads. This table is derived from IAB VAST 2.0+. Refer
--   to www.iab.net/vast/ for more information.
data VASTCompanionType =
    -- | Static Resource
    Static

    -- | HTML Resource
  | HTML

    -- | iframe Resource
  | IFrame
