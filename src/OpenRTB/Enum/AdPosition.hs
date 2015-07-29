module OpenRTB.Enum.AdPosition where

-- | The following table specifies the position of the ad as a relative measure
--   of visibility or prominence. This OpenRTB table has values derived from
--   the IAB Quality Assurance Guidelines (QAG). Practitioners should keep in
--   sync with updates to the QAG values as published on IAB.net. Values
--   "3" - "6" apply to apps per the mobile addendum to QAG version 1.5.
data AdPosition =
    Unknown

  | AboveTheFold

    -- | May or may not be initially visible depending on screen
    --   size/resolution.
  | DEPRECATED

  | BelowTheFold

  | Header

  | Footer

  | Sidebar

  | FullScreen
