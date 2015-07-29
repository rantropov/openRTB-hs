module OpenRTB.Enum.ExpandableDirection where

-- | The following table lists the directions in which an expandable ad may
--   expand, given the positioning of the ad unit on the page and constraints
--   imposed by the content.
data ExpandableDirection =
    Left

  | Right

  | Up

  | Down

  | FullScreen
