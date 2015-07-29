module OpenRTB.Enum.VideoStartDelay where

-- | The following table lists the various options for the video start delay.
--   If the start delay Value is greater than 0, then the position is mid-roll
--   and the value indicates the start delay.
data VideoStartDelay =
    -- | > 0 Mid-Roll (value indicates start delay in seconds)
    MidRoll Integer

    -- | 0 Pre-Roll
  | PreRoll

    -- | -1 Generic Mid-Roll
  | GenericMidRoll

    -- | -2 Generic Post-Roll
  | GenericPostRoll
