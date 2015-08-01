module OpenRTB.Enum.VideoStartDelay where

import Control.Monad
import Data.Aeson
import Data.Scientific
import Data.Word

-- | The following table lists the various options for the video start delay.
--   If the start delay Value is greater than 0, then the position is mid-roll
--   and the value indicates the start delay.
data VideoStartDelay =
    -- | > 0 Mid-Roll (value indicates start delay in seconds)
    MidRoll Word16

    -- | 0 Pre-Roll
  | PreRoll

    -- | -1 Generic Mid-Roll
  | GenericMidRoll

    -- | -2 Generic Post-Roll
  | GenericPostRoll
  deriving (Show, Eq)

instance Enum VideoStartDelay where
  toEnum n | 0 < n = (MidRoll . fromIntegral) n
  toEnum 0 = PreRoll
  toEnum (-1) = GenericMidRoll
  toEnum (-2) = GenericPostRoll

  fromEnum (MidRoll n) = fromIntegral n
  fromEnum PreRoll = 0
  fromEnum GenericMidRoll = -1
  fromEnum GenericPostRoll = -2

instance FromJSON VideoStartDelay where
  parseJSON (Number i) =
    case floatingOrInteger i of
      Right n | (-2) <= n -> return (toEnum n)
      _ -> mzero

instance ToJSON VideoStartDelay where
  toJSON vsd = Number (scientific (fromIntegral $ fromEnum vsd) 0)
