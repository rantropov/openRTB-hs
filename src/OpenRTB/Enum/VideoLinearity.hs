module OpenRTB.Enum.VideoLinearity where

import Control.Monad
import Data.Aeson
import Data.Scientific

-- | The following table indicates the options for video linearity. "In-stream"
--   or "linear" video refers to pre-roll, post-roll, or mid-roll video ads
--   where the user is forced to watch ad in order to see the video content.
--   "Overlay" or "non-linear" refer to ads that are shown on top of the video
--   content.
--
--   This field is optional. The following is the interpretation of the bidder
--   based upon the presence or absence of the field in the bid request:
--
--      * If no value is set, any ad (linear or not) can be present in the
--        response.
--
--      * If a value is set, only ads of the corresponding type can be present
--        in the response.
--
--   Note to the reader: This OpenRTB table has values derived from the IAB
--   Quality Assurance Guidelines (QAG). Practitioners should keep in sync
--   with updates to the QAG values as published on IAB.net.
data VideoLinearity =
    -- | Linear / In-Stream
    Linear

    -- | Non-Linear / Overlay
  | NonLinear
  deriving (Show, Eq)

instance Enum VideoLinearity where
  toEnum 1 = Linear
  toEnum 2 = NonLinear

  fromEnum Linear = 1
  fromEnum NonLinear = 2

instance FromJSON VideoLinearity where
  parseJSON (Number i) =
    case floatingOrInteger i of
      Right n | 1 <= n && n <= 2 -> return (toEnum n)
      _ -> mzero

instance ToJSON VideoLinearity where
  toJSON vl = Number (scientific (fromIntegral $ fromEnum vl) 0)
