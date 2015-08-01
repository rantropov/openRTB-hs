module OpenRTB.Enum.VideoPlaybackMethod where

import Control.Monad
import Data.Aeson
import Data.Scientific

-- | The following table lists the various video playback methods.
data VideoPlaybackMethod =
    -- | Auto-Play Sound On
    AutoPlaySoundOn

    -- | Auto-Play Sound Off
  | AutoPlaySoundOff

    -- | Click-to-Play
  | ClickToPlay

    -- | Mouse-Over
  | MouseOver
  deriving (Show, Eq)

instance Enum VideoPlaybackMethod where
  toEnum 1 = AutoPlaySoundOn
  toEnum 2 = AutoPlaySoundOff
  toEnum 3 = ClickToPlay
  toEnum 4 = MouseOver

  fromEnum AutoPlaySoundOn = 1
  fromEnum AutoPlaySoundOff = 2
  fromEnum ClickToPlay = 3
  fromEnum MouseOver = 4

instance FromJSON VideoPlaybackMethod where
  parseJSON (Number i) =
    case floatingOrInteger i of
      Right n | 1 <= n && n <= 4 -> return (toEnum n)
      _ -> mzero

instance ToJSON VideoPlaybackMethod where
  toJSON vpm = Number (scientific (fromIntegral $ fromEnum vpm) 0)
