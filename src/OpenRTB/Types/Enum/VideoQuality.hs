module OpenRTB.Types.Enum.VideoQuality where

import Control.Monad
import Data.Aeson
import Data.Scientific

-- | The following table lists the options for the video quality. These values
--   are defined by the IAB -
--   http://www.iab.net/media/file/long-form-video-final.pdf.

data VideoQuality =
    -- | Unknown
    Unknown

    -- | Professionally Produced
  | ProfessionallyProduced

    -- | Prosumer
  | Prosumer

    -- | User Generated (UGC)
  | UserGenerated
  deriving (Enum, Show, Eq)

instance FromJSON VideoQuality where
  parseJSON (Number i) =
    case floatingOrInteger i of
      Right n | 0 <= n && n <= 3 -> return (toEnum n)
      _ -> mzero

instance ToJSON VideoQuality where
  toJSON vq = Number (scientific (fromIntegral $ fromEnum vq) 0)
