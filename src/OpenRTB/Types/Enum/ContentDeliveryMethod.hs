module OpenRTB.Types.Enum.ContentDeliveryMethod where

import Control.Monad
import Data.Aeson
import Data.Scientific

-- | The following table lists the various options for the delivery of video
--   content.
data ContentDeliveryMethod =
    -- | Streaming
    Streaming

    -- | Progressive
  | Progressive
  deriving (Show, Eq)

instance Enum ContentDeliveryMethod where
  toEnum 1 = Streaming
  toEnum 2 = Progressive

  fromEnum Streaming = 1
  fromEnum Progressive = 2

instance FromJSON ContentDeliveryMethod where
  parseJSON (Number i) =
    case floatingOrInteger i of
      Right n | 1 <= n && n <= 2 -> return (toEnum n)
      _ -> mzero

instance ToJSON ContentDeliveryMethod where
  toJSON cdm = Number (scientific (fromIntegral $ fromEnum cdm) 0)
