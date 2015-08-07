module OpenRTB.Types.Enum.VideoBidResponseProtocol where

import Control.Monad
import Data.Aeson
import Data.Scientific

-- | The following table lists the options for video bid response protocols
--   that could be supported by an exchange.
data VideoBidResponseProtocol =
    -- | Vast 1.0
    Vast10

    -- | Vast 2.0
  | Vast20

    -- | Vast 3.0

  | Vast30

    -- | Vast 1.0 Wrapper
  | Vast10Wrapper

    -- | Vast 2.0 Wrapper
  | Vast20Wrapper

    -- | Vast 3.0 Wrapper
  | Vast30Wrapper
  deriving (Show, Eq)

instance Enum VideoBidResponseProtocol where
  toEnum 1 = Vast10
  toEnum 2 = Vast20
  toEnum 3 = Vast30
  toEnum 4 = Vast10Wrapper
  toEnum 5 = Vast20Wrapper
  toEnum 6 = Vast30Wrapper

  fromEnum Vast10 = 1
  fromEnum Vast20 = 2
  fromEnum Vast30 = 3
  fromEnum Vast10Wrapper = 4
  fromEnum Vast20Wrapper = 5
  fromEnum Vast30Wrapper = 6

instance FromJSON VideoBidResponseProtocol where
  parseJSON (Number i) =
    case floatingOrInteger i of
      Right n | 1 <= n && n <= 6 -> return (toEnum n)
      _ -> mzero

instance ToJSON VideoBidResponseProtocol where
  toJSON vbrp = Number (scientific (fromIntegral $ fromEnum vbrp) 0)
