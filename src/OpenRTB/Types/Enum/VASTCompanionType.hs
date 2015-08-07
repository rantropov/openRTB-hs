module OpenRTB.Types.Enum.VASTCompanionType where

import Control.Monad
import Data.Aeson
import Data.Scientific

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
  deriving (Show, Eq)

instance Enum VASTCompanionType where
  toEnum 1 = Static
  toEnum 2 = HTML
  toEnum 3 = IFrame

  fromEnum Static = 1
  fromEnum HTML = 2
  fromEnum IFrame = 3

instance FromJSON VASTCompanionType where
  parseJSON (Number i) =
    case floatingOrInteger i of
      Right n | 1 <= n && n <= 3 -> return (toEnum n)
      _ -> mzero

instance ToJSON VASTCompanionType where
  toJSON vct = Number (scientific (fromIntegral $ fromEnum vct) 0)
