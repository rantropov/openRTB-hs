module OpenRTB.Enum.APIFramework where

import Control.Monad
import Data.Aeson
import Data.Scientific

-- | The following table is a list of API frameworks supported by the publisher.
--   Note that MRAID-1 is a subset of MRAID-2. In OpenRTB 2.1 and prior,
--   value "3" was "MRAID". However, not all MRAID capable APIs understand
--   MRAID-2 features and as such the only safe interpretation of value "3"
--   is MRAID-1. In OpenRTB 2.2, this was made explicit and MRAID-2 has been
--   added as value "5".
data APIFramework =
    VPAID10

  | VPAID20

  | MRAID1

  | ORMMA

  | MRAID2
  deriving (Show, Eq)

instance Enum APIFramework where
  toEnum 1 = VPAID10
  toEnum 2 = VPAID20
  toEnum 3 = MRAID1
  toEnum 4 = ORMMA
  toEnum 5 = MRAID2

  fromEnum VPAID10 = 1
  fromEnum VPAID20 = 2
  fromEnum MRAID1 = 3
  fromEnum ORMMA = 4
  fromEnum MRAID2 = 5

instance FromJSON APIFramework where
  parseJSON (Number i) =
    case floatingOrInteger i of
      Right n | 1 <= n && n <= 5 -> return (toEnum n)
      _ -> mzero

instance ToJSON APIFramework where
  toJSON api = Number (scientific (fromIntegral $ fromEnum api) 0)
