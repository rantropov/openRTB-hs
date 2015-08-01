module OpenRTB.Enum.ExpandableDirection where

import Prelude as P

import Control.Monad
import Data.Aeson
import Data.Scientific

-- | The following table lists the directions in which an expandable ad may
--   expand, given the positioning of the ad unit on the page and constraints
--   imposed by the content.
data ExpandableDirection =
    Left

  | Right

  | Up

  | Down

  | FullScreen
  deriving (Show, Eq)

instance P.Enum ExpandableDirection where
  toEnum 1 = OpenRTB.Enum.ExpandableDirection.Left
  toEnum 2 = OpenRTB.Enum.ExpandableDirection.Right
  toEnum 3 = Up
  toEnum 4 = Down
  toEnum 5 = FullScreen

  fromEnum OpenRTB.Enum.ExpandableDirection.Left = 1
  fromEnum OpenRTB.Enum.ExpandableDirection.Right = 2
  fromEnum Up = 3
  fromEnum Down = 4
  fromEnum FullScreen = 5

instance FromJSON ExpandableDirection where
  parseJSON (Number i) =
    case floatingOrInteger i of
      P.Right n | 1 <= n && n <= 5 -> return (toEnum n)
      _ -> mzero

instance ToJSON ExpandableDirection where
  toJSON ed = Number (scientific (fromIntegral $ fromEnum ed) 0)
