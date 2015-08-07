module OpenRTB.Types.Enum.BannerAdType where

import Control.Monad
import Data.Aeson
import Data.Scientific

data BannerAdType =
    -- | XHTML Text Ad (usually mobile)
    XHTMLTextAd

    -- | XHTML Banner Ad (usually mobile)
  | XHTMLBannerAd

    -- | JavaScript Ad; must be valid XHTML (i.e., Script Tags Included)
  | JavaScriptAd

    -- | iframe
  | IFrame
  deriving (Show, Eq)

instance Enum BannerAdType where
  toEnum 1 = XHTMLTextAd
  toEnum 2 = XHTMLBannerAd
  toEnum 3 = JavaScriptAd
  toEnum 4 = IFrame

  fromEnum XHTMLTextAd = 1
  fromEnum XHTMLBannerAd = 2
  fromEnum JavaScriptAd = 3
  fromEnum IFrame = 4

instance FromJSON BannerAdType where
  parseJSON (Number i) =
    case floatingOrInteger i of
      Right n | 1 <= n && n <= 4 -> return (toEnum n)
      _ -> mzero

instance ToJSON BannerAdType where
  toJSON bat = Number (scientific (fromIntegral $ fromEnum bat) 0)
