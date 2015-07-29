module OpenRTB.Enum.BannerAdTypes where

data BannerAdType =
    -- | XHTML Text Ad (usually mobile)
    XHTMLTextAd

    -- | XHTML Banner Ad (usually mobile)
  | XHTMLBannerAd

    -- | JavaScript Ad; must be valid XHTML (i.e., Script Tags Included)
  | JavaScriptAd

    -- | iframe
  | IFrame
