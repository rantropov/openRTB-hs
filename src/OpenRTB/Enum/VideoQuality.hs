module OpenRTB.Enum.VideoQuality where

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
