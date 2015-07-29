module OpenRTB.Enum.QAGMediaRatings where

-- | The following table lists the media ratings used in describing content
--   based on the QAG categorization. Refer to http://www.iab.net/ne_guidelines
--   for more information.
data QAGMediaRating =
    -- | All Audiences
    AllAudiences

    -- | Everyone Over 12
  | EveryoneOver12

    -- | Mature Audiences
  | MatureAudiences
