module OpenRTB.Enum.QAGMediaRatings where

import Control.Monad
import Data.Aeson
import Data.Scientific

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
  deriving (Show, Eq)

instance Enum QAGMediaRating where
  toEnum 1 = AllAudiences
  toEnum 2 = EveryoneOver12
  toEnum 3 = MatureAudiences

  fromEnum AllAudiences = 1
  fromEnum EveryoneOver12 = 2
  fromEnum MatureAudiences = 3

instance FromJSON QAGMediaRating where
  parseJSON (Number i) =
    case floatingOrInteger i of
      Right n | 1 <= n && n <= 3  -> return (toEnum n)
      _ -> mzero

instance ToJSON QAGMediaRating where
  toJSON qag = Number (scientific (fromIntegral $ fromEnum qag) 0)
