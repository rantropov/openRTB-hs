module OpenRTB.Types.Enum.ContentContext where

import Control.Monad
import Data.Aeson
import Data.Scientific

-- | The following table lists the various options for indicating the type of
--   content in which the impression will appear.
--
--   This OpenRTB table has values derived from the IAB Quality Assurance
--   Guidelines (QAG). Practitioners should keep in sync with updates
--   to the QAG values as published on IAB.net.
data ContentContext =
    -- | Video (a video file or stream that is being watched by the user,
    --   including (internet) television broadcasts)
    Video

    -- | Game (an interactive software game that is being played by the user)
  | Game

    -- | Music (an audio file or stream that is being listened to by the user,
    --   including (internet) radio broadcasts)
  | Music

    -- | Application (an interactive software application that is being used
    --   by the user
  | Application

    -- | Text (a document that is primarily textual in nature that is being read
    --   or viewed by the user, including web page, eBook, or news article)
  | Text

    -- | Other (content type unknown or that user is consumer content which does
    --   not fit into one of the categories above)
  | Other

    -- | Unknown
  | Unknown
  deriving (Show, Eq)

instance Enum ContentContext where
  toEnum 1 = Video
  toEnum 2 = Game
  toEnum 3 = Music
  toEnum 4 = Application
  toEnum 5 = Text
  toEnum 6 = Other
  toEnum 7 = Unknown

  fromEnum Video = 1
  fromEnum Game = 2
  fromEnum Music = 3
  fromEnum Application = 4
  fromEnum Text = 5
  fromEnum Other = 6
  fromEnum Unknown = 7

instance FromJSON ContentContext where
  parseJSON (Number i) =
    case floatingOrInteger i of
     Right n | 1 <= n && n <= 7 -> return (toEnum n)
     _ -> mzero

instance ToJSON ContentContext where
  toJSON cc = Number (scientific (fromIntegral $ fromEnum cc) 0)
