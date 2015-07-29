module OpenRTB.Enum.ContentContext where

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
