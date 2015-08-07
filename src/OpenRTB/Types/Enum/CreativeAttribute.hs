module OpenRTB.Types.Enum.CreativeAttribute where

import Control.Monad
import Data.Aeson
import Data.Scientific

-- | The collowing table specifies a standard list of creative attributes that
--   can describe an ad being served or serve as restrictions of thereof.
data CreativeAttribute =
    -- | Audio Ad (Auto-Play)
    AudioAdAutoPlay

    -- | Audio Ad (User Initiated)
  | AudioAdUserInitiated

    -- | Expandable (Automatic)
  | ExpandableAuto

    -- | Expandable (User Initiated - Click)
  | ExpandableUserClick

    -- | Expandable (User Initiated - Rollover)
  | ExpandableUserRoll

    -- | In-Banner Video Ad (Auto-Play)
  | InBannerVideoAdAutoPlay

    -- | In-Banner Video Ad (User Initiated)
  | InBannerVideoAdUserInitiated

    -- | Pop (e.g., Over, Under, or Upon Exit)
  | Pop

    -- | Provocative or Suggestive Imagery
  | ProvocativeOrSuggestiveImagery

    -- | Shaky, Flashing, Flickering, Extreme Animation, Smileys
  | SFFEAS

    -- | Surveys
  | Surveys

    -- | Text Only
  | Text

    -- | User Interactive (e.g., Embedded Games)
  | UserInteractive

    -- | Windows Dialog or Alert Style
  | Windows

    -- | Has Audio On/Off Button
  | AudioButton

    -- | Ad Can be Skipped (e.g., Skip Button on Pre-Roll Video)
  | Skippable
  deriving (Show, Eq)

instance Enum CreativeAttribute where
  toEnum 1 = AudioAdAutoPlay
  toEnum 2 = AudioAdUserInitiated
  toEnum 3 = ExpandableAuto
  toEnum 4 = ExpandableUserClick
  toEnum 5 = ExpandableUserRoll
  toEnum 6 = InBannerVideoAdAutoPlay
  toEnum 7 = InBannerVideoAdUserInitiated
  toEnum 8 = Pop
  toEnum 9 = ProvocativeOrSuggestiveImagery
  toEnum 10 = SFFEAS
  toEnum 11 = Surveys
  toEnum 12 = Text
  toEnum 13 = UserInteractive
  toEnum 14 = Windows
  toEnum 15 = AudioButton
  toEnum 16 = Skippable

  fromEnum AudioAdAutoPlay = 1
  fromEnum AudioAdUserInitiated = 2
  fromEnum ExpandableAuto = 3
  fromEnum ExpandableUserClick = 4
  fromEnum ExpandableUserRoll = 5
  fromEnum InBannerVideoAdAutoPlay = 6
  fromEnum InBannerVideoAdUserInitiated = 7
  fromEnum Pop = 8
  fromEnum ProvocativeOrSuggestiveImagery = 9
  fromEnum SFFEAS = 10
  fromEnum Surveys = 11
  fromEnum Text = 12
  fromEnum UserInteractive = 13
  fromEnum Windows = 14
  fromEnum AudioButton = 15
  fromEnum Skippable = 16

instance FromJSON CreativeAttribute where
  parseJSON (Number i) =
    case floatingOrInteger i of
      Right n | 1 <= n && n <= 16 -> return (toEnum n)
      _ -> mzero

instance ToJSON CreativeAttribute where
  toJSON ca = Number (scientific (fromIntegral $ fromEnum ca) 0)
