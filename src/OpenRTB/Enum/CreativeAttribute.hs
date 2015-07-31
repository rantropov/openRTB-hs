module OpenRTB.Enum.CreativeAttribute where

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
