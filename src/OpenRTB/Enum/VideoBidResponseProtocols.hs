module OpenRTB.Enum.VideoBidResponseProtocols where

-- | The following table lists the options for video bid response protocols
--   that could be supported by an exchange.
data VideoBidResponseProtocols =
    -- | Vast 1.0
    Vast10

    -- | Vast 2.0
  | Vast20

    -- | Vast 3.0

  | Vast30

    -- | Vast 1.0 Wrapper
  | Vast10Wrapper

    -- | Vast 2.0 Wrapper
  | Vast20Wrapper

    -- | Vast 3.0 Wrapper
  | Vast30Wrapper
