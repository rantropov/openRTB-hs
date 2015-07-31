{-# LANGUAGE OverloadedStrings #-}
module OpenRTB.Enum.NoBidReasonCode where

import Control.Monad
import Data.Aeson
import Data.Scientific

-- | The following table lists the options for a bidder to signal the exchange
--   as to why it did not offer a bid for the impression.
data NoBidReasonCode =
    -- | Unknown Error
    Unknown

    -- | Technical Error
  | Technical

    -- | Invalid Request
  | Invalid

    -- | Known Web Spider
  | Spider

    -- | Suspected Non-Human Traffic
  | Fraud

    -- | Cloud, Data center, or Proxy IP
  | BadIP

    -- | Unsupported Device
  | UnsupportedDevice

    -- | Blocked Publisher or Site
  | Blocked

    -- | Unmatched User
  | UnmatchedUser
  deriving (Enum, Show, Eq)

instance FromJSON NoBidReasonCode where
  -- try floatingOrInteger
  parseJSON (Number i) =
    case floatingOrInteger i of
      Right n | 0 <= n && n <= 8 -> return (toEnum n)
      _ -> mzero

instance ToJSON NoBidReasonCode where
  toJSON nbrc = Number (scientific (fromIntegral $ fromEnum nbrc) 0)
