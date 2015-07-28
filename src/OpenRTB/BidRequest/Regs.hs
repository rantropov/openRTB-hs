module OpenRTB.BidRequest.Regs where

import Data.Aeson

-- | This object contains any legal, governmental, or industry regulations that
--   apply to the request. The `coppa` flag signals whether or not the request
--   falls under the United States Federal Trade Commission's regulations for
--   the United States Children's Online Privacy Protection Act ("COPPA"). Refer
--   to Section 7.1 for more information.
data Regs = Regs
  {
    -- | Flag indicatting if this request is subject to the COPPA regulations
    --   established by the USA FTC, where 0 = no, 1 = yes.
    coppa :: Maybe Bool

    -- | Placeholder for exchange-specific extensions to OpenRTB.
  , ext :: Maybe Value
  }
