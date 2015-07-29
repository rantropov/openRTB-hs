module OpenRTB.Enum.LocationType where

-- | The following table lists the options to indicate how the geographic
--   information was determined.
data LocationType =
    -- | GPS/Location Services
    GPS

    -- | IP Address
  | IP

    -- | User provided (e.g., registration data)
  | UserProvided
