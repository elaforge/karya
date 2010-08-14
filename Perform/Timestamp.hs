{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- | Timetstamp is used by the various rendering subsystems as well as
    'Derive.Player'.

    RealTime are converted to these as the final stage of performance.
-}
module Perform.Timestamp where
import qualified Control.DeepSeq as DeepSeq
import Text.Printf
import Util.Pretty

import Ui


-- | An absolute timestamp, measured from some arbitrary starting position.
-- The resolution is only milliseconds, so it can't be used to align audio.
-- This is just for the MIDI.
newtype Timestamp = Timestamp Integer
    deriving (Eq, Ord, Show, Num, Enum, Real, Integral, DeepSeq.NFData)

immediately :: Timestamp
immediately = Timestamp 0

seconds :: (RealFrac a) => a -> Timestamp
seconds secs = Timestamp (round (secs * 1000))
to_seconds :: Timestamp -> Double
to_seconds (Timestamp ts) = fromIntegral ts / 1000

to_microseconds :: Timestamp -> Integer
to_microseconds (Timestamp ts) = ts * 1000

from_real_time :: RealTime -> Timestamp
from_real_time = round . (*1000)
to_real_time :: Timestamp -> RealTime
to_real_time = RealTime . (/1000) . fromIntegral

instance Pretty Timestamp where
    pretty ts = printf "%.3fs" (to_seconds ts)
