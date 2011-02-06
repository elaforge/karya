{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- | Timetstamp is used by the various rendering subsystems as well as
    'Derive.Player'.

    RealTime are converted to these as the final stage of performance.
-}
module Perform.Timestamp (
    Timestamp, zero
    , from_millis, to_millis, to_micros
    , seconds, to_seconds
    , from_real_time, to_real_time
) where
import qualified Control.DeepSeq as DeepSeq
import qualified Util.Pretty as Pretty

import Ui


-- | An absolute timestamp, measured from some arbitrary starting position.
-- The resolution is only milliseconds, so it can't be used to align audio.
-- This is just for the MIDI.
newtype Timestamp = Timestamp Integer
    deriving (Eq, Ord, Show, Num, Enum, Real, DeepSeq.NFData)

zero :: Timestamp
zero = Timestamp 0

from_millis :: Integer -> Timestamp
from_millis = Timestamp

to_millis :: Timestamp -> Integer
to_millis (Timestamp i) = i

to_micros :: Timestamp -> Integer
to_micros (Timestamp ts) = ts * 1000

seconds :: (RealFrac a) => a -> Timestamp
seconds secs = Timestamp (round (secs * 1000))

to_seconds :: Timestamp -> Double
to_seconds (Timestamp ts) = fromIntegral ts / 1000

from_real_time :: RealTime -> Timestamp
from_real_time = Timestamp . round . (*1000)

to_real_time :: Timestamp -> RealTime
to_real_time = RealTime . (/1000) . fromIntegral . to_millis

instance Pretty.Pretty Timestamp where
    pretty ts = Pretty.show_float (Just 3) (to_seconds ts) ++ "s"
