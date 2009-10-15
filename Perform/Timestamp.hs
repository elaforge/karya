{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- | Timetstamp is used by the various rendering subsystems as well as
    'Derive.Player'.

    TrackPos are converted to these as the final stage of performance.
-}
module Perform.Timestamp where
import Util.Pretty
import Text.Printf

import Ui


-- | An absolute timestamp, measured from some arbitrary starting position.
-- Since I use the default PortMidi timer, it's from the initialization of the
-- midi subsystem at the moment.
--
-- The resolution is milliseconds.
newtype Timestamp = Timestamp Integer
    deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

immediately :: Timestamp
immediately = Timestamp 0

seconds :: (RealFrac a) => a -> Timestamp
seconds secs = Timestamp (round (secs * 1000))
to_seconds :: Timestamp -> Double
to_seconds (Timestamp ts) = fromIntegral ts / 1000

to_microseconds :: Timestamp -> Integer
to_microseconds (Timestamp ts) = ts * 1000

-- | TrackPos is converted 1:1000.  This means that a TrackPos, after passing
-- through all the tempo mapping, should eventually correspond to seconds.
-- This means I can't align to samples, but MIDI timing is not that accurate
-- anyway.  If I ever need to align samples I may have to change this.
from_track_pos :: TrackPos -> Timestamp
from_track_pos = round . (*1000)
to_track_pos :: Timestamp -> TrackPos
to_track_pos = TrackPos . (/1000) . fromIntegral

instance Pretty Timestamp where
    pretty ts = printf "%.3fs" (to_seconds ts)
