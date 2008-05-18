{-# OPTIONS_GHC -XGeneralizedNewtypeDeriving #-}
{- | Timetstamp is used by the various rendering subsystems as well as
Derive.Player.

Basically, everyone who deals with times that go directly to the midi scheduler
uses Timestamps.  This is more concrete than TrackPos, which may still be
warped through the tempo map, before eventually being mapped to a Timestamp.
-}
module Perform.Timestamp where
import Util.Pretty
import Text.Printf

import qualified Util.Seq as Seq

import Ui.Types


-- | An absolute timestamp, measured from some arbitrary starting position.
-- Since I use the default PortMidi timer, it's from the initialization of the
-- midi subsystem at the moment.
--
-- The resolution is milliseconds.
newtype Timestamp = Timestamp Integer deriving (Eq, Ord, Show, Num)

immediately :: Timestamp
immediately = Timestamp 0

seconds :: (RealFrac a) => a -> Timestamp
seconds secs = Timestamp (floor (secs * 1000))
to_seconds :: Timestamp -> Double
to_seconds (Timestamp ts) = fromIntegral ts / 1000

to_microseconds :: Timestamp -> Integer
to_microseconds (Timestamp ts) = ts

-- | TrackPos is converted 1:1.  This means that a TrackPos, after passing
-- through all the tempo mapping, should eventually correspond to milliseconds.
-- This means I can't align to samples, but MIDI timing is not that accurate
-- anyway.  If I ever need to align samples I may have to change this.
from_track_pos :: TrackPos -> Timestamp
from_track_pos (TrackPos pos) = Timestamp pos
to_track_pos :: Timestamp -> TrackPos
to_track_pos (Timestamp ts) = TrackPos ts

instance Pretty Timestamp where
    pretty ts = printf "%.3fs" (to_seconds ts)
