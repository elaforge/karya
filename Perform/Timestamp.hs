{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- | Timetstamp is used by the various rendering subsystems as well as
    'Derive.Player'.

    RealTime are converted to these as the final stage of performance.
-}
module Perform.Timestamp (
    Timestamp
    , add, sub, mul, zero
    , from_millis, to_millis, to_micros
    , seconds, to_seconds
    , from_real_time, to_real_time
) where
import qualified Data.Word as Word
import qualified Control.DeepSeq as DeepSeq
import qualified Util.Pretty as Pretty

import Ui
import qualified Perform.RealTime as RealTime


-- | An absolute timestamp, measured from some arbitrary starting position.
-- The resolution is only milliseconds, so it can't be used to align audio.
-- This is just for the MIDI.
newtype Timestamp = Timestamp Word.Word64
    deriving (Eq, Ord, Show, DeepSeq.NFData)
    -- The underlying type used by CoreMIDI is uint64.  I don't know what other
    -- OSes use, but probably similar.
    -- I previously used Integer, but that's a sum type and hence can't be
    -- unpacked into another type, e.g. WriteMessage.

add, sub :: Timestamp -> Timestamp -> Timestamp
add (Timestamp a) (Timestamp b) = Timestamp (a + b)
sub (Timestamp a) (Timestamp b)
    | b > a = zero
    | otherwise = Timestamp (a - b)

mul :: Timestamp -> Int -> Timestamp
mul (Timestamp a) b = Timestamp (a * fromIntegral b)

zero :: Timestamp
zero = Timestamp 0

from_millis :: Integer -> Timestamp
from_millis = Timestamp . fromIntegral

to_millis :: Timestamp -> Integer
to_millis (Timestamp i) = fromIntegral i

to_micros :: Timestamp -> Integer
to_micros (Timestamp ts) = fromIntegral (ts * 1000)

seconds :: (RealFrac a) => a -> Timestamp
seconds secs = Timestamp (round (secs * 1000))

to_seconds :: Timestamp -> Double
to_seconds (Timestamp ts) = fromIntegral ts / 1000

from_real_time :: RealTime -> Timestamp
from_real_time = Timestamp . fromIntegral . (`div` 1000)
    . RealTime.to_microseconds

to_real_time :: Timestamp -> RealTime
to_real_time (Timestamp ms) = RealTime.microseconds (fromIntegral ms * 1000)

instance Pretty.Pretty Timestamp where
    pretty ts = Pretty.show_float (Just 3) (to_seconds ts) ++ "s"
