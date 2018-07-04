-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveDataTypeable #-}
{- | RealTime represents seconds, as opposed to ScoreTime, which is in abstract
    units.  Everything eventually is transformed into RealTime to be
    performed.

    This type has switched from floating point to decimal and back again.  The
    problem is that floating point is not exact, but there are a few
    operations that require events that have the same ScoreTime to be grouped
    with each other once they reach RealTime.  For instance, controls are
    clipped to the note boundaries, and a note is required to have a pitch at
    exactly its starting time.  While the event that produces the pitch signal
    may have the same ScoreTime as the note it belongs to, if imprecision has
    caused it to drift a little by the time it gets to performance, the note
    may wind up with no initial pitch, or pick up the pitch of the next note
    as a pitch bend.

    An example of how imprecision can accumulate is a block call with pitch
    set in the caller.  If the sub-block has a note at 0 this should line up
    with the start of the block call in the super-block and hence with a pitch
    at the same time.  But the sub-block has its own warp which is
    a composition of the its tempo and the super-block's tempo.  In theory the
    sub-block's warp should be shifted so its 0 starts at the calling point
    in the super-block, but in practice this is a number of floating point
    operations (addition, linear interpolation, ...) and the value may very
    well be slightly different.

    Unfortunately switching RealTime to a lower-precision decimal type has the
    same problem because it introduces even more imprecision due to the
    ScoreTime -> RealTime -> ScoreTime conversion (this happens during warp
    composition, for instance, since shift and stretch are in ScoreTime).
    And I think it's ultimately not quite right because rounding will still
    produce incorrect results if the imprecise value falls at a rounding
    boundary.

    Eventually, for MIDI at least, everything is rounded down to milliseconds
    so hopefully any imprecision can be accounted for by the operations that
    care about it and eventually be removed from the final result.
-}
module Perform.RealTime (
    RealTime, div, mul, large, larger, suffix
    , show_units
    -- * convert from
    , seconds, milliseconds, microseconds, from_score
    -- * convert to
    , to_diff, to_seconds, to_milliseconds, to_microseconds, to_score
    -- * misc
    , eta, (==), (>), (<=)
) where
import qualified Prelude
import Prelude hiding ((==), (>), (<=), div)
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Digest.CRC32 as CRC32
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Data.Typeable as Typeable

import qualified Foreign
import qualified ForeignC as C
import qualified Text.Read as Read

import qualified Util.CUtil as CUtil
import Util.Crc32Instances ()
import qualified Util.Num as Num
import qualified Util.Serialize as Serialize
import qualified Util.Test.ApproxEq as ApproxEq

import qualified Ui.ScoreTime as ScoreTime
import qualified Derive.ShowVal as ShowVal
import Global


-- | A concrete unit of time.
--
-- This must have negative values because it's used for signals, which are
-- used for the warp map, which is oriented with zero at the note start.  If
-- a note wants to get the real time before it, it must look up a negative
-- RealTime.
newtype RealTime = RealTime Double deriving
    ( DeepSeq.NFData, Num, Fractional, Real, RealFrac, Eq, Ord
    , Serialize.Serialize, CRC32.CRC32, ApproxEq.ApproxEq
    , Typeable.Typeable, Aeson.ToJSON, Aeson.FromJSON
    )

-- I could derive Storable, but technically speaking Double is not necessarily
-- the same as CDouble.
instance Foreign.Storable RealTime where
    sizeOf _ = Foreign.sizeOf (0 :: C.CDouble)
    alignment _ = Foreign.alignment (0 :: C.CDouble)
    poke p (RealTime d) = Foreign.poke (Foreign.castPtr p) (CUtil.c_double d)
    peek p = RealTime . CUtil.hs_double <$> Foreign.peek (Foreign.castPtr p)

instance C.CStorable RealTime where
    sizeOf = Foreign.sizeOf
    alignment = Foreign.alignment
    peek = Foreign.peek
    poke = Foreign.poke

instance Show RealTime where show (RealTime t) = show t
instance Read.Read RealTime where readPrec = RealTime <$> Read.readPrec

instance ShowVal.ShowVal RealTime where
    show_val = (`Text.snoc` suffix) . Num.showFloat 3 . to_seconds

instance Pretty RealTime where
    pretty t = Num.showFloatP False 2 (to_seconds t) <> Text.singleton suffix

div :: RealTime -> Double -> RealTime
div a b = seconds (to_seconds a / b)
infixl 7 `div`

mul :: RealTime -> Double -> RealTime
mul a b = seconds (to_seconds a * b)
infixl 7 `mul`

-- | A large RealTime as a stand-in for "forever" in signals.
--
-- I tried Infinity, but a constant signal starting at -Infinity will have
-- an integral ending at (Infinity, Infinity) (or (Infinity, NaN) in practice),
-- at which point I lost the slope.
--
-- 1e10 is recognizable in debugging output as a special value, and still quite
-- far away from 2^53 (9e15), which is where integers can no longer be
-- represented exactly in a 64 bit Double.  This means I can take the integral
-- at a steep slope and I should still be in the realm of exact integers, which
-- means the slope should stay accurate.
large :: RealTime
large = 1e10

-- | Comfortably bigger than 'large', so it won't cross large given normal
-- amonuts of time shift.
larger :: RealTime
larger = 1e11

suffix :: Char
suffix = 's'

-- | Show RealTime as hours, minutes, seconds.
show_units :: RealTime -> Text
show_units t = units <> pretty (seconds (fromIntegral secs + frac))
    where
    units = mconcatMap (\(a, b) -> showt a <> b) $
        filter ((Prelude.>0) . fst) [(hours, "h"), (mins, "m")]
    (t1, frac) = properFraction (to_seconds t)
    (hours, t2) = t1 `divMod` (60 * 60)
    (mins, secs) = t2 `divMod` 60

-- * convert from

seconds :: Double -> RealTime
seconds = RealTime

milliseconds :: Integer -> RealTime
milliseconds = seconds . (/1000) . fromIntegral

microseconds :: Integer -> RealTime
microseconds = seconds . (/1000000) . fromIntegral

from_score :: ScoreTime.ScoreTime -> RealTime
from_score = seconds . ScoreTime.to_double

-- * convert to

to_diff :: RealTime -> Time.NominalDiffTime
to_diff = realToFrac

to_seconds :: RealTime -> Double
to_seconds (RealTime s) = s

to_milliseconds :: RealTime -> Integer
to_milliseconds = round . (*1000) . to_seconds

to_microseconds :: RealTime -> Integer
to_microseconds = round . (*1000000) . to_seconds

to_score :: RealTime -> ScoreTime.ScoreTime
to_score = ScoreTime.from_double . to_seconds

-- | Eta for comparison.  Since RealTimes are seconds, this amount of time is
-- definitely unnoticeable.
eta :: RealTime
eta = 0.0000000000004

-- | RealTimes are imprecise, so compare them with this instead of (==).
(==) :: RealTime -> RealTime -> Bool
(==) = ApproxEq.eq (to_seconds eta)

(>), (<=) :: RealTime -> RealTime -> Bool
a > b = a - eta Prelude.> b
a <= b = not (a > b)
