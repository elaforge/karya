-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Ui.ScoreTime (
    ScoreTime, TrackTime, round, to_double, to_cdouble, double, suffix
    , eta, (==), (>), (<=)
    , is_negative
) where
import qualified Prelude
import Prelude hiding ((==), (>), (<=), round)
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Digest.CRC32 as CRC32
import qualified Data.Text as Text
import qualified ForeignC as C
import qualified Text.Read as Read

import qualified Util.ApproxEq as ApproxEq
import qualified Util.CUtil as CUtil
import Util.Crc32Instances ()
import qualified Util.Num as Num
import qualified Util.Serialize as Serialize

import qualified Derive.ShowVal as ShowVal
import Global


-- | Score time is the abstract unit of time, and its mapping to real time
-- is dependent on the score context.  ScoreTime units can be negative, but
-- blocks only display events at >=0 ScoreTime.
newtype ScoreTime = ScoreTime Double deriving
    ( DeepSeq.NFData, Num, Fractional, Real, RealFrac, Eq, Ord
    , Serialize.Serialize, CRC32.CRC32, ApproxEq.ApproxEq
    )

instance ShowVal.ShowVal ScoreTime where
    show_val = (`Text.snoc` suffix) . Num.showFloat 3 . to_double

{- | This is also ScoreTime, but it's relative to the beginning of the track.
    I.e., UI events are all in track time, but when they get shifted and
    stretched as by note slicing they're no longer in TrackTime, but not yet in
    RealTime.

    I'd like to make a type-level distinction because it's easy to get confused
    about whether a time has or hasn't been transformed, but when I tried it
    seemed like a big hassle since I'd really like for TrackTime to be
    a subtype of ScoreTime.  I could do it with a phantom type, but it would
    change about a million type declarations.  And since Events start in
    TrackTime but are then ScoreTime if transformed, they would also need
    a type parameter, along with probably a few other basic data types.

    Unless I work up the courage to do that someday, the least I can do is
    document the difference with a type synonym.
-}
type TrackTime = ScoreTime

{- | Traditionally, time would be an integral type with a highly composite
    number as the unit.  This is so that common musical durations such as 1/3,
    1/6, or 1/64 can be represented exactly.  However, while this is good
    enough for the score, it's insufficiently accurate for derivation, which
    uses ScoreTime to shift and stretch events.

    A principled solution would probably be to use an integral type for UI
    events in "Events.Events" and convert to floating point on derivation.
    However, that seems like a hassle and simply rounding the event's start and
    durations when they go into the track should achieve the same effect.
-}
round :: ScoreTime -> ScoreTime
round t
    | isNegativeZero (to_double t) = t
    | otherwise = double . (/divisor) . fromIntegral . to_int $ t
    where
    to_int :: ScoreTime -> Integer
    to_int = Prelude.round . (*divisor) . to_double

divisor :: Double
divisor = 2^7 * 3^3 * 5^2 * 7

-- I could derive Storable, but technically speaking Double is not necessarily
-- the same as CDouble.
instance C.CStorable ScoreTime where
    sizeOf _ = C.sizeOf (0 :: C.CDouble)
    alignment _ = C.alignment (0 :: C.CDouble)
    poke p (ScoreTime d) = C.poke (C.castPtr p) (CUtil.c_double d)
    peek p = ScoreTime . CUtil.hs_double <$> C.peek (C.castPtr p)

instance Show ScoreTime where show (ScoreTime n) = show n
instance Read.Read ScoreTime where readPrec = ScoreTime <$> Read.readPrec

instance Pretty ScoreTime where
    pretty (ScoreTime p) = Num.showFloat 3 p <> Text.singleton suffix

to_double :: ScoreTime -> Double
to_double (ScoreTime p) = p

to_cdouble :: ScoreTime -> C.CDouble
to_cdouble = C.CDouble . to_double

double :: Double -> ScoreTime
double = ScoreTime

-- | t is for time, since RealTime uses s for seconds
suffix :: Char
suffix = 't'

-- | Eta for comparison.  ScoreTimes are all relative, but there's no reason to
-- use such tiny ones.
eta :: ScoreTime
eta = 0.00000000000004

-- | ScoreTimes are imprecise, so compare them with this instead of (==).
(==) :: ScoreTime -> ScoreTime -> Bool
(==) = ApproxEq.eq (to_double eta)

-- | True if the second is greater than the first - eta.  This can be used to
-- determine if the start of an event has passed, while giving a little bit of
-- extra allowance if its close enough.
(>), (<=) :: ScoreTime -> ScoreTime -> Bool
a > b = a - eta Prelude.> b
a <= b = not (a > b)

-- | Unlike <0, this counts -0 as negative.
is_negative :: ScoreTime -> Bool
is_negative (ScoreTime t) = t < 0 || isNegativeZero t
