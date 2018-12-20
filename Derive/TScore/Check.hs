-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.TScore.Check where
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Util.CallStack as CallStack
import qualified Util.Log as Log
import qualified Util.Then as Then

import qualified Derive.LEvent as LEvent
import qualified Derive.TScore.Parse as Parse
import qualified Derive.TScore.T as T

import           Global


-- * rhythm

{-
    Maybe writing, say, 12 for 3 akshara in chatusra gati is too annoying.
    Also, if it turns out I need 8 per akshara then I would have to double
    every single number.  So the duration number should be akshara, with a
    way to divide it.

    Multiplicative doesn't have this problem.

    4, 2, 1, 1/2, 1/4.  I could omit the 1, so /2.  Isn't this effectively
    multiplicative, only with akshara whole notes?  If I allow a numerator,
    then I can write 3/5 etc.

    So for kandam, I would use 1 for akshara, then 3/5 etc. for durations under
    that.  Or I could use a ruler-style subdivision, e.g. 1 is one avartanam,
    .1 is one akshara, ..1 is one matra.  This way I can't say "two notes at
    double speed", I have to know the division, e.g. 'k1 t2 k1 k.2 t o2'.  Of
    course I could also write it as 'k.2 ~ t ~ ~ ~ k ~ k t o ~ ~ ~', or if
    I have a tuplet notation: 'k1 t ~ k t(k t) o ~'.

    In kandam: 'k..2 d..2 p..1'.  It seems like high subdivisions are going to
    be buried in dots, unless I can set a base matra: 'matra=.. k2 d2 p1'

    matra=.. k2 d -/p1 | k2 d -/p1 | k2 k d1 | ~ p k2 -/p1 |
    p = [| k d -/p |]
    sequence = [| 'p | 'p | k2 k d1 | ~ p k2 -/p1 |]

    also what about local assignment:

    '-p' = -/p
    P = matra=.. k2 d -p1
    sequence = matra=.. P | P | k2 k d1 | ~ p k2 -p1 |

    Notes:
        . The matra= setting is local to its expression, e.g. 'P'
        . Calls have implicit duration, they don't need a 5 suffix.
-}

data Meter = Meter {
    -- | Rank, and then time interval to the next rank:
    -- Adi: [(2, 4), (1, 2), (1, 2)] * nadai
    meter_pattern :: [(T.Rank, Time)]
    -- | How many time units per beat.
    , meter_time_per_beat :: Time
    -- | If true, beats fall at the end of measures.
    , meter_negative :: !Bool
    } deriving (Eq, Show)

-- | Integral time.  This is the smallest time unit expressed.
newtype Time = Time Int
    deriving (Ord, Eq, Num, Enum, Real, Integral)

instance Show Time where
    show (Time t) = show t ++ "t"

data RhythmState = RhythmState {
    state_now :: !Time
    , state_duration :: ! Time
    } deriving (Eq, Show)

additive_rhythm :: Meter -> [T.Token] -> [LEvent.LEvent (Time, Time, T.Note)]
additive_rhythm meter =
    Maybe.catMaybes . Then.mapAccumL token initial_state final
    where
    initial_state = RhythmState 0 (meter_time_per_beat meter)
    final state
        | beat == 0 = []
        | otherwise = (:[]) $ Just $
            warn state Nothing $ "extra beats at the end: " <> showt beat
        where beat = state_now state `mod` cycle_dur
    token state t = (state,) $ case t of
        T.TBarline (T.Barline rank) -> case Map.lookup beat expected_rank of
             Just r | r == rank -> Nothing
             _ -> Just $ warn state Nothing $ "barline " <> showt rank
        T.TNote note -> Just $ LEvent.Event (state_now state, 1, note)
            where dur = T.note_duration note
        where
        beat = state_now state `mod` cycle_dur

    cycle_dur = sum $ map snd pattern
    expected_rank = Map.fromList (zip ts (map fst pattern))
        where ts = scanl (+) 0 (map snd pattern)
    pattern = meter_pattern meter

    warn :: CallStack.Stack => RhythmState -> Maybe T.Note -> Text
        -> LEvent.LEvent a
    warn state note =
        LEvent.Log . Log.msg Log.Warn Nothing
            . ((showt measure <> "/" <> showt beat <> note_s <> ": ") <>)
        where
        (measure, beat) = state_now state `divMod` cycle_dur
        note_s = maybe "" ((" "<>) . Parse.unparse) note
