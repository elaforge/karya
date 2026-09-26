-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{-# LANGUAGE StrictData #-}
-- | Support for rhythmic spelling in different meters.
module Perform.Lilypond.Meter (
    Meter, meterNums, meterDenom
    , timeNum
    , defaultMeter
    , measureTime
    , unparseMeter, parseMeter
    , isBinary
    -- * tie breaking
    , allowedDuration
#ifdef TESTING
    , module Perform.Lilypond.Meter
#endif
) where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Vector.Unboxed as Vector
import           Data.Vector.Unboxed ((!))

import qualified Util.Num as Num
import qualified Util.P as P
import qualified Util.Parse as Parse
import qualified Util.Lists as Lists

import qualified Perform.Lilypond.Types as Types
import           Perform.Lilypond.Types (Duration(..), NoteDuration(..), Time)
import qualified Ui.Meter.Make as Meter.Make
import qualified Ui.Meter.Meter as Meter
import           Ui.Meter.Meter (AbstractMeter(..))

import           Global


data Meter = Meter
    -- | NonEmpty list of numerators.  E.g. [2, 3] indicates 2+3.
    { meterNums :: [Int]
    , meterDenom :: Duration
    , meterRanks :: Ranks
    } deriving (Eq, Show)

-- | Meter rank, indexed by 128th note.
type Ranks = Vector.Vector Rank
type Rank = Int

timeNum :: Meter -> Int
timeNum = Num.sum . meterNums

rankAt :: Meter -> Time -> Int
rankAt meter t = v ! (timeIndex t `mod` Vector.length v)
    where v = meterRanks meter

timeIndex :: Time -> Int
timeIndex = fromIntegral

indexTime :: Int -> Time
indexTime = fromIntegral

instance Pretty Meter where pretty = unparseMeter

instance Types.ToLily Meter where
    toLily (Meter nums denom _) =
        showt (Num.sum nums) <> "/" <> Types.toLily denom

isBinary :: Meter -> Bool
isBinary meter = case meterNums meter of
    [num] -> (==0) $ snd $ properFraction $ logBase 2 (fromIntegral num)
    _ -> False

-- | Duration of a measure, in Time.
measureTime :: Meter -> Time
measureTime meter =
    fromIntegral (timeNum meter) * Types.durToTime (meterDenom meter)

unparseMeter :: Meter -> Text
unparseMeter meter = Text.intercalate "+" (map showt (meterNums meter))
    <> "/" <> Types.toLily (meterDenom meter)

defaultMeter :: Meter
Right defaultMeter = parseMeter "4/4"

parseMeter :: Text -> Either Text Meter
parseMeter text = do
    (nums, denom) <- first ("parsing meter: "<>) $ Parse.parse pMeter text
    denom <- tryJust ("denominator not a valid duration: " <> showt denom) $
        Types.intDur denom
    makeMeter nums denom (abstractMeter nums denom)

abstractMeter :: [Int] -> Duration -> [AbstractMeter]
abstractMeter nums denom = case Map.lookup (nums, denom) defaultMeters of
    Just m -> [m]
    Nothing -> case defaultDivisions nums denom of
        [num] -> [D (replicate num T)]
        nums -> [D [D (replicate n T) | n <- nums]]
    where
    -- Certain simple duple meters get a simpler division.  This has the effect
    -- of allowing notes to cross beat divisions, e.g. 4 2 4 in 4/4.
    defaultMeters = Map.fromList
        [ (([1], D4), T)
        , (([2], D4), T)
        , (([4], D4), T)
        ]

defaultDivisions :: [Int] -> Duration -> [Int]
defaultDivisions [num] denom = Map.findWithDefault [num] (num, denom) defaults
    where
    defaults = Map.fromList
        [ ((5, D8), [3, 2])
        , ((6, D8), [3, 3])
        , ((7, D8), [3, 4])
        , ((9, D8), [3, 3, 3])
        , ((12, D8), [3, 3, 3, 3])
        ]
defaultDivisions nums _ = nums

pMeter :: Parse.Parser ([Int], Int)
pMeter = (,) <$> P.sepBy1 Parse.pPositive (P.char '+')
    <*> (P.char '/' *> Parse.pPositive)

makeMeter :: [Int] -> Duration -> [AbstractMeter] -> Either Text Meter
makeMeter nums denom meters = Meter nums denom <$> vector
    where
    vector
        | frac /= 0 = Left $ "can't fit " <> showt ranks <> " into "
            <> showt expected <> " by doubling"
        | otherwise = Right $ toVector $ subdivides (replicate exp 2) meters
    (exp, frac) = properFraction $
        logBase 2 (fromIntegral expected / fromIntegral ranks)
    expected = Num.sum nums * timeIndex (Types.durToTime denom)
    ranks = Num.sum $ map abstractLength meters
    toVector = Vector.fromList . map (fromEnum . fst)
        . Meter.Make.toRankDurations . map (1,)

subdivides :: [Int] -> [AbstractMeter] -> [AbstractMeter]
subdivides divs meter = foldr subdivide meter (reverse divs)

subdivide :: Int -> [AbstractMeter] -> [AbstractMeter]
subdivide n = map (Meter.subdivide n)

abstractLength :: AbstractMeter -> Int
abstractLength (D ds) = Num.sum $ map abstractLength ds
abstractLength T = 1

-- * allowed time

{- | Figure out how much time a note at the given position should be allowed
    before it must tie.

    The heuristic is:

    - A binary meter is one whose numerator is a power of 2.

    - First, restrict the maximum 'allowed_time'.  For binary meters, this is
    up to the rank of the start point - 2, which means that if you start on
    an 8th note, you can span until the next half note.  Compound meters are
    different because the rank divisions don't correspond to binary note
    divisions (e.g. 3/4 goes dotted half, quarter, 8th, etc., instead of 4/4's
    whole, half, quarter, etc.).  So they only go up to the rank-1.  This is
    winds up being too restrictive though, because it means that e.g. you could
    never span a quarter note if you start on an eighth, but 8 4 8 4 is
    a perfectly readable 3/4 bar.  So the allowed duration is extended to
    twice the duration to the next rank-1 crossing, which allows 8 8~8 to
    become 8 4.

    - Next, if the allowed duration corresponds exactly to possible note
    duration, take that.  This expresses that it's preferable to spell without
    a tie if you can.  If it doesn't correspond to a note duration, then it
    has to be tied, and break on the place where it crosses the lowest rank.

    - Complex meters like 2+3/4 are treated as binary if you are in the binary
    part.  TODO not yet

    See NOTE [tie-breaking heuristic]
-}
allowedDuration :: Bool -> Meter -> Time -> Time -> NoteDuration
allowedDuration useDot meter start_ dur =
    bestDuration . min (start + dur) $ allowedTime meter start
    where
    bestDuration end
        | Just ndur <- List.find ((== start+dur) . toTime) candidates = ndur
        | otherwise = fromMaybe (Types.NoteDuration Types.D128 False) $
            Lists.minimumOn (rankAt meter . toTime) candidates
        where
        candidates = takeWhile ((<=end) . toTime) $
            if useDot then dottedDurs else durs
    durs = reverse $ map (flip NoteDuration False) [D1 .. D128]
    dottedDurs = reverse
        [NoteDuration d dot | d <- [D1 .. D64], dot <- [True, False]]
    start = start_ `mod` measureTime meter
    toTime dur = start + Types.noteDurToTime dur

-- | See 'allowed_duration'.
allowedTime :: Meter -> Time -> Time
allowedTime meter start = fromMaybe (measureTime meter) $ if isBinary meter
    -- TODO is_binary in this part of the meter
    then atRank (startRank - 2)
    else case atRank (startRank - 1) of
        Nothing -> Nothing
        Just end -> min (start + (end - start) * 2) <$> atRank (startRank - 2)
    where
    atRank n = findRank start n meter
    startRank = rankAt meter start

-- | Find the time of the rank <= the given one.  Rank 0 can never be spanned,
-- so always stop at a rank 0.
findRank :: Time -> Rank -> Meter -> Maybe Time
findRank start rank = fmap ((+ (start + 1)) . indexTime)
    . Vector.findIndex (<= max 0 rank)
    . Vector.drop (timeIndex start + 1) . meterRanks

{-
    NOTE [tie-breaking heuristic]

    Because it's 3/4, h is not a valid rank, because it doesn't divide at
    natural points.  Actually ranks are not note durations, they are metric
    divisions.

    But this should be ok:
    0 w                       | 3/4
    1 q       q       q       |
    2 e   e   e   e   e   e   |
          4------>4------>8-->|

    But if it's longer than 4, then switch to tieing on the low rank:
    0 w                       | 3/4
    1 q       q       q       |
    2 e   e   e   e   e   e   |
          8-->                |
          4------>            |
          8-->4------>        | 4., but 4. is too complicated
          8-->4.--------->    | 2, seems like jumping 2 ranks, 8,4,2.
          8-->2-------------->|

    0 w                       | 3/4
    1 q       q       q       |
    2 e   e   e   e   e   e   |
    3 s s s s s s s s s s s s |
        s>
        8-->
        s>8-->
        s>8-->s>
        s>4------>
        s>4------>s>
        s>8-->4------>

    So maybe what I want is not spanning ranks, but the duration spanning
    ranks.  E.g. I started on an 8th note rank, so I can have up to 4 duration,
    but not beyond.  Or maybe I could go up to rank-1, but add a candidate at
    distance * 2?

    0 w                                       | 5/4
    1 h               h                       |
    2 q       q       q       q       q       |
    3 e   e   e   e   e   e   e   e   e   e   |
    4 s s s s s s s s s s s s s s s s s s s s |

    I think this is like 4/4, if I don't cross rank 1 I should be ok.
    Or treat it as one 2/4, one 3/4?  In that case, I shouldn't do complicated
    things in the second part.

    0 w                       | 6/8
    1 q           q           |
    2 e   e   e   e   e   e   |
          8-->
          4------>
          4------>8-->

    1 q           q           |
    2 e   e   e   e   e   e   |
              8-->8-->

    0 w                       | 6/8
    1 q           q           |
    2 e   e   e   e   e   e   |
    3 s s s s s s s s s s s s |
        s>
        8-->
        s>8-->

    0 w                               | 4/4
    1 h               h               |
    2 q       q       q       q       |
    3 e   e   e   e   e   e   e   e   |
          8-->
          4------>
          8-->8.-->
          4.--------->
          4.--------->8-->            | could be 2, but crosses h
          4.--------->4------>        | so 3 to 1

    But

    1 h               h               | 16
    2 q       q       q       q       | 8
    3 e   e   e   e   e   e   e   e   | 4
          4------>4------>4------>
          4------>8-->8-->4------>
              2-------------->

    So for binary, go up to rank-2.  For compound, go up to rank-1, but allow
    a complete note as long as its duration is rank-1.
    Or maybe I could say I don't like 8~8

    2 q       q       q       q       | 8
    3 e   e   e   e   e   e   e   e   | 4
    4 s s s s s s s s s s s s s s s s | 2
        s>
        8-->
        8.--->
        8.--->s>
        8.--->8-->
        8.--->8.--->
        8.--->4------>

    4/4
    0 w                               | 32
    1 h               h               | 16
    2 q       q       q       q       | 8
    3 e   e   e   e   e   e   e   e   | 4
    4 s s s s s s s s s s s s s s s s | 2
    5 33333333333333333333333333333333| 1
          |------>
          |-------------------------->

    3/4
    0 w                       |
    1 h               h       |
    2 q       q       q       |
    3 e   e   e   e   e   e   |
    4 s s s s s s s s s s s s |
    5 333333333333333333333333|
      2.--------------------->  0->0
          8->                   3->2
             2--------------->  2->0
                 8-->           3->2
                     4------->  2->0
                         8--->  3->end
-}
