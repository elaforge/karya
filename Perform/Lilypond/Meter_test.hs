-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Perform.Lilypond.Meter_test where
import qualified Data.Text as Text
import qualified Data.Vector.Unboxed as Vector.Unboxed
import           GHC.Stack (HasCallStack)

import qualified Util.Lists as Lists
import qualified Util.Test.Testing as Testing
import           Perform.Lilypond.LilypondTest (parseMeter)
import qualified Perform.Lilypond.Meter as Meter
import qualified Perform.Lilypond.Types as Types
import           Perform.Lilypond.Types (Duration(..))

import           Global
import           Util.Test



test_allowed_duration :: Test
test_allowed_duration = do
    let f meter start dur = toLily $
            convertDuration (parseMeter meter) True start dur
        toLily = Text.intercalate "~" . map Types.toLily
    let nd = Types.NoteDuration

    equalDurs "4/4" 0 (steps D4 D4 5)
        [ [nd D4 False]
        , [nd D2 False]
        , [nd D2 True]
        , [nd D1 False]
        , [nd D1 False, nd D4 False]
        ]

    equal (f "4/4" (d D8) (d D4)) "4"
    -- Starting on a quarter note rank 2 can span rank 1 at the center.
    equal (f "4/4" (d D4) (d D2)) "2"

    equal (f "4/4" (d D8) (d D4)) "4"
    equal (f "4/4" (d D8) (d D4 + d D16)) "8~8."
    equalDurs "4/4" (d D8) (steps D8 D8 5)
        [ [nd D8 False]
        , [nd D4 False]
        , [nd D4 True]
        , [nd D4 True, nd D8 False]
        , [nd D4 True, nd D4 False]
        ]
    equalDurs "4/4" (d D16) (steps D16 D16 5)
        [ [nd D16 False]
        , [nd D8 False]
        , [nd D8 True]
        , [nd D8 True, nd D16 False]
        , [nd D8 True, nd D8 False]
        ]
    -- But if I have to break it, break at the quarter.
    equal (f "4/4" (d D8) (d D8 * 2 + d D16)) "8~8."

    equalDurs "3/4" 0 (steps D4 D4 4)
        [ [nd D4 False]
        , [nd D2 False]
        , [nd D2 True]
        , [nd D2 True, nd D4 False]
        ]
    equal (f "3/4" (d D8) (d D4)) "4"
    equalDurs "3/4" (d D8) [d D8 * 3] [[nd D8 False, nd D4 False]]
    equalDurs "3/4" (d D8) (steps D8 D8 5)
        [ [nd D8 False]
        , [nd D4 False]
        , [nd D8 False, nd D4 False]
        , [nd D8 False, nd D4 True]
        , [nd D8 False, nd D2 False]
        ]

    -- I used to break the middle of 6/8, but I'm more lenient in non-binary
    -- meters now.  This means a 6/8 will happily spell q q q, but I guess
    -- that's not exactly hard to read, so ok.
    -- equal_durs "6/8" (d D4) [d D4] [[nd D8 False, nd D8 False]]

-- TODO this used to be in Meter, so I tested it.  It's no longer necessary,
-- so I should probably move to directly testing Meter.allowed_duration, but
-- it's too much bother right now.
convertDuration :: Meter.Meter -> Bool -> Types.Time -> Types.Time
    -> [Types.NoteDuration]
convertDuration meter useDot_ = go
    where
    go start dur
        | dur <= 0 = []
        | allowed >= dur = [allowedDur]
        | otherwise = allowedDur : go (start + allowed) (dur - allowed)
        where
        allowedDur = Meter.allowedDuration useDot meter start dur
        allowed = Types.noteDurToTime allowedDur
    -- Dots are always allowed for non-binary meters.
    useDot = useDot_ || not (Meter.isBinary meter)

-- This is probably overkill.  "8~8" seems ok for single comparisons.
equalDurs :: HasCallStack => Text -> Types.Time -> [Types.Time]
    -> [[Types.NoteDuration]] -> Test
equalDurs meter_ start durs =
    Testing.equalFmt
        ((fmtMeter meter <>) . Text.unlines . map (fmtDurs start))
        (map (convertDuration meter True start) durs)
    where meter = parseMeter meter_

fmtMeter :: Meter.Meter -> Text
fmtMeter meter = Text.unlines $ map fmtRank [0..3]
    where
    ranks = Vector.Unboxed.toList $ Meter.meterRanks meter
    fmtRank r = mconcatMap (fmt r) $
        dropWhile null $ Lists.splitBefore (<=r) ranks
    fmt r g = Text.justifyLeft (toSpaces g) ' '  (showt r)
    toSpaces = (`div` fromIntegral (Types.durToTime minDuration)) . length

fmtDurs :: Types.Time -> [Types.NoteDuration] -> Text
fmtDurs start durs =
    Text.replicate (fromIntegral $ start `div` minTime) " "
    <> mconcatMap fmtDur durs

fmtDur :: Types.NoteDuration -> Text
fmtDur dur =
    Text.justifyLeft (toSpaces dur - 1) '-' (Types.noteDurChar dur) <> ">"
    where
    toSpaces = fromIntegral . (`div` minTime) . Types.noteDurToTime

minDuration :: Types.Duration
minDuration = D32
minTime = Types.durToTime minDuration

d :: Duration -> Types.Time
d = Types.durToTime

steps :: Duration -> Duration -> Int -> [Types.Time]
steps start dur count = take count $ Lists.range_ (d start) (d dur)
