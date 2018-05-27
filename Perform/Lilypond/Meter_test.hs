-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Perform.Lilypond.Meter_test where
import qualified Data.Text as Text
import qualified Data.Vector.Unboxed as Vector.Unboxed

import qualified Util.CallStack as CallStack
import qualified Util.Seq as Seq
import Util.Test
import qualified Util.Test.Testing as Testing

import Perform.Lilypond.LilypondTest (parse_meter)
import qualified Perform.Lilypond.Meter as Meter
import qualified Perform.Lilypond.Types as Types
import Perform.Lilypond.Types (Duration(..))

import Global



test_allowed_duration = do
    let f meter start dur = to_lily $
            convert_duration (parse_meter meter) True start dur
        to_lily = Text.intercalate "~" . map Types.to_lily
    let nd = Types.NoteDuration

    equal_durs "4/4" 0 (steps D4 D4 5)
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
    equal_durs "4/4" (d D8) (steps D8 D8 5)
        [ [nd D8 False]
        , [nd D4 False]
        , [nd D4 True]
        , [nd D4 True, nd D8 False]
        , [nd D4 True, nd D4 False]
        ]
    equal_durs "4/4" (d D16) (steps D16 D16 5)
        [ [nd D16 False]
        , [nd D8 False]
        , [nd D8 True]
        , [nd D8 True, nd D16 False]
        , [nd D8 True, nd D8 False]
        ]
    -- But if I have to break it, break at the quarter.
    equal (f "4/4" (d D8) (d D8 * 2 + d D16)) "8~8."

    equal_durs "3/4" 0 (steps D4 D4 4)
        [ [nd D4 False]
        , [nd D2 False]
        , [nd D2 True]
        , [nd D2 True, nd D4 False]
        ]
    equal (f "3/4" (d D8) (d D4)) "4"
    equal_durs "3/4" (d D8) [d D8 * 3] [[nd D8 False, nd D4 False]]
    equal_durs "3/4" (d D8) (steps D8 D8 5)
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
convert_duration :: Meter.Meter -> Bool -> Types.Time -> Types.Time
    -> [Types.NoteDuration]
convert_duration meter use_dot_ = go
    where
    go start dur
        | dur <= 0 = []
        | allowed >= dur = [allowed_dur]
        | otherwise = allowed_dur : go (start + allowed) (dur - allowed)
        where
        allowed_dur = Meter.allowed_duration use_dot meter start dur
        allowed = Types.note_dur_to_time allowed_dur
    -- Dots are always allowed for non-binary meters.
    use_dot = use_dot_ || not (Meter.is_binary meter)

-- This is probably overkill.  "8~8" seems ok for single comparisons.
equal_durs :: CallStack.Stack => Text -> Types.Time -> [Types.Time]
    -> [[Types.NoteDuration]] -> IO Bool
equal_durs meter_ start durs =
    Testing.equal_fmt
        ((fmt_meter meter <>) . Text.unlines . map (fmt_durs start))
        (map (convert_duration meter True start) durs)
    where meter = parse_meter meter_

fmt_meter :: Meter.Meter -> Text
fmt_meter meter = Text.unlines $ map fmt_rank [0..3]
    where
    ranks = Vector.Unboxed.toList $ Meter.meter_ranks meter
    fmt_rank r = mconcatMap (fmt r) $
        dropWhile null $ Seq.split_before (<=r) ranks
    fmt r g = Text.justifyLeft (to_spaces g) ' '  (showt r)
    to_spaces = (`div` fromIntegral (Types.dur_to_time min_duration)) . length

fmt_durs :: Types.Time -> [Types.NoteDuration] -> Text
fmt_durs start durs =
    Text.replicate (fromIntegral $ start `div` min_time) " "
    <> mconcatMap fmt_dur durs

fmt_dur :: Types.NoteDuration -> Text
fmt_dur dur =
    Text.justifyLeft (to_spaces dur - 1) '-' (Types.note_dur_char dur) <> ">"
    where
    to_spaces = fromIntegral . (`div` min_time) . Types.note_dur_to_time

min_duration :: Types.Duration
min_duration = D32
min_time = Types.dur_to_time min_duration

d :: Duration -> Types.Time
d = Types.dur_to_time

steps :: Duration -> Duration -> Int -> [Types.Time]
steps start dur count = take count $ Seq.range_ (d start) (d dur)
