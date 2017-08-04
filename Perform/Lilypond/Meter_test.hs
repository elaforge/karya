-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Perform.Lilypond.Meter_test where
import qualified Data.Text as Text

import qualified Util.CallStack as CallStack
import qualified Util.Seq as Seq
import Util.Test

import Perform.Lilypond.LilypondTest (parse_meter)
import qualified Perform.Lilypond.Meter as Meter
import qualified Perform.Lilypond.Types as Types
import Perform.Lilypond.Types (Duration(..))

import Global


test_convert_duration = do
    let f meter pos = Types.to_lily $ head $
            Meter.convert_duration (parse_meter meter) True
                pos (Types.time_per_whole - pos)
    equal (map (f "4/4") [0, 8 .. 127])
        [ "1", "8.", "4.", "16", "2.", "8.", "8", "16"
        -- mid-measure
        , "2", "8.", "4.", "16", "4", "8.", "8", "16"
        ]
    equal (map (f "2/4") [0, 8 .. 127]) $
        concat $ replicate 2 ["2", "8.", "4.", "16", "4", "8.", "8", "16"]

test_allowed_time_best = do
    let f meter = extract_rhythms
            . map (Meter.allowed_time_best True (parse_meter meter))
        steps dur end = Seq.range' 0 (end * Types.dur_to_time dur)
            (Types.dur_to_time dur)

    -- 4/4, being duple, is liberal about spanning beats, since it uses rank-2.
    equal (f "4/4" (steps D4 5)) "1 2. 2 4 1"
    equal (f "4/4" (steps D8 9)) "1 4. 2. 8 2 4. 4 8 1"
    -- 6/8 is more conservative.
    equal (f "3+3/8" (steps D8 7)) "2. 4 8 4. 4 8 2."
    -- Irregular meters don't let you break the middle dividing line no matter
    -- what, because 'Meter.find_rank' always stops at rank 0.
    equal (f "3+2/4" (steps D8 11)) "2. 8 2 8 4 8 2 8 4 8 2."
        -- This has 8 after the middle 2 instead of 4. like 4/4 would have.
        -- That's because it's not duple, so it's more conservative.
        -- I guess that's probably ok.

    equal (f "3+4/8" (steps D8 7)) "4. 4 8 2 4. 4 8"
    -- 2+2 means don't go over 8th note division.
    equal (f "3+2+2/8" (steps D8 7)) "4. 4 8 2 8 4 8"

extract_rhythms :: [Types.Time] -> Text
extract_rhythms =
    Text.unwords . map (Types.to_lily . expect1 . Types.time_to_note_durs)

expect1 :: (CallStack.Stack, Show a) => [a] -> a
expect1 [x] = x
expect1 xs = errorStack $ "expected only one element: " <> showt xs
