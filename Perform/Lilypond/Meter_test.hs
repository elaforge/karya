module Perform.Lilypond.Meter_test where
import Util.Test
import Perform.Lilypond.LilypondTest (mkmeter)
import qualified Perform.Lilypond.Meter as Meter
import qualified Perform.Lilypond.Types as Types
import Perform.Lilypond.Types (Duration(..))


test_convert_duration = do
    let f meter pos = Types.to_lily $ head $
            Meter.convert_duration (mkmeter meter) True False
                pos (Types.time_per_whole - pos)
    equal (map (f "4/4") [0, 8 .. 127])
        [ "1", "8.", "4.", "16", "2.", "8.", "8", "16"
        -- mid-measure
        , "2", "8.", "4.", "16", "4", "8.", "8", "16"
        ]
    equal (map (f "2/4") [0, 8 .. 127]) $
        concat $ replicate 2 ["2", "8.", "4.", "16", "4", "8.", "8", "16"]

test_allowed_time_greedy = do
    let f meter = extract_rhythms
            . map (Meter.allowed_time_greedy True (mkmeter meter))
        t = Types.dur_to_time

    -- 4/4, being duple, is liberal about spanning beats, since it uses rank-2.
    equal (f "4/4" [0, t D4 .. 4 * t D4]) "1 2. 2 4 1"
    equal (f "4/4" [0, t D8 .. 8 * t D8])
        "1 4. 2. 8 2 4. 4 8 1"

    -- 6/8 is more conservative.
    equal (f "3+3/8" [0, t D8 .. 6 * t D8])
        "2. 4 8 4. 4 8 2."

    -- Irregular meters don't let you break the middle dividing line no matter
    -- what, because 'Meter.find_rank' always stops at rank 0.
    equal (f "3+2/4" [0, t D8 .. 10 * t D8])
        "2. 8 2 8 4 8 2 8 4 8 2."
        -- This has 8 after the middle 2 instead of 4. like 4/4 would have.
        -- That's because it's not duple, so it's more conservative.
        -- I guess that's probably ok.

test_allowed_time_best = do
    let f use_dot meter = extract_rhythms
            . map (Meter.allowed_time_best use_dot (mkmeter meter))
        t = Types.dur_to_time
    equal (f False "4/4" [0, t D4 .. 4 * t D4])
        "1 4 2 4 1"
    equal (f False "4/4" [0, t D8 .. 8 * t D8])
        "1 8 4 8 2 8 4 8 1"
    equal (f True "3+3/8" [0, t D8 .. 6 * t D8])
        "2. 4 8 4. 4 8 2."

extract_rhythms :: [Types.Time] -> String
extract_rhythms = unwords
        . map (Types.to_lily . expect1 . Types.time_to_note_durs)
    where
    expect1 [x] = x
    expect1 xs = error $ "expected only one element: " ++ show xs
