module Perform.Lilypond.Lilypond_test where
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import qualified System.Process as Process

import Util.Control
import qualified Util.Pretty as Pretty
import Util.Test

import qualified Ui.UiTest as UiTest
import qualified Cmd.Cmd as Cmd
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score

import qualified Perform.Lilypond.Convert as Convert
import qualified Perform.Lilypond.Lilypond as Lilypond
import qualified Perform.Pitch as Pitch

import Types


config :: Lilypond.Config
config = Lilypond.Config False []

test_convert_notes = do
    let f sig = map Lilypond.to_lily . Lilypond.convert_notes config sig whole
            . map mkevent
        s44 = sig 4 4
    equal (f s44 [(0, 1, "a"), (1, 1, "b")]) ["a4", "b4", "r2"]

    -- Rests are not dotted, even when they could be.
    equal (f s44 [(0, 1, "a"), (1.5, 1, "b")])
        ["a4", "r8", "b8~", "b8", "r8", "r4"]
    equal (f s44 [(0, 2, "a"), (3.5, 0.25, "b"), (3.75, 0.25, "c")])
        ["a2", "r4", "r8", "b16", "c16"]
    equal (f s44 [(0, 0.5, "a"), (0.5, 1, "b"), (1.5, 0.5, "c")])
        ["a8", "b4", "c8", "r2"]

test_chords = do
    let f = map Lilypond.to_lily . Lilypond.convert_notes config (sig 4 4) whole
            . map mkevent
    -- Homogenous durations.
    equal (f [(0, 1, "a"), (0, 1, "c")])
        ["<a c>4", "r4", "r2"]
    -- Starting at the same time.
    equal (f [(0, 2, "a"), (0, 1, "c")])
        ["<a c>4~", "a4", "r2"]
    -- Starting at different times.
    equal (f [(0, 2, "a"), (1, 1, "c")])
        ["a4~", "<a c>4", "r2"]
    equal (f [(0, 2, "a"), (1, 2, "c"), (2, 2, "e")])
        ["a4~", "<a c>4~", "<c e>4~", "e4"]

test_convert_duration = do
    let f sig pos = Lilypond.to_lily $ head $
            Lilypond.convert_duration sig True pos
                (Lilypond.time_per_whole - pos)
    equal (map (f (sig 4 4)) [0, 8 .. 127])
        [ "1", "8.", "4.", "16", "2.", "8.", "8", "16"
        -- mid-measure
        , "2", "8.", "4.", "16", "4", "8.", "8", "16"
        ]
    equal (map (f (sig 2 4)) [0, 8 .. 127]) $
        concat $ replicate 2 ["2", "8.", "4.", "16", "4", "8.", "8", "16"]

test_make_ly = do
    let ((score, smap), staves, events) = run (mkmeta "title" "treble" "4/4")
            -- complicated rhythm
            [ ("s/i1", [(0, 1, "4c"), (1.5, 2, "4d#")], [])
            -- rhythm starts after 0, long multi measure note
            , ("s/i2", [(1, 1, "4g"), (2, 12, "3a")], [])
            ]
        extract_staff (clef, inst, measures) = (clef, Lilypond.inst_name inst,
            map (map Lilypond.to_lily) measures)
    equal (map extract_staff staves)
        [ ("treble", "i1",
            [["c'4", "r8", "ds'8~", "ds'4.", "r8"], ["r1"], ["r1"], ["r1"]])
        , ("treble", "i2",
            [["r4", "g'4", "a2~"], ["a1~"], ["a1~"], ["a2", "r2"]])
        ]
    -- compile_ly score
    Pretty.pprint events
    Text.IO.putStrLn $ mconcat score
    pprint smap


-- * util

compile_ly :: [Text.Text] -> IO ()
compile_ly score = do
    Text.IO.writeFile "build/test/test.ly" (mconcat score)
    void $ Process.rawSystem
        "lilypond" ["-o", "build/test/test", "build/test/test.ly"]

read_note :: String -> Lilypond.Note
read_note text
    | pitch == "r" = Lilypond.rest dur
    | otherwise = Lilypond.Note [pitch] dur (tie == "~") "" Nothing
    where
    (pitch, rest) = break Char.isDigit text
    (dur_text, tie) = break (=='~') rest
    Just dur = flip Lilypond.NoteDuration False <$>
        Lilypond.read_duration dur_text

mkmeta :: String -> String -> String -> Map.Map String String
mkmeta title clef sig = Map.fromList
    [ (Lilypond.meta_ly, "")
    , (Lilypond.meta_title, title)
    , (Lilypond.meta_clef, clef)
    , (Lilypond.meta_time_signature, sig)
    ]

run :: Map.Map String String -> [UiTest.NoteSpec]
    -> (([Text.Text], Cmd.StackMap), [Lilypond.Staff], [Lilypond.Event])
run meta notes = (Lilypond.make_ly config score events, staves, events)
    where
    staves = Lilypond.make_staves config (Lilypond.score_clef score) sig events
    sig = Lilypond.score_time score
    res = DeriveTest.derive_tracks (concatMap UiTest.note_spec notes)
    (events, _logs) = LEvent.partition $ Convert.convert 1 (Derive.r_events res)
    Just (Right score) = Lilypond.meta_to_score (Just (Pitch.Key "d-min")) meta

mkevent :: (RealTime, RealTime, String) -> Lilypond.Event
mkevent (start, dur, pitch) = mkevent_inst (start, dur, pitch, "")

mkevent_inst :: (RealTime, RealTime, String, String) -> Lilypond.Event
mkevent_inst (start, dur, pitch, inst) =
    Lilypond.Event
        { Lilypond.event_start = Convert.real_to_time 1 start
        , Lilypond.event_duration = Convert.real_to_time 1 dur
        , Lilypond.event_pitch = pitch
        , Lilypond.event_instrument = Score.Instrument inst
        , Lilypond.event_dynamic = 0.5
        , Lilypond.event_stack = UiTest.mkstack (1, 0, 1)
        }

sig :: Int -> Int -> Lilypond.TimeSignature
sig num denom = Lilypond.TimeSignature num dur
    where Just dur = Lilypond.read_duration (show denom)

whole :: Lilypond.Time
whole = Lilypond.time_per_whole
