module Perform.Lilypond.Lilypond_test where
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified System.Process as Process

import Util.Control
import qualified Util.Pretty as Pretty
import Util.Test

import qualified Ui.UiTest as UiTest
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.LEvent as LEvent

import qualified Perform.Lilypond.Convert as Convert
import qualified Perform.Lilypond.Lilypond as Lilypond
import Perform.Lilypond.Lilypond (Duration(..))
import qualified Perform.Pitch as Pitch

import Types


test_convert_notes = do
    let f sig = map Lilypond.to_lily . Lilypond.convert_notes sig . map mkevent
        s44 = sig 4 4
    equal (f s44 [(0, 1, "a"), (1, 1, "b")])
        ["a4", "b4", "r2"]
    equal (f s44 [(0, 1, "a"), (1.5, 1, "b")])
        ["a4", "r8", "b8~", "b8", "r8", "r4"]

test_allowed_time = do
    let f sig = Lilypond.time_to_dur . Lilypond.allowed_time sig
        times = enumFromThenTo 0 16
    -- Signature is divisible, so it can use whole and half notes.
    equal (map (f (sig 4 4)) (times 128)) [D1, D8, D4, D8, D2, D8, D4, D8, D1]
    -- Also divisible, but the measure is too short for whole notes.
    equal (map (f (sig 2 4)) (times 64)) [D2, D8, D4, D8, D2]
    -- Not divisible, quarter is the largest note.
    equal (map (f (sig 3 4)) (times 128)) [D4, D8, D4, D8, D4, D8, D4, D8, D4]

test_take_tied_until = do
    let f until = extract . Lilypond.take_tied_until True until
            . map read_note
        extract (pre, post) =
            (map Lilypond.to_lily pre, map Lilypond.to_lily post)
    equal (f 500 ["c1~", "c2", "c4"]) (["c1~", "c2"], ["c4"])
    equal (f 5 ["c1~", "c2", "c4"]) (["c1~"], ["c2", "c4"])
    equal (f 500 ["r1", "r2", "c1"]) (["r1", "r2"], ["c1"])
    equal (f 5 ["r1", "r2", "c1"]) (["r1"], ["r2", "c1"])

test_measure_to_whole = do
    let f pos = map Lilypond.to_lily
            . Lilypond.measure_to_whole Lilypond.time_per_whole pos
            . map read_note
    -- Simplified, but only within one measure.
    equal (f 0 ["c2~", "c2", "c2"]) ["c1", "c2"]
    -- If the last note has a tie, keep it.
    equal (f 0 ["c2~", "c2~", "c2"]) ["c1~", "c2"]
    -- Note has to take up the whole measure.
    equal (f 0 ["c2~", "c4", "d4"]) ["c2~", "c4", "d4"]
    -- If I'm not at a the start of a measure, then don't simplify.
    equal (f 5 ["c2~", "c2~", "c2"]) ["c2~", "c2~", "c2"]
    -- Rests simplify too.
    equal (f 0 ["r2", "r2", "r2"]) ["r1", "r2"]

test_make_score = do
    let (score, notes, events) = run (mkmeta "title" "treble" "4/4")
            ("", [(0, 1, "4c"), (1.5, 2, "4d#")], [])
    equal notes ["c'4", "r8", "ds'8~", "ds'4~", "ds'8", "r8"]
    -- compile_ly score
    pprint events
    pprint score


-- * util

compile_ly :: Pretty.Doc -> IO ()
compile_ly score = do
    writeFile "build/test/test.ly" (Pretty.formatted score)
    void $ Process.rawSystem
        "lilypond" ["-o", "build/test/test", "build/test/test.ly"]

read_note :: String -> Lilypond.Note
read_note text
    | pitch == "r" = Lilypond.rest dur
    | otherwise = Lilypond.note pitch dur (tie == "~")
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

run :: Map.Map String String -> UiTest.NoteSpec
    -> (Pretty.Doc, [String], [Lilypond.Event])
run meta note_spec =
    (Lilypond.make_score score events,
        map Lilypond.to_lily (Lilypond.convert_notes sig events),
        events)
    where
    sig = Lilypond.score_time score
    res = DeriveTest.derive_tracks (UiTest.note_spec note_spec)
    (events, _logs) = LEvent.partition $ Convert.convert Lilypond.D4
        (Derive.r_events res)
    Just (Right score) = Lilypond.meta_to_score (Just (Pitch.Key "d-min")) meta

score0 = Lilypond.make_score
    (Lilypond.Score "hi there" (sig 3 4) "treble" ("d", Lilypond.Major)
        Lilypond.D4)
    [Lilypond.Event start dur pitch | (start, dur, pitch) <-
        [(0, 4, "a"), (4, 4, "b"), (16, 2, "a"), (18, 2, "b")]]


mkevent :: (RealTime, RealTime, String) -> Lilypond.Event
mkevent (start, dur, pitch) =
    Lilypond.Event (Convert.real_to_time Lilypond.D4 start)
        (Convert.real_to_time Lilypond.D4 dur) pitch

sig :: Int -> Int -> Lilypond.TimeSignature
sig num denom = Lilypond.TimeSignature num dur
    where Just dur = Lilypond.read_duration (show denom)
