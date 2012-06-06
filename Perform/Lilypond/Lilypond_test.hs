module Perform.Lilypond.Lilypond_test where
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
import qualified Perform.Pitch as Pitch

import Types


test_convert_notes = do
    let f sig = Lilypond.notes_to_lily
            . Lilypond.convert_notes sig . map mkevent
        s44 = sig 4 4
    equal (f s44 [(0, 1, "a"), (1, 1, "b")])
        ["a4", "b4", "r2"]
    equal (f s44 [(0, 1, "a"), (1.5, 1, "b")])
        ["a4", "r8", "b8~", "b8", "r8", "r4"]

test_allowed_time = do
    let f sig = (\(Lilypond.Duration d) -> d) . Lilypond.time_to_dur
            . Lilypond.allowed_time sig
        times = enumFromThenTo 0 16
    -- Signature is divisible, so it can use whole and half notes.
    equal (map (f (sig 4 4)) (times 128)) [1, 8, 4, 8, 2, 8, 4, 8, 1]
    -- Also divisible, but the measure is too short for whole notes.
    equal (map (f (sig 2 4)) (times 64))
        [4, 8, 4, 8, 4]
    -- Not divisible, quarter is the largest note.
    equal (map (f (sig 3 4)) (times 128)) [4, 8, 4, 8, 4, 8, 4, 8, 4]

test_make_score = do
    let (score, notes, events) = run (mkmeta "title" "treble" "4/4")
            ("", [(0, 1, "4c"), (1.5, 2, "4d#")], [])
    equal notes ["c'4", "r8", "ds'8~", "ds'4~", "ds'8", "r8"]
    -- compile_ly score
    pprint events
    pprint score

compile_ly :: Pretty.Doc -> IO ()
compile_ly score = do
    writeFile "build/test/test.ly" (Pretty.formatted score)
    void $ Process.rawSystem
        "lilypond" ["-o", "build/test/test", "build/test/test.ly"]

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
        Lilypond.notes_to_lily (Lilypond.convert_notes sig events),
        events)
    where
    sig = Lilypond.score_time score
    res = DeriveTest.derive_tracks (UiTest.note_spec note_spec)
    (events, _logs) = LEvent.partition $ Convert.convert Lilypond.quarter
        (Derive.r_events res)
    Just (Right score) = Lilypond.meta_to_score (Just (Pitch.Key "d-min")) meta

score0 = Lilypond.make_score
    (Lilypond.Score "hi there" (sig 3 4) "treble" ("d", Lilypond.Major))
    [Lilypond.Event start dur pitch | (start, dur, pitch) <-
        [(0, 4, "a"), (4, 4, "b"), (16, 2, "a"), (18, 2, "b")]]


mkevent :: (RealTime, RealTime, String) -> Lilypond.Event
mkevent (start, dur, pitch) =
    Lilypond.Event (Convert.real_to_time Lilypond.quarter start)
        (Convert.real_to_time Lilypond.quarter dur) pitch

sig :: Int -> Int -> Lilypond.TimeSignature
sig num denom = Lilypond.TimeSignature num (Lilypond.Duration denom)
