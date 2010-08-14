module Derive.Note_test where
import qualified Data.Map as Map

import Util.Test
import qualified Util.Log as Log
import qualified Util.Seq as Seq

import Ui

import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score

import qualified Perform.Signal as Signal


-- * derivers

test_c_note = do
    -- Test basic Derive.d_note_track plumbing and the note (null) deriver
    -- along with it.
    let run title evts = extract_nostack $
            DeriveTest.derive_tracks_tempo [(title, evts)]
    let inst = Just "i"

    let evt s d = mkevent s d "" inst []
    equal (run ">i" [(0, 1, ""), (1, 2, ""), (3, 3, "")])
        (Right [evt 0 1, evt 1 2, evt 3 3], [])

    let (evts, logs) = (run ">i" [(0, 1, "n +a 42")])
    equal evts (Right [])
    strings_like logs ["expected inst or attr"]

    let (evts, logs) = run ">i" [(0, 1, ")parse/error")]
    equal evts (Right [])
    strings_like logs ["parse error"]

    -- title error throws exception
    left_like (fst (run ">i $parse/err" [(0, 1, "")])) "parse error"

    -- comment only event is filtered out
    equal (run ">i" [(0, 1, "--")]) (Right [], [])
    equal (run ">" [(0, 1, "n >i +a")])
        (Right [mkevent 0 1 "n >i +a" inst ["a"]], [])
    equal (run ">i +a" [(0, 1, "")])
        (Right [mkevent 0 1 "" inst ["a"]], [])

    -- event overrides attrs
    equal (run "> +a" [(0, 1, "n =b"), (1, 1, "n -a")])
        (Right [mkevent 0 1 "n =b" Nothing ["b"],
            mkevent 1 1 "n -a" Nothing []],
        [])
    -- alternate syntax
    equal (run ">i" [(0, 1, ""), (1, 1, "n >i2 |")])
        (Right [mkevent 0 1 "" inst [],
            mkevent 1 1 "n >i2 |" (Just "i2") []],
        [])

test_c_block = do
    -- This also tests Derive.Call.Note.lookup_note_call
    let run evts = extract_nostack $ DeriveTest.derive_blocks
            [ ("b1", [(">", evts)])
            , ("sub", [(">", [(0, 22, "--sub")])])
            ]
    let (evts, logs) = run [(0, 1, "nosuch")]
    equal evts (Right [])
    strings_like logs ["call not found: nosuch"]

    strings_like (snd (run [(0, 1, "sub >arg")]))
        ["args for block call not implemented yet"]

    -- subderived stuff is stretched and placed, inherits instrument
    let (evts, logs) = run [(0, 1, "sub"), (1, 2, "n >i +a | sub")]
    equal logs []
    equal evts $ Right
        [ mkevent 0 1 "--sub" Nothing []
        , mkevent 1 2 "--sub" (Just "i") ["a"]
        ]

test_c_equal = do
    -- Test the '=' call, but also test the special parsing Derive.Note deriver
    -- eval in general.
    let e_evt e = (Score.event_start e, Score.event_instrument e,
            Score.attrs_list (Score.event_attributes e))
    let do_run e_evts title evts =
            DeriveTest.extract e_evts Log.msg_string $
                DeriveTest.derive_tracks_tempo [(title, evts)]
    let inst = Just . Score.Instrument

    let run = do_run e_evt
    -- log stack should be at the track level
    let (evts, logs) = run "> | inst = inst" [(0, 1, "")]
    left_like evts $
        "[(bid \"test/b1\") / (tid \"test/b1.t0\") / (tid \"test/b1.t1\") "
        ++ "/ note / equal]*expected Instrument"
    equal logs []

    -- only the event with the error is omitted
    let (evts, logs) = run ">" [(0, 1, "inst = inst |"), (1, 1, "")]
    equal evts (Right [(1, Nothing, [])])
    strings_like logs ["expected Instrument"]

    let run = do_run e_evt
    -- works as "generator"
    equal (run ">i" [(0, 0, "inst = >i2"), (1, 1, ""),
            (2, 0, "inst = >i3"), (3, 1, "")])
        (Right [(1, inst "i2", []), (3, inst "i3", [])], [])

    equal (run ">i +a" [(0, 0, "attr = +b"), (1, 1, ""),
            (2, 0, "attr = -a"), (3, 1, "")])
        (Right [(1, inst "i", ["a", "b"]), (3, inst "i", ["b"])], [])

    -- works as transformer
    equal (run ">i" [(0, 1, ""), (1, 1, "inst = >i2 |"), (2, 1, "n >i3 |")])
        (Right [(0, inst "i", []), (1, inst "i2", []), (2, inst "i3", [])], [])

test_environ_across_tracks = do
    let e_evt = fmap Signal.unsignal . Map.lookup (Score.Control "cont")
            . Score.event_controls
    let run tracks = DeriveTest.extract e_evt Log.msg_string $
            DeriveTest.derive_tracks_tempo ((">", [(0, 10, "")]) : tracks)

    -- first make sure srate works as I expect
    let interpolated = [(0, 0), (1, 0.25), (2, 0.5), (3, 0.75), (4, 1)]
    equal (run [("cont", [(0, 0, "0"), (4, 0, "i 1")])])
        (Right [Just interpolated], [])
    equal (run [("cont | srate = 2", [(1, 0, "0"), (5, 0, "i 1")])])
        (Right [Just [(1, 0), (3, 0.5), (5, 1)]], [])
    equal (run [("cont", [(0, 0, "srate = 2"), (1, 0, "0"), (5, 0, "i 1")])])
        (Right [Just [(1, 0), (3, 0.5), (5, 1)]], [])

    -- now make sure srate in one track doesn't affect another
    let cont = ("cont", [(0, 0, "0"), (4, 0, "i 1")])
    equal (run [("cont2", [(0, 0, "srate = 2")]), cont])
        (Right [Just interpolated], [])
    equal (run [cont, ("cont2", [(0, 0, "srate = 2")])])
        (Right [Just interpolated], [])
    equal (run [("cont2 | srate = 2", []), cont])
        (Right [Just interpolated], [])
    equal (run [cont, ("cont2 | srate = 2", [])])
        (Right [Just interpolated], [])

test_call_errors = do
    let extract r = case DeriveTest.extract DeriveTest.e_event id r of
            (Right val, []) -> Right val
            (Left err, []) -> Left err
            (_, logs) -> Left (Seq.join "\n" (map Log.msg_string logs))

    let run_title title = extract $
            DeriveTest.derive_tracks_tempo [(title, [(0, 1, "--1")])]
    left_like (run_title ">i | no-such-call") "call not found: no-such-call"
    left_like (run_title ">i | delay *bad-arg") "expected Control but got"
    left_like (run_title ">i | delay 1 2 3 4") "too many arguments"
    left_like (run_title ">i | delay") "not in environment and no default given"
    left_like (run_title ">i | delay _") "not in environment"
    left_like (run_title ">i | delay %delay") "not in environment"

    let run_evt evt = extract $
            DeriveTest.derive_tracks_tempo [(">i", [(0, 1, evt)])]
    left_like (run_evt "no-such-call")
        "call not found: no-such-call"
    left_like (run_evt "abs-trill")
        "non-generator in generator position: absolute_trill"
    left_like (run_evt "abs-trill |")
        "ArgError: too few arguments"
    equal (run_evt "delay 2 | abs-trill 2 |")
        (Right [(2, 1, "delay 2 | abs-trill 2 |")])

test_environ_default = do
    -- Mostly tested in TrackLang_test, but also make sure c_equal and and
    -- track evaluation and the environ default all work together.
    let extract = DeriveTest.extract DeriveTest.e_event Log.msg_string
    let run evts = extract $ DeriveTest.derive_tracks_tempo [(">i", evts)]
    equal (run [(0, 0, "delay-time = 0"), (1, 1, "delay |"),
            (2, 0, "delay-time = 1"), (3, 1, "delay |")])
        (Right [(1, 1, "delay |"), (4, 1, "delay |")], [])

type Extracted =
    (RealTime, RealTime, String, Maybe Score.Instrument, Score.Attributes)

extract_nostack :: Derive.DeriveResult [Score.Event]
    -> (Either String [Extracted], [String])
extract_nostack = DeriveTest.extract extract_event Log.msg_string
    where
    -- | Events aren't in Eq, so extract the bits I want to test.
    extract_event e = (Score.event_start e, Score.event_duration e,
        Score.event_string e, Score.event_instrument e,
        Score.event_attributes e)

mkevent :: RealTime -> RealTime -> String -> Maybe String -> [String]
    -> Extracted
mkevent start dur text inst attrs =
    (start, dur, text, fmap Score.Instrument inst, Score.attributes attrs)
