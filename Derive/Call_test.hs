module Derive.Call_test where
import qualified Data.Map as Map

import Util.Test
import qualified Util.Log as Log
import qualified Util.Seq as Seq

import qualified Derive.CallSig as CallSig
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Derive as Derive
import qualified Derive.TrackLang as TrackLang
import qualified Derive.Score as Score
import qualified Derive.Call.CallTest as CallTest

import qualified Perform.PitchSignal as PitchSignal
import qualified Perform.Signal as Signal


test_c_block = do
    -- This also tests Derive.Call.Note.lookup_note_call
    let run evts = DeriveTest.extract DeriveTest.e_everything Log.msg_string $
            DeriveTest.derive_blocks
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
        [ (0, 1, "--sub", Nothing, [])
        , (1, 2, "--sub", Just "i", ["a"])
        ]

test_c_equal = do
    -- Test the '=' call, but also test the special parsing Derive.Note deriver
    -- eval in general.
    let run title evts = DeriveTest.extract extract Log.msg_string $
            DeriveTest.derive_tracks_tempo [(title, evts)]
        extract e = (s, inst, attrs)
            where (s, _, _, inst, attrs) = DeriveTest.e_everything e

    -- log stack should be at the track level
    let (evts, logs) = run "> | inst = inst" [(0, 1, "")]
    equal evts (Right [])
    strings_like logs ["expected Instrument"]

    -- only the event with the error is omitted
    let (evts, logs) = run ">" [(0, 1, "inst = inst |"), (1, 1, "")]
    equal evts (Right [(1, Nothing, [])])
    strings_like logs ["expected Instrument"]

    equal (run ">i" [(0, 1, ""), (1, 1, "inst = >i2 |"), (2, 1, "n >i3 |")])
        (Right [(0, Just "i", []), (1, Just "i2", []), (2, Just "i3", [])], [])

test_assign_controls = do
    let run inst_title cont_title val = extract $ DeriveTest.derive_tracks
            [ (inst_title, [(0, 1, "")])
            , (cont_title, [(0, 0, val)])
            ]
        extract = DeriveTest.extract e_event Log.msg_string
        e_event e = (head (PitchSignal.unsignal_degree (Score.event_pitch e)),
            fmap head $ DeriveTest.e_control "cont" e)

    -- normal
    equal (run ">i" "cont" "1")
        (Right [((0, 60), Just (0, 1))], [])
    -- not seen
    equal (run ">i" "gont" "1")
        (Right [((0, 60), Nothing)], [])

    -- a non-existent control with no default is an error
    let (events, logs) = run ">i | %cont = %bonk" "gont" "1"
    equal events (Right [])
    strings_like logs ["not found: Control \"bonk\""]
    -- control assigned
    equal (run ">i | %cont = %gont" "gont" "1")
        (Right [((0, 60), Just (0, 1))], [])

    -- set a constant signal
    equal (run ">i | %cont = 42" "gont" "1")
        (Right [((0, 60), Just (0, 42))], [])
    -- set constant signal with a default
    equal (run ">i | %cont = %gont,42" "bonk" "1")
        (Right [((0, 60), Just (0, 42))], [])
    equal (run ">i | %cont = %gont,42" "gont" "1")
        (Right [((0, 60), Just (0, 1))], [])

    -- named pitch doesn't show up
    equal (run ">i" "*twelve #foo" "2c")
        (Right [((0, 60), Nothing)], [])
    -- assigned to default pitch, so it shows up
    equal (run ">i | # = #foo" "*twelve #foo" "2c")
        (Right [((0, 36), Nothing)], [])
    -- set constant pitch
    equal (run ">i | # = (1c)" "*twelve #foo" "2c")
        (Right [((0, 24), Nothing)], [])

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

    -- now make sure srate in one track doesn't affect another
    let cont = ("cont", [(0, 0, "0"), (4, 0, "i 1")])
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
    left_like (run_title ">i | delay") "not found and no default"
    left_like (run_title ">i | delay _") "not found and no default"
    left_like (run_title ">i | delay %delay") "not found and no default"

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

test_val_call = do
    let extract = DeriveTest.extract e_event Log.msg_string
        e_event = fmap Signal.unsignal . Map.lookup (Score.Control "cont")
            . Score.event_controls
    let run evt = extract $ DeriveTest.derive_tracks_with with_add1
            [(">", [(0, 1, "")]), ("cont", [(0, 0, evt)])]
    equal (run "foobar")
        (Right [Just []], ["DeriveError: call not found: foobar"])
    equal (run "set 1")
        (Right [Just [(0, 1)]], [])
    equal (run "set (add1 1)")
        (Right [Just [(0, 2)]], [])
    equal (run "set (add1 (add1 1))")
        (Right [Just [(0, 3)]], [])
    let (res, logs) = run "set (add1 1 2)" in do
        equal res (Right [Just []])
        strings_like logs ["too many arguments"]

with_add1 = CallTest.with_val_call "add1" add_one

add_one :: Derive.ValCall
add_one = Derive.ValCall "add" $ \args -> CallSig.call1 args
    (CallSig.required "v") $ \val -> return (TrackLang.VNum (val + 1))
