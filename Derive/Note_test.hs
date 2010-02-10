module Derive.Note_test where

import Util.Test
import qualified Util.Log as Log
import qualified Util.Seq as Seq

import Ui
-- import qualified Ui.State as State
import qualified Ui.UiTest as UiTest

import qualified Derive.DeriveTest as DeriveTest
-- import qualified Derive.Note as Note
import qualified Derive.Score as Score

-- import qualified Perform.Pitch as Pitch
import qualified Perform.Warning as Warning


-- * derivers

test_c_note = do
    -- Test basic Derive.d_note_track plumbing and the note (null) deriver
    -- along with it.
    let run title evts = extract_nostack $
            DeriveTest.derive_tracks_tempo [(title, evts)]
    let inst = Just "i"

    let (evts, logs) = (run ">i" [(0, 1, "+a 42")])
    equal evts (Right [])
    strings_like logs ["expected inst or attr"]
    let (evts, logs) = run ">i" [(0, 1, "parse/error")]
    equal evts (Right [])
    strings_like logs ["parse error"]
    -- title error throws exception
    left_like (fst (run ">i parse/err" [(0, 1, "")])) "parse error"

    -- comment only event is filtered out
    equal (run ">i" [(0, 1, "--")]) (Right [], [])
    equal (run ">" [(0, 1, ">i +a")])
        (Right [mkevent 0 1 ">i +a" [] inst ["a"]], [])
    equal (run ">i +a" [(0, 1, "")])
        (Right [mkevent 0 1 "" [] inst ["a"]], [])

    -- event overrides attrs
    equal (run "> +a" [(0, 1, "=b"), (1, 1, "-a")])
        (Right [mkevent 0 1 "=b" [] Nothing ["b"],
            mkevent 1 1 "-a" [] Nothing []],
        [])
    -- alternate syntax
    equal (run ">i" [(0, 1, ""), (1, 1, ">i2 |")])
        (Right [mkevent 0 1 "" [] inst [],
            mkevent 1 1 ">i2 |" [] (Just "i2") []],
        [])

test_c_block = do
    -- This also tests Basic.lookup_note_call
    let run evts = extract_common $ DeriveTest.derive_blocks
            [ ("b1", [(">", evts)])
            , ("sub", [(">", [(0, 22, "--sub")])])
            ]
    let (evts, logs) = run [(0, 1, "nosuch")]
    equal evts (Right [])
    strings_like (map fst logs) ["CallNotFound: nosuch"]
    equal (map snd logs) [Just (mkstack [("b1", "b1.t0", (0, 1))])]

    strings_like (map fst (snd (run [(0, 1, "sub >arg")])))
        ["args for block call not implemented yet"]

    -- subderived stuff is stretched and placed, inherits instrument
    let (evts, logs) = run [(0, 1, "sub"), (1, 2, ">i +a | sub")]
    equal logs []
    equal evts $ Right
        [ mkevent 0 1 "--sub"
            [("sub", "sub.t0", (0, 22)), ("b1", "b1.t0", (0, 1))]
            Nothing []
        , mkevent 1 2 "--sub"
            [("sub", "sub.t0", (0, 22)), ("b1", "b1.t0", (1, 3))]
            (Just "i") ["a"]
        ]

test_c_equal = do
    -- Test the '=' call, but also test the special parsing Derive.Note deriver
    -- eval in general.
    let e_evt e = (Score.event_start e, Score.event_instrument e,
            Score.attrs_list (Score.event_attributes e))
    let do_run e_evts e_logs title evts = DeriveTest.extract e_evts e_logs $
            DeriveTest.derive_tracks_tempo [(title, evts)]
    let inst = Just . Score.Instrument

    let run = do_run e_evt id
    -- log stack should be at the track level
    let (evts, logs) = run "> | inst = inst" [(0, 1, "")]
    equal evts (Right [])
    equal (map Log.msg_stack logs)
        [Just (mk_track_stack [("b1", "b1.t1")])]
    strings_like (map Log.msg_string logs) ["expected TInstrument"]

    let (evts, logs) = run "> | 42 = >inst" [(0, 1, "")]
    equal evts (Right [])
    strings_like (map Log.msg_string logs)
        ["arg 0/symbol: expected type Symbol but got"]

    -- only the event with the error is omitted
    let (evts, logs) = run ">" [(0, 1, "inst = inst |"), (1, 1, "")]
    equal evts (Right [(1, Nothing, [])])
    strings_like (map Log.msg_string logs)
        ["expected TInstrument"]

    let run = do_run e_evt Log.msg_string
    -- works as "generator"
    equal (run ">i" [(0, 0, "inst = >i2"), (1, 1, ""),
            (2, 0, "inst = >i3"), (3, 1, "")])
        (Right [(1, inst "i2", []), (3, inst "i3", [])], [])

    equal (run ">i +a" [(0, 0, "attr = +b"), (1, 1, ""),
            (2, 0, "attr = -a"), (3, 1, "")])
        (Right [(1, inst "i", ["a", "b"]), (3, inst "i", ["b"])], [])

    -- works as transformer
    equal (run ">i" [(0, 1, ""), (1, 1, "inst = >i2 |"), (2, 1, ">i3 |")])
        (Right [(0, inst "i", []), (1, inst "i2", []), (2, inst "i3", [])], [])

test_calls = do
    let extract r = case DeriveTest.extract DeriveTest.e_event id r of
            (Right val, []) -> Right val
            (Left err, []) -> Left err
            (_, logs) -> Left (Seq.join "\n" (map Log.msg_string logs))
    let run title = extract $
            DeriveTest.derive_tracks_tempo [(title, [(0, 1, "--1")])]

    left_like (run ">i | no-such-call") "CallNotFound"
    left_like (run ">i | delay *bad-arg")
        "expected type Signal but got"
    left_like (run ">i | delay 1 2 3 4")
        "too many arguments"
    left_like (run ">i | delay")
        "not in environment and no default given"
    left_like (run ">i | delay _")
        "not in environment"
    left_like (run ">i | delay %delay") $
        "not in environment"

test_environ_default = do
    -- Mostly tested in TrackLang_test, but also make sure c_equal and and
    -- track evaluation and the environ default all work together.
    let extract = DeriveTest.extract DeriveTest.e_event Log.msg_string
    let run evts = extract $ DeriveTest.derive_tracks_tempo [(">i", evts)]
    equal (run [(0, 0, "delay-time = 0"), (1, 1, "delay |"),
            (2, 0, "delay-time = 1"), (3, 1, "delay |")])
        (Right [(1, 1, "delay |"), (4, 1, "delay |")], [])

{-
-- this is all broken at the moment until I re-implement duration calculation
-- test_negative_duration = do
    let extract = DeriveTest.extract (\e -> DeriveTest.e_event e) Log.msg_string
    let run evts = extract $ DeriveTest.derive_tracks_tempo
            [(DeriveTest.default_inst_title, evts)]

    let deflt = Note.negative_duration_default
    -- events get lined up
    equal (run [(1, -1, "--1"), (3, -2, "--2")])
        (Right [(1, 2, "--1"), (3, deflt, "--2")], [])
    -- rest
    equal (run [(1, -1, "--1"), (3, -1, "--2")])
        (Right [(1, 1, "--1"), (3, deflt, "--2")], [])
    -- 0 dur is omitted
    equal (run [(1, -1, "--1"), (3, 0, "--2")])
        (Right [(1, 2, "--1")],
            ["compile (bid \"test/b1\"): omitting note with 0 duration"])

    -- TODO these won't work properly until durations can be calculated
    -- properly, see comment in Derive.Note.calculate_duration.
    -- let run evts = extract $ DeriveTest.derive_blocks
    --         [ ("b1", [(">", evts)])
    --         , ("sub", [(">", [(1, -1, "--1"), (2, -1, "--2")])])
    --         ]
    -- -- last event extends up to "rest" at 5
    -- equal (run [(4, -4, "sub"), (6, -1, "")])
    --     (Right [(2, 2, "--1"), (4, 1, "--2"), (6, deflt, "")], [])

    -- TODO This will come out incorrect because I don't pass the correct next
    -- event.  Punt on this for now, I may have a better solution later.
    -- equal (run [(4, -4, "sub"), (8, -4, "sub")])
    --     (Right [(2, 2, "--1"), (4, 2, "--2"), (6, 2, "--1"),
    --         (8, deflt, "--2")],
    --     [])
-}


type Extracted =
    (RealTime, RealTime, String, Warning.Stack, Maybe Score.Instrument,
        Score.Attributes)

extract_common :: DeriveTest.Result [Score.Event]
    -> (Either String [Extracted], [(String, Maybe Warning.Stack)])
extract_common = DeriveTest.extract extract_event extract_log
    where
    extract_log msg = (Log.msg_string msg, Log.msg_stack msg)
    -- | Events aren't in Eq, so extract the bits I want to test.
    extract_event e = (Score.event_start e, Score.event_duration e,
        Score.event_string e, Score.event_stack e, Score.event_instrument e,
        Score.event_attributes e)

extract_nostack :: DeriveTest.Result [Score.Event]
    -> (Either String [Extracted], [String])
extract_nostack = f . extract_common
    where
    f (result, logs) =
        (fmap (map (\(a, b, c, _, d, e) -> (a, b, c, [], d, e))) result,
            map fst logs)

mkevent :: RealTime -> RealTime -> String
    -> [(String, String, (ScoreTime, ScoreTime))] -> Maybe String -> [String]
    -> Extracted
mkevent start dur text stack inst attrs = (start, dur, text, mkstack stack,
    fmap Score.Instrument inst, Score.attributes attrs)

mkstack :: [(String, String, (ScoreTime, ScoreTime))] -> Warning.Stack
mkstack = map $ \(bid, tid, pos) ->
    (UiTest.bid bid, Just (UiTest.tid tid), Just pos)
mk_track_stack :: [(String, String)] -> Warning.Stack
mk_track_stack = map $ \(bid, tid) ->
    (UiTest.bid bid, Just (UiTest.tid tid), Nothing)

-- * sub

-- TODO use to test non-block subderives
{-
d_fake_sub :: Derive.EventDeriver
d_fake_sub = do
    st <- Derive.get
    start <- Derive.score_to_real 0
    end <- Derive.score_to_real 1
    return [Score.Event start (end-start) (Text.pack "hi") Map.empty fake_pitch
        (Derive.state_stack st)
        (Derive.state_instrument st) (Derive.state_attributes st)]
    where
    fake_pitch = PitchSignal.constant (Pitch.ScaleId "fake") (Pitch.Degree 60)

lookup_deriver deriver block_id
    | block_id == UiTest.bid "some-sub" = Right deriver
    | otherwise = Left (State.StateError "not found")
-}
