module Derive.Note_test where

import Util.Test
import qualified Util.Log as Log

import Ui
-- import qualified Ui.State as State
import qualified Ui.UiTest as UiTest

import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Note as Note
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch
import qualified Perform.Warning as Warning


-- * derivers

test_d_note_track = do
    -- Test basic plumbing, but also test the null deriver while I'm at it.
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
    strings_like (map fst logs) ["CallNotFound: Symbol \"nosuch\""]
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

{-
-- TODO this should probably go in Basic_test
-- test_echo = do
    let derive title tracks = DeriveTest.derive_tracks_tempo
                ((title, [(0, 1, "--1"), (1, 1, "--2")]) : tracks)
    let result = derive (DeriveTest.default_inst_title ++ " | echo 2")
            [("*twelve", [(0, 0, "4c"), (1, 0, "4d")])]
    let (events, logs) = DeriveTest.e_val_right result
    let (_perf_events, convert_warns, mmsgs, perf_warns) =
            DeriveTest.perform DeriveTest.default_inst_config events

    -- The MIDI test is probably enough.
    equal logs []
    equal convert_warns []
    equal perf_warns []
    equal (DeriveTest.note_on_times mmsgs)
        [(0, 60, 100), (1000, 62, 100), (2000, 60, 40), (3000, 62, 40)]

-- test_tick = do
    let extract = DeriveTest.extract_events $ \e ->
            (Score.event_start e, Score.event_duration e, pitch e,
                Score.initial_velocity e)
        pitch e = let Pitch.Degree p = Score.initial_pitch e in p
    let derive evts tracks = extract $ DeriveTest.derive_tracks_tempo
            ((DeriveTest.default_inst_title++" | tick .5", evts) : tracks)
    let vel = Derive.default_velocity

    let (_evts, logs) = derive
            [(0, 1, ";tick"), (1, 1, ";tick"), (2, 1, "")]
            [("*twelve", [(0, 0, "4c"), (2, 0, "4d")])]
    equal (map Log.msg_string logs)
        ["compile (bid \"test/b1\") / note call \"tick\": no previous event"]
    equal (map Log.msg_stack logs)
        [Just (mkstack [("b1", "b1.t1", (0, 1))])]

    let (evts, logs) = derive [(0, 1, ""), (1, 1, ";tick"), (2, 1, "")]
            [("*twelve", [(0, 0, "4c"), (2, 0, "4d")])]
    equal logs []
    equal evts $ Right
        [ (0, 1, 60, vel)
        , (1.5,  0.5, 61, vel*0.5)
        , (2, 1, 62, vel)
        ]

    let (evts, logs) = derive [(0, 0.5, ""), (0.5, 0.5, ";tick"), (1, 1, "")]
            [("*twelve", [(0, 0, "4c"), (1, 0, "4d")])]
    equal logs []
    equal evts $ Right
        [ (0, 0.5, 60, vel)
        , (0.5,  0.5, 61, vel*0.5)
        , (1, 1, 62, vel)
        ]

-- test_calls = do
    let extract = DeriveTest.extract_events DeriveTest.e_event
    let run title tracks = fst $ extract $ DeriveTest.derive_tracks_tempo
            ((title, [(0, 1, "--1"), (1, 1, "--2")]) : tracks)

    left_like (run ">i | call | 42 bad parse" [])
        "non-function in function position"
    left_like (run ">i | no-such-call" [])
        "unknown CallId \"no-such-call\""
    left_like (run ">i | delay *bad-arg" [])
        "expected signal but got"
    left_like (run ">i | delay 1 2 3 4" [])
        "too many arguments"
    left_like (run ">i | delay" [])
        "not in environment"
    left_like (run ">i | delay _" [])
        "not in environment"
    left_like (run ">i | delay %delay" []) $
        "not in environment"

    equal (run ">i | delay 2" []) $
        Right [(2, 1, "--1"), (3, 1, "--2")]
    equal (run ">i | delay %delay,2" []) $
        Right [(2, 1, "--1"), (3, 1, "--2")]
    equal (run ">i | delay %delay" [("delay", [(0, 0, "1"), (1, 0, "2")])]) $
        Right [(1, 1, "--1"), (3, 1, "--2")]
    equal (run ">i | delay %delay,1 | delay %delay,1" []) $
        Right [(2, 1, "--1"), (3, 1, "--2")]
-}

test_negative_duration = do
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


type Extracted =
    (TrackPos, TrackPos, String, Warning.Stack, Maybe Score.Instrument,
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

mkevent :: TrackPos -> TrackPos -> String
    -> [(String, String, (TrackPos, TrackPos))] -> Maybe String -> [String]
    -> Extracted
mkevent start dur text stack inst attrs = (start, dur, text, mkstack stack,
    fmap Score.Instrument inst, Score.attributes attrs)

mkstack :: [(String, String, (TrackPos, TrackPos))] -> Warning.Stack
mkstack = map $ \(bid, tid, pos) ->
    (UiTest.bid bid, Just (UiTest.tid tid), Just pos)

-- * sub

-- TODO use to test non-block subderives
{-
d_fake_sub :: Derive.EventDeriver
d_fake_sub = do
    st <- Derive.get
    start <- Derive.local_to_global 0
    end <- Derive.local_to_global 1
    return [Score.Event start (end-start) (Text.pack "hi") Map.empty fake_pitch
        (Derive.state_stack st)
        (Derive.state_instrument st) (Derive.state_attributes st)]
    where
    fake_pitch = PitchSignal.constant (Pitch.ScaleId "fake") (Pitch.Degree 60)

lookup_deriver deriver block_id
    | block_id == UiTest.bid "some-sub" = Right deriver
    | otherwise = Left (State.StateError "not found")
-}
