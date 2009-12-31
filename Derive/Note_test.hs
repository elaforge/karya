module Derive.Note_test where
import qualified Data.Map as Map
import qualified Data.Set as Set

import Util.Test
import qualified Util.Log as Log
import qualified Util.Seq as Seq

import qualified Midi.Midi as Midi

import Ui
import qualified Ui.State as State
import qualified Ui.UiTest as UiTest

import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Note as Note
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Warning as Warning


-- * parse

test_parse_note_dec = do
    let f ndesc text = either (const Nothing)
            (Just . Note.parse_note_desc ndesc . fst) (TrackLang.parse text)
    let mkdesc args inst attrs = Just $ Note.NoteDesc (map TrackLang.VNum args)
            (fmap Score.Instrument inst) (Set.fromList attrs)
    let ndesc = Note.NoteDesc [TrackLang.VNum 1] Nothing Score.no_attrs
    equal (f ndesc "3 4") (mkdesc [1, 3, 4] Nothing [])
    equal (f ndesc "+foo +bar -foo") (mkdesc [1] Nothing ["bar"])
    equal (f ndesc "+foo =") (mkdesc [1] Nothing [])
    equal (f ndesc ">") (mkdesc [1] Nothing [])
    equal (f ndesc ">i") (mkdesc [1] (Just "i") [])
    equal (f ndesc "foo.bar") Nothing

-- * derivers

test_d_note_track = do
    let run title evts = nostack $
            DeriveTest.derive_tracks_tempo [(title, evts)]

    equal (run ">i" []) (Right [], [])
    let inst = Just "i"
    equal (run ">i" [(0, 1, ">i +a")])
        (Right [mkevent 0 1 ">i +a" [] inst ["a"]], [])

    strings_like [show (run "> | bad/attr" [])] ["parse error"]

    -- override attrs
    let (evts, logs) = run "> +a" [(0, 1, "=b"), (1, 1, "-a")]
    equal evts $ Right
        [mkevent 0 1 "=b" [] Nothing ["b"], mkevent 1 1 "-a" [] Nothing []]
    equal logs []

test_d_sub = do
    let run evts = extract $ DeriveTest.derive_block ui_state (UiTest.bid "b1")
            where
            (_, ui_state) = UiTest.run State.empty $ do
                UiTest.mkstate "sub" [(">", [(0, 1, "--sub")])]
                UiTest.mkstate "b1" [(">", evts)]
    let (evts, logs) = run [(0, 1, "nosuch")]
    equal evts (Right [])
    strings_like (map fst logs)
        ["error sub-deriving.* unknown \\(bid \"test/nosuch\""]
    equal (map snd logs) [Just (mkstack [("b1", "b1.t0", (0, 1))])]

    -- subderived stuff is stretched and placed, inherits instrument
    let (evts, logs) = run [(0, 1, "sub"), (1, 2, ">i +a sub")]
    equal logs []
    equal evts $ Right
        [ mkevent 0 1 "--sub"
            [("sub", "sub.t0", (0, 1)), ("b1", "b1.t0", (0, 1))]
            Nothing []
        , mkevent 1 2 "--sub"
            [("sub", "sub.t0", (0, 1)), ("b1", "b1.t0", (1, 3))]
            (Just "i") ["a"]
        ]

-- TODO this should probably go in Basic_test
test_echo = do
    let derive title tracks = DeriveTest.derive_tracks_tempo
                ((title, [(0, 1, "--1"), (1, 1, "--2")]) : tracks)
    let result = derive (DeriveTest.default_inst_title ++ " | echo 2")
            [("*twelve", [(0, 0, "4c"), (1, 0, "4d")])]
    let (events, logs) = DeriveTest.e_val_right result
    let (_perf_events, convert_warns, mmsgs, perf_warns) =
            DeriveTest.perform DeriveTest.default_inst_config events

    -- The MIDI test is probably enough.
    -- let extract e = (Score.event_start e,
    --         Map.assocs (Score.event_controls e), Score.event_pitch e)
    -- pprint (map extract events)
    equal logs []
    equal convert_warns []
    equal perf_warns []
    equal (Seq.map_maybe Midi.channel_message (filter Midi.is_note_on mmsgs))
        (map (uncurry Midi.NoteOn) [(60, 100), (62, 100), (60, 40), (62, 40)])

test_calls = do
    let simple_evt val = fmap (map f) val
            where f (start, dur, text, _, _, _) = (start, dur, text)
    let run title tracks = simple_evt $ fst $ nostack $
            DeriveTest.derive_tracks_tempo
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


type Extracted =
    (TrackPos, TrackPos, String, Warning.Stack, Maybe Score.Instrument,
        Score.Attributes)

extract :: DeriveTest.Result [Score.Event]
    -> (Either String [Extracted], [(String, Maybe Warning.Stack)])
extract result = (fmap (map extract_event) err_events, map extract_log logs)
    where
    (err_events, logs) = DeriveTest.e_val result
    extract_log msg = (Log.msg_string msg, Log.msg_stack msg)
    -- | Events aren't in Eq, so extract the bits I want to test.
    extract_event e = (Score.event_start e, Score.event_duration e,
        Score.event_string e, Score.event_stack e, Score.event_instrument e,
        Score.event_attributes e)

nostack :: DeriveTest.Result [Score.Event]
    -> (Either String [Extracted], [String])
nostack = f . extract
    where
    f (result, logs) =
        (fmap (map (\(a, b, c, _, d, e) -> (a, b, c, [], d, e))) result,
            map fst logs)

mkevent :: TrackPos -> TrackPos -> String
    -> [(String, String, (TrackPos, TrackPos))] -> Maybe String -> [String]
    -> Extracted
mkevent start dur text stack inst attrs =
    (start, dur, text, mkstack stack, fmap Score.Instrument inst, mkattrs attrs)

mkattrs = Set.fromList :: [String] -> Score.Attributes

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
