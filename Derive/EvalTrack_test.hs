-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.EvalTrack_test where
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Util.Log as Log
import qualified Util.Seq as Seq
import Util.Test

import qualified Ui.Event as Event
import qualified Ui.UiTest as UiTest
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.Call as Call
import qualified Derive.Call.CallTest as CallTest
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Prelude.Note as Note
import qualified Derive.Call.Sub as Sub
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.EvalTrack as EvalTrack
import qualified Derive.Instrument.DUtil as DUtil
import qualified Derive.PSignal as PSignal
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import qualified Derive.Stack as Stack
import qualified Derive.Stream as Stream
import qualified Derive.TrackWarp as TrackWarp

import qualified Perform.Midi.Patch as Patch
import qualified Perform.Signal as Signal
import Global
import Types


module_ :: Module.Module
module_ = "test-module"

test_event_serial = do
    -- Verify that 'Derive.state_event_serial' is reset for each event.
    --
    -- Ultimately, I want different randomization for each note in a single
    -- event, but randomization is not affected if a previous event changes.
    let run = DeriveTest.extract (fromMaybe (error "no control")
                . DeriveTest.e_start_control "serial")
            . DeriveTest.derive_tracks_setup with_calls ""
            . UiTest.note_track
        with_calls = CallTest.with_note_generator "" note
            <> CallTest.with_note_generator "notes" notes
    equal (run [(0, 1, "notes 1 -- 4c"), (1, 1, "notes 1 -- 4d")])
        ([0, 0], [])
    equal (run [(0, 1, "notes 2 -- 4c"), (1, 1, "notes 1 -- 4d")])
        ([0, 1, 0], [])
    equal (run [(0, 1, "notes 1 -- 4c"), (1, 1, "notes 2 -- 4d")])
        ([0, 0, 1], [])
    where
    note = Note.transformed_note "" mempty $ \_args deriver -> do
        serial <- Derive.gets $
            Derive.state_event_serial . Derive.state_threaded
        Derive.with_constant_control "serial"
            (Score.untyped (fromIntegral serial)) deriver
    notes = Derive.generator CallTest.module_ "test" mempty "test doc" $
        Sig.call (Sig.required "notes" "Number of notes.") $ \notes _args ->
            mconcat $ replicate notes Call.note

test_threaded_last_val = do
    let run notes = DeriveTest.extract (DeriveTest.e_control "c") $
            DeriveTest.derive_tracks_setup with_calls ""
                [ (">", [(t, 1, "") | (t, _) <- events])
                , ("c", [(t, 0, s) | (t, s) <- events])
                ]
                where events = zip (Seq.range_ 0 1) notes
        with_calls = CallTest.with_control_generator "prev" c_prev
    equal (run ["1", "prev"])
        ([[(0, 1), (0.75, 0.75)], [(1, 1), (1.25, 1.25)]], [])
    equal (run ["1", "prev", "prev"])
        ( [ [(0, 1), (0.75, 0.75)]
          , [(1, 1), (1.25, 1.25), (1.75, 1)]
          , [(2, 1.25), (2.25, 1.5)]
          ], [])
    where
    c_prev :: Derive.Generator Derive.Control
    c_prev = CallTest.generator1 $ \args -> case Args.prev_control args of
        Nothing -> Derive.throw "no prev val"
        Just (x, y) -> do
            start <- Args.real_start args
            -- Log.warn $ show (start, (x, y))
            return $ Signal.signal $ if start > x
                then [(start - 0.25, y - 0.25), (start, y),
                    (start + 0.25, y + 0.25)]
                else []

test_threaded_last_event = do
    let run = snd . DeriveTest.extract id . DeriveTest.derive_tracks_setup
            (with_calls <> DeriveTest.with_linear) ""
        with_calls = CallTest.with_note_generator "prev" n_prev
    equal (run [(">", [(0, 1, ""), (1, 1, "prev")])]) ["0.0"]
    -- Get last event when both are inverted.
    equal (run [(">", [(0, 1, ""), (1, 1, "prev")]),
            ("*", [(0, 0, "3c"), (1, 0, "3d")])])
        ["0.0"]
    -- Get last event when one is an orphan.
    equal (run
            [ (">", [(0, 1, "+a")])
            , (">", [(0, 1, ""), (1, 1, "prev")])
            , ("*", [(0, 0, "4c"), (1, 0, "4e")])
            ])
        ["0.0"]
    where
    n_prev :: Derive.Generator Derive.Note
    n_prev = CallTest.generator $ \args -> case Args.prev_val args of
        Nothing -> Derive.throw "no prev val"
        Just prev -> do
            Log.warn $ showt (Score.event_start prev)
            return Stream.empty

test_assign_controls = do
    let run inst_title cont_title val = extract $ DeriveTest.derive_tracks ""
            [ (cont_title, [(0, 0, val)])
            , ("*", [(0, 0, "4c")])
            , (inst_title, [(0, 1, "")])
            ]
        extract = DeriveTest.extract $ \e ->
            (DeriveTest.e_pitch e, DeriveTest.e_control "cont" e)

    -- normal
    equal (run ">i" "cont" "1") ([("4c", [(0, 1)])], [])
    -- not seen
    equal (run ">i" "gont" "1") ([("4c", [])], [])

    -- a non-existent control with no default is an error
    let (events, logs) = run ">i | %cont = %bonk" "gont" "1"
    equal events []
    strings_like logs ["not found: Control \"bonk\""]
    -- control assigned
    equal (run ">i | %cont = %gont" "gont" "1") ([("4c", [(0, 1)])], [])

    -- set a constant signal
    equal (run ">i | %cont = 42" "gont" "1") ([("4c", [(0, 42)])], [])
    -- set constant signal with a default
    equal (run ">i | %cont = %gont,42" "bonk" "1") ([("4c", [(0, 42)])], [])
    equal (run ">i | %cont = %gont,42" "gont" "1") ([("4c", [(0, 1)])], [])

    -- named pitch doesn't show up
    equal (run ">i" "*twelve #foo" "2c") ([("4c", [])], [])
    -- assigned to default pitch, so it shows up
    equal (run ">i | # = #foo" "*twelve #foo" "2c") ([("2c", [])], [])
    -- set constant pitch
    equal (run ">i | # = (1c)" "*twelve #foo" "2c") ([("1c", [])], [])

test_environ_across_tracks = do
    let run tracks = DeriveTest.extract (DeriveTest.e_control "cont") $
            DeriveTest.derive_tracks "" ((">", [(0, 10, "")]) : tracks)

    -- first make sure srate works as I expect
    let interpolated = [(0, 0), (1, 0.25), (2, 0.5), (3, 0.75), (4, 1)]
    equal (run [("cont", [(0, 0, "0"), (4, 0, "i 1")])])
        ([interpolated], [])
    equal (run [("set cont | srate = 2", [(1, 0, "0"), (5, 0, "i 1")])])
        ([[(1, 0), (3, 0.5), (5, 1)]], [])

    -- now make sure srate in one track doesn't affect another
    let cont = ("cont", [(0, 0, "0"), (4, 0, "i 1")])
    equal (run [("cont2 | srate = 2", []), cont])
        ([interpolated], [])
    equal (run [cont, ("cont2 | srate = 2", [])])
        ([interpolated], [])

test_call_errors = do
    let derive = extract . DeriveTest.derive_tracks_setup with_trans ""
        with_trans = CallTest.with_note_transformer "test-t" trans
        extract r = case DeriveTest.extract DeriveTest.e_event r of
            (val, []) -> Right val
            (_, logs) -> Left $ Seq.join "\n" logs

    let run_title title = derive [(title, [(0, 1, "--1")])]
    left_like (run_title ">i | no-such-call")
        "note transformer not found: no-such-call"
    left_like (run_title ">i | test-t *bad-arg") "expected ControlRef but got"
    left_like (run_title ">i | test-t 1 2 3 4") "too many arguments"
    left_like (run_title ">i | test-t") "not found and no default"
    left_like (run_title ">i | test-t _") "not found and no default"
    left_like (run_title ">i | test-t %delay") "not found and no default"

    let run_evt evt = derive [(">i", [(0, 1, evt)])]
    left_like (run_evt "no-such-call") "note generator not found: no-such-call"
    let tr_result = extract $ DeriveTest.derive_tracks ""
            [(">", [(0, 4, "")]), ("*twelve", [(0, 0, "tr")])]
    left_like tr_result "ArgError: expected another argument"
    equal (run_evt "test-t 2 | test-t 1 |")
        (Right [(0, 1, "test-t 2 | test-t 1 |")])
    where
    trans = Derive.transformer module_ "trans" mempty "doc" $ Sig.callt
        (Sig.defaulted "arg1" (Sig.required_control "test") "doc") $
        \c _args deriver -> do
            Call.control_at c 0
            deriver

test_val_call = do
    let extract = DeriveTest.extract (DeriveTest.e_control "cont")
    let run evt = extract $ DeriveTest.derive_tracks_setup with_add1 ""
            [(">", [(0, 1, "")]), ("cont", [(0, 0, evt)])]
        with_add1 = CallTest.with_val_call "add1" add_one
    strings_like (snd (run "foobar")) ["control generator not found: foobar"]
    equal (run "set 1") ([[(0, 1)]], [])
    equal (run "set (add1 1)") ([[(0, 2)]], [])
    equal (run "set (add1 (add1 1))") ([[(0, 3)]], [])
    strings_like (snd (run "set (add1 1 2)")) ["too many arguments"]
    where
    add_one :: Derive.ValCall
    add_one = Derive.val_call module_ "add" mempty "doc" $ Sig.call
        (Sig.required "v" "doc") $
        \val _ -> return (val + 1 :: Double)

test_inst_call = do
    let run inst = DeriveTest.extract DeriveTest.e_attributes $
            DeriveTest.derive_tracks_setup with_inst ""
            [(inst, [(0, 1, "sn")])]
        with_inst = DeriveTest.with_synths
            (UiTest.allocations [("i1", "s/1"), ("with-call", "s/with-call")])
            [synth]
    equal (run ">i1") ([], ["Error: note generator not found: sn"])
    equal (run ">with-call") (["+snare"], [])
    where
    synth = DeriveTest.make_synth "s" patches
    code = MidiInst.note_generators [("sn", DUtil.attributes_note Attrs.snare)]
    patches =
        [ MidiInst.code #= code $ MidiInst.make_patch $
            Patch.attribute_map #= Patch.unpitched_keymap [(Attrs.snare, 42)] $
            DeriveTest.make_patch "with-call"
        , MidiInst.make_patch $ DeriveTest.make_patch "1"
        ]

test_events_around = do
    -- Ensure sliced inverting notes still have access to prev and next events
    -- via the track_around hackery.
    let logs = extract $ DeriveTest.derive_tracks_setup with_call ""
            [ (">", [(0, 1, ""), (1, 1, "around"), (2, 1, "")])
            , ("*twelve", [(0, 0, "4c"), (2, 0, "4d")])
            ]
        with_call = CallTest.with_note_generator "around" c_around
        extract = DeriveTest.r_log_strings
    equal logs ["prev: [0.0]", "next: [2.0]"]
    where
    c_around = CallTest.generator $ Sub.inverting $ \args -> do
        Log.warn $ "prev: " <> showt (map Event.start (Args.prev_events args))
        Log.warn $ "next: " <> showt (map Event.start (Args.next_events args))
        return Stream.empty

test_track_dynamic = do
    let e_scale_inst dyn =
            (env_lookup EnvKey.scale env, env_lookup EnvKey.instrument env)
            where env = Derive.state_environ dyn
        block_id = UiTest.default_block_id
    let res = DeriveTest.derive_blocks
            [ ("b", [("*legong", [(0, 0, "1")]), (">i1", [(0, 1, "sub")])])
            , ("sub", [(">", [(0, 1, "")]), ("*", [(0, 0, "2")])])
            ]
    equal (e_track_dynamic e_scale_inst res)
        [ ((UiTest.bid "b", 1), ("legong", "Nothing"))
        , ((UiTest.bid "b", 2), ("legong", ">i1"))
        , ((UiTest.bid "sub", 1), ("legong", ">i1"))
        , ((UiTest.bid "sub", 2), ("legong", ">i1"))
        ]

    -- I get TrackDynamics from the *legong track even when there are 0 events
    -- on it.
    let res = DeriveTest.derive_tracks ""
            [(">i1", [(0, 1, "")]), ("*legong", [])]
    equal (e_track_dynamic e_scale_inst res)
        [ ((block_id, 1), ("legong", ">i1"))
        , ((block_id, 2), ("legong", ">i1"))
        ]

    -- Controls for the note track come from the uninverted version, env from
    -- inverted version.  Otherwise, Dynamic is from the first slice.
    let res = DeriveTest.derive_tracks_linear ""
            [ ("dyn", [(0, 0, ".5")])
            , (">", [(0, 1, ""), (1, 1, ""), (2, 1, "")])
            , ("*legong", [])
            , ("dyn", [(0, 0, ".25"), (1, 0, ".5"), (2, 0, ".75")])
            , ("dyn", [(0, 0, ".25"), (1, 0, ".5"), (2, 0, ".75")])
            ]
    let e_dyn dyn = Signal.unsignal . Score.typed_val <$>
            Map.lookup Controls.dynamic (Derive.state_controls dyn)
        e_scale = env_lookup EnvKey.scale . Derive.state_environ
        all_tracks = [(block_id, n) | n <- [1..5]]
    equal (e_track_dynamic e_scale res)
        (zip all_tracks ("twelve" : repeat "legong"))
    equal (e_track_dynamic e_dyn res)
        (zip all_tracks
            [ Just [(0, 1)]
            , Just [(0, 0.5)]
            , Just [(0, 0.5)]
            , Just [(0, 0.5)]
            , Just [(0, 0.125)]
            ])

test_track_dynamic_consistent = do
    -- Ensure that all parts of the Dynamic come from the same derivation of
    -- the track.
    let run extract = lookup (UiTest.bid "sub", 1) $
            e_track_dynamic extract $ DeriveTest.derive_blocks
            [ ("top",
                [ ("> | env = a", [(0, 1, "sub")])
                , ("> | env = b | %c = 1", [(0, 1, "sub")])
                ])
            , ("sub=ruler", [(">", [(0, 1, "")])])
            ]
        e_env = env_lookup "env" . Derive.state_environ
        e_control = fmap (Signal.unsignal . Score.typed_val) . Map.lookup "c"
            . Derive.state_controls
    -- %c is only set in the env=b branch, so it shouldn't be set when env=a.
    equal (run e_env) (Just "a")
    equal (run e_control) (Just Nothing)

test_prev_val = do
    -- Test the prev_val and saved_val stuff in 'EvalTrack.derive_track'.
    let run = DeriveTest.extract Score.initial_dynamic
            . DeriveTest.derive_tracks ""
    -- 0        1       2       3       4       5
    -- ""       ""      ""      ""
    -- .5       i> 0                            1
    equal (run
            [ (">", [(0, 1, ""), (1, 1, ""), (2, 1, ""), (3, 1, "")])
            , ("dyn", [(0, 0, ".5"), (1, 0, "i> 0"), (5, 0, "1")])
            ])
        ([0.5, 0.5, 0.375, 0.25], [])

    equal (run
            [ (">", [(0, 1, ""), (1, 1, ""), (2, 1, ""), (3, 1, "")])
            , ("dyn", [(0, 0, "0"), (4, 0, "i 1")])
            ])
        ([0, 0.25, 0.5, 0.75], [])

env_lookup :: Env.Key -> Env.Environ -> String
env_lookup key = prettys . Env.lookup key

e_track_dynamic :: (Derive.Dynamic -> a) -> Derive.Result
    -> [((BlockId, TrackNum), a)]
e_track_dynamic extract = map (first (second UiTest.tid_tracknum))
    . Map.toList . fmap extract . Derive.r_track_dynamic

test_track_dynamic_invert = do
    -- Ensure the correct TrackDynamic is collected even in the presence of
    -- inversion.
    let run = e_track_dynamic (e_env . Derive.state_environ)
            . DeriveTest.derive_tracks ""
        e_env e = (lookup EnvKey.instrument e, lookup EnvKey.scale e)
        lookup val = prettys . Env.lookup val
    -- Both tracks get *legong, even though >inst has to be inverted to see it.
    equal (run [(">i", [(0, 0, "")]), ("*legong", [(0, 0, "1")])])
        [ ((UiTest.default_block_id, 1), (">i", "legong"))
        , ((UiTest.default_block_id, 2), (">i", "legong"))
        ]

test_note_end = do
    let run = DeriveTest.derive_tracks_setup with_call ""
        with_call = CallTest.with_control_generator "g" gen
    let result = run [(">", [(0, 4, "")]), ("dyn", [(0, 0, "g")])]
    equal (DeriveTest.extract DeriveTest.e_dyn result)
        ([[(0, 0), (4, 1)]], [])
    where
    gen :: Derive.Generator Derive.Control
    gen = CallTest.generator1 $ \args -> do
        (start, end) <- Args.range_or_note_end args
        start <- Derive.real start
        end <- Derive.real end
        return $ Signal.signal [(start, 0), (end, 1)]

-- * test orphans

test_orphans = do
    let extract = DeriveTest.extract_events Score.event_start
    let run = extract . DeriveTest.derive_tracks_setup
            (with_calls <> DeriveTest.with_linear) ""
        with_calls = CallTest.with_note_generator "show" show_subs
    -- uncovered events are still played
    equal (run
            [ (">i1", [(1, 1, "show")])
            , (">i2", [(0, 1, ""), (1, 1, ""), (2, 1, "")])
            ])
        [0, 2]
    -- as above, but with tuplet, verify it gets the correct subs
    equal (run
            [ (">", [(1, 4, "t")])
            , (">", [(0, 1, ""), (1, 1, ""), (2, 1, ""), (5, 1, "")])
            ])
        [0, 1, 3, 5]

    -- 0 dur captures the matching event below
    equal (run
            [ (">", [(1, 0, "show")])
            , (">", [(0, 1, ""), (1, 1, ""), (2, 1, "")])
            ])
        [0, 2]

    -- empty track above is ignored completely
    equal (run
            [ (">", [])
            , (">", [(0, 1, ""), (1, 1, ""), (2, 1, "")])
            ])
        [0, 1, 2]
    where
    show_subs :: Derive.Generator Derive.Note
    show_subs = CallTest.generator $ \args -> do
        let subs = Derive.ctx_sub_tracks (Derive.passed_ctx args)
        Log.warn $ pretty subs
        return Stream.empty

test_record_empty_tracks = do
    -- Ensure that TrackWarps and TrackDynamics are collected for empty tracks.
    let run = DeriveTest.derive_tracks_linear ""
        track_warps = concatMap (Set.toList . TrackWarp.tw_tracks)
            . Derive.r_track_warps
        track_dyn = Map.keys . Derive.r_track_dynamic

    let result = run [(">i1", []), (">i2", []), (">i3", [(0, 1, "")])]
    equal (track_warps result) (map UiTest.mk_tid [1, 2, 3])
    equal (track_dyn result)
        (map (((,) UiTest.default_block_id) . UiTest.mk_tid) [1, 2, 3])

test_empty_parent_track = do
    -- Ensure orphan tracks pick the instrument up from the parent.
    -- Well, the absentee parent, since they're orphans.
    let run = DeriveTest.extract extract . DeriveTest.derive_tracks_linear ""
        extract e = (Score.event_start e, DeriveTest.e_instrument e)
    equal (run [(">i1", [(0, 1, "t")]), (">", [(0, 1, "")])]) ([(0, "i1")], [])
    equal (run [(">i1", []), (">", [(0, 1, "")])]) ([(0, "i1")], [])

test_two_level_orphans = do
    -- Orphan extraction should be recursive, in case there are multiple
    -- intervening empty tracks.
    let run = DeriveTest.extract extract . DeriveTest.derive_tracks_linear ""
        extract e = (DeriveTest.e_note e, DeriveTest.e_attributes e)
    equal (run
        [ (">i", [(0, 1, "+a")])
        , (">", [(1, 1, "+b")])
        , (">", [(2, 1, "+c")])
        , (">", [(0, 1, ""), (1, 1, ""), (2, 1, "")])
        , ("*", [(0, 0, "4c"), (1, 0, "4d"), (2, 0, "4e")])
        ])
        ([((0, 1, "4c"), "+a"), ((1, 1, "4d"), "+b"), ((2, 1, "4e"), "+c")],
            [])

test_orphan_ranges = do
    -- These test TrackTree.track_end, indirectly by making sure it clips
    -- or doesn't clip signal correctly.
    let run = DeriveTest.extract DeriveTest.e_nns
            . DeriveTest.derive_tracks_linear ""
    -- Each note has only its control.
    equal (run
        [ (">", [(1, 3, "(")])
        , (">", [])
        , (">", [(1, 1, "n --a"), (2, 1, "n --b")])
        , ("*", [(1, 0, "4c"), (2, 0, "4d")])
        ])
        ([[(1, 60)], [(2, 62)]], [])

    -- The note has the pitch after its end.
    equal (run
        [ ("*", [(0, 0, "4c"), (10, 0, "4d")])
        , (">", [])
        , (">", [(0, 1, "")])
        ])
        ([[(0, 60), (10, 62)]], [])

-- * neighbors

test_derive_neighbor_pitches = do
    let run notes = DeriveTest.extract DeriveTest.e_note $
            DeriveTest.derive_tracks_setup
                (CallTest.with_note_generator "g" gen) "" $
            UiTest.note_track notes
            ++ [("dyn", [(0, 0, ".5")])]
    strings_like (snd $ run [(0, 1, "4c"), (1, 1, "g -- 4d"), (2, 1, "4e")])
        [ "Just \"4c\"*Just \"4e\""
        -- It stops evaluating at the first pitch track, so it doesn't see the
        -- dyn track.
        , "Just \"1\"*Just \"1\""
        ]
    where
    gen :: Derive.Generator Derive.Note
    gen = CallTest.generator $ \args -> do
        state <- Derive.get
        let neighbors = EvalTrack.derive_neighbor_pitches state
                (Args.context args)
            extract f = (map (first (fmap f)) *** map (first (fmap f)))
        Log.warn $ showt $ extract (pretty . Score.initial_note) neighbors
        Log.warn $ showt $ extract (pretty . Score.initial_dynamic) neighbors
        return Stream.empty

test_neighbor_pitches_note_track = do
    let run prev = DeriveTest.extract DeriveTest.e_start_note $
            DeriveTest.derive_tracks_setup
                (CallTest.with_note_generator "g" (note_gen prev)) "" $
            UiTest.note_track [(0, 1, "4c"), (1, 1, "g -- 4d"), (2, 1, "4e")]
    equal (run True) ([(0, "4c"), (1, "4c"), (2, "4e")], [])
    equal (run False) ([(0, "4c"), (1, "4e"), (2, "4e")], [])
    where
    note_gen :: Bool -> Derive.Generator Derive.Note
    note_gen prev = CallTest.generator $ \args -> do
        pitch <- Derive.require "no prev/next" =<< if prev
            then Args.lookup_prev_logical_pitch
            else Args.lookup_next_logical_pitch
        Call.place args $ Call.pitched_note pitch

test_neighbor_pitches_pitch_track = do
    let run prev = DeriveTest.extract DeriveTest.e_start_note $
            DeriveTest.derive_tracks_setup
                (CallTest.with_pitch_generator "g" (pitch_gen prev)) "" $
            UiTest.note_track [(0, 1, "4c"), (1, 1, "g"), (2, 1, "4e")]
    -- g is called 3 times
    equal (run True) ([(0, "4c"), (1, "4c"), (2, "4e")], [])
    equal (run False) ([(0, "4c"), (1, "4e"), (2, "4e")], [])
    where
    pitch_gen :: Bool -> Derive.Generator Derive.Pitch
    pitch_gen prev = CallTest.generator1 $ \args -> do
        pitch <- if prev
            then Args.lookup_prev_logical_pitch
            else Args.lookup_next_logical_pitch
        start <- Args.real_start args
        return $ PSignal.signal $ maybe [] (\p -> [(start, p)]) pitch

-- * misc

test_bi_zipper = do
    let f = EvalTrack.bi_zipper
    let (prevs, nexts) = f [2, 1, 0] 3 [4, 5, 6]
    equal prevs
        [ ([1, 0], 2, [3, 4, 5, 6])
        , ([0], 1, [2, 3, 4, 5, 6])
        , ([], 0, [1, 2, 3, 4, 5, 6])
        ]
    equal nexts
        [ ([3, 2, 1, 0], 4, [5, 6])
        , ([4, 3, 2, 1, 0], 5, [6])
        , ([5, 4, 3, 2, 1, 0], 6, [])
        ]

test_parse_error = do
    let run = first (map Score.event_start) . DeriveTest.extract_logs
            . Stream.to_list . Derive.r_events . DeriveTest.derive_tracks ""
    let (events, logs) = run [(">", [(1, 2, "- 4:12")])]
    equal events []
    equal (map (fmap Stack.to_ui . Log.msg_stack) logs)
        [Just [(Just (UiTest.bid "b1"), Just (UiTest.tid "b1.t1"),
            Just (1, 3))]]

test_exception_reverts_state = do
    let run = DeriveTest.extract (DeriveTest.e_environ "a")
            . DeriveTest.derive_tracks ""
    equal (run [(">", [(0, 1, "a=b | err"), (1, 1, "")])])
        ([Nothing], ["Error: note generator not found: err"])
