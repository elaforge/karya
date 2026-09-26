-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.EvalTrack_test where
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Util.Lists as Lists
import qualified Util.Log as Log

import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.C.Prelude.Note as Note
import qualified Derive.Call as Call
import qualified Derive.Call.CallTest as CallTest
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Sub as Sub
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Instrument.DUtil as DUtil
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.Sig as Sig
import qualified Derive.Stack as Stack
import qualified Derive.Stream as Stream
import qualified Derive.Symbols as Symbols
import qualified Derive.TrackWarp as TrackWarp

import qualified Perform.Midi.Patch as Patch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import qualified Ui.Event as Event
import qualified Ui.UiTest as UiTest

import           Global
import           Types
import           Util.Test


module_ :: Module.Module
module_ = "test-module"

test_event_serial :: Test
test_event_serial = do
    -- Verify that 'Derive.state_event_serial' is reset for each event.
    --
    -- Ultimately, I want different randomization for each note in a single
    -- event, but randomization is not affected if a previous event changes.
    let run = DeriveTest.extract extract
            . DeriveTest.deriveTracksSetup withCalls ""
            . UiTest.noteTrack
        extract = fromMaybe (error "no control")
            . DeriveTest.eInitialControl "serial"
        withCalls = CallTest.withNoteGenerator Symbols.nullNote note
            <> CallTest.withNoteGenerator "notes" notes
    equal (run [(0, 1, "notes 1 -- 4c"), (1, 1, "notes 1 -- 4d")])
        ([0, 0], [])
    equal (run [(0, 1, "notes 2 -- 4c"), (1, 1, "notes 1 -- 4d")])
        ([0, 1, 0], [])
    equal (run [(0, 1, "notes 1 -- 4c"), (1, 1, "notes 2 -- 4d")])
        ([0, 0, 1], [])
    where
    note = Note.transformedNote "" mempty $ \_args deriver -> do
        serial <- Derive.gets $
            Derive.stateEventSerial . Derive.stateThreaded
        Derive.withConstantControl "serial" (fromIntegral serial) deriver
    notes = Derive.generator CallTest.module_ "test" mempty "test doc" $
        Sig.call (Sig.required "notes" "Number of notes.") $ \notes _args ->
            mconcat $ replicate notes Call.note

test_threaded_last_val :: Test
test_threaded_last_val = do
    let run notes = DeriveTest.extract (DeriveTest.eControl "c") $
            DeriveTest.deriveTracksSetup withCalls ""
                [ (">", [(t, 1, "") | (t, _) <- events])
                , ("c", [(t, 0, s) | (t, s) <- events])
                ]
                where events = zip (Lists.range_ 0 1) notes
        withCalls = CallTest.withControlGenerator "prev" cPrev
    equal (run ["1", "prev"])
        ( [ [(0, 1), (0.75, 1), (0.75, 0.75), (1, 1)]
          , [(1, 1), (1.25, 1.25)]
          ]
        , []
        )
    equal (run ["1", "prev", "prev"])
        ( [ [(0, 1), (0.75, 1), (0.75, 0.75), (1, 1)]
          , [(1, 1), (1.25, 1.25), (1.75, 1.25), (1.75, 1), (2, 1.25)]
          , [(2, 1.25), (2.25, 1.5)]
          ], [])
    where
    cPrev :: Derive.Generator Derive.Control
    cPrev = CallTest.generator1 $ \args -> case Args.prevControl args of
        Nothing -> Derive.throw "no prev val"
        Just (x, y) -> do
            start <- Args.realStart args
            -- Log.warn $ show (start, (x, y))
            pure $ Signal.fromPairs $ if start > x
                then [(start - 0.25, y - 0.25), (start, y),
                    (start + 0.25, y + 0.25)]
                else []

test_threaded_last_event :: Test
test_threaded_last_event = do
    let run = snd . DeriveTest.extract id . DeriveTest.deriveTracksSetup
            (withCalls <> DeriveTest.withLinear) ""
        withCalls = CallTest.withNoteGenerator "prev" nPrev
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
    nPrev :: Derive.Generator Derive.Note
    nPrev = CallTest.generator $ \args -> case Args.prevVal args of
        Nothing -> Derive.throw "no prev val"
        Just prev -> do
            Log.warn $ showt (Score.eventStart prev)
            pure Stream.empty

test_assign_controls :: Test
test_assign_controls = do
    let run instTitle contTitle val = extract $ DeriveTest.deriveTracks ""
            [ (contTitle, [(0, 0, val)])
            , ("*", [(0, 0, "4c")])
            , (instTitle, [(0, 1, "")])
            ]
        extract = DeriveTest.extract $ \e ->
            (DeriveTest.ePitch e, DeriveTest.eControl "cont" e)
    -- normal
    equal (run ">i" "cont" "1") ([("4c", [(0, 1)])], [])
    -- not seen
    equal (run ">i" "gont" "1") ([("4c", [])], [])

    -- a non-existent control with no default is an error
    let (events, logs) = run ">i | cont = %bonk" "gont" "1"
    equal events []
    stringsLike logs ["control not found: %bonk"]

    -- TODO I could support this with Derive.get_control_map in
    -- NoteUtil.make_event, but I don't think I use this feature.
    -- -- control assigned
    -- equal (run ">i | cont = %gont" "gont" "1") ([("4c", [(0, 1)])], [])
    -- -- set constant signal with a default
    -- equal (run ">i | cont = %gont,42" "bonk" "1")
    --     ([("4c", [(-RealTime.larger, 42)])], [])
    -- equal (run ">i | cont = %gont,42" "gont" "1") ([("4c", [(0, 1)])], [])

    -- set a constant signal
    equal (run ">i | cont = 42" "gont" "1")
        ([("4c", [(-RealTime.larger, 42)])], [])

    -- named pitch doesn't show up
    equal (run ">i" "*twelve #foo" "2c") ([("4c", [])], [])
    -- assigned to default pitch, so it shows up
    equal (run ">i | # = #foo" "*twelve #foo" "2c") ([("2c", [])], [])
    -- set constant pitch
    equal (run ">i | # = (1c)" "*twelve #foo" "2c") ([("1c", [])], [])

test_environ_across_tracks :: Test
test_environ_across_tracks = do
    let run tracks = DeriveTest.extract (DeriveTest.eControl "cont") $
            DeriveTest.deriveTracks "" ((">", [(0, 10, "")]) : tracks)

    -- first make sure srate works as I expect
    let interpolated = [(0, 0), (1, 0.0625), (2, 0.25), (3, 0.5625), (4, 1)]
    equal (run [("cont", [(0, 0, "0"), (4, 0, "e 1")])])
        ([interpolated], [])
    equal (run [("cont set | srate = .5", [(1, 0, "0"), (5, 0, "e 1")])])
        ([[(1, 0), (3, 0.25), (5, 1)]], [])

    -- now make sure srate in one track doesn't affect another
    let cont = ("cont", [(0, 0, "0"), (4, 0, "e 1")])
    equal (run [("cont2 | srate = .5", []), cont]) ([interpolated], [])
    equal (run [cont, ("cont2 | srate = .5", [])]) ([interpolated], [])

test_call_errors :: Test
test_call_errors = do
    let derive = extract . DeriveTest.deriveTracksSetup withTrans ""
        withTrans = CallTest.withNoteTransformer "test-t" trans
        extract r = case DeriveTest.extract DeriveTest.eEvent r of
            (val, []) -> Right val
            (_, logs) -> Left $ Lists.join "\n" logs

    let runTitle title = derive [(title, [(0, 1, "--1")])]
    leftLike (runTitle ">i | no-such-call")
        "note transformer not found: no-such-call"
    leftLike (runTitle ">i | test-t bad-arg") "expected Signal but got Str"
    leftLike (runTitle ">i | test-t 1 2 3 4") "too many arguments"
    leftLike (runTitle ">i | test-t") "expected an argument at \"test\""
    leftLike (runTitle ">i | test-t _") "expected Signal but got _"
    leftLike (runTitle ">i | test-t %delay")
        "couldn't convert ControlRef %delay: control not found"

    let runEvt evt = derive [(">i", [(0, 1, evt)])]
    leftLike (runEvt "no-such-call") "note generator not found: no-such-call"
    let trResult = extract $ DeriveTest.deriveTracks ""
            [(">", [(0, 4, "")]), ("*twelve", [(0, 0, "tr")])]
    leftLike trResult "expected an argument"
    equal (runEvt "test-t 2 | test-t 1 |")
        (Right [(0, 1, "test-t 2 | test-t 1 |")])
    where
    trans = CallTest.transformerArgs $ Sig.callt
        (Sig.required "test" "doc") $ \c  _args deriver ->
            Derive.withConstantControl "c" c deriver

test_val_call :: Test
test_val_call = do
    let extract = DeriveTest.extract (DeriveTest.eControl "cont")
    let run evt = extract $ DeriveTest.deriveTracksSetup withAdd1 ""
            [(">", [(0, 1, "")]), ("cont", [(0, 0, evt)])]
        withAdd1 = CallTest.withValCall "add1" addOne
    stringsLike (snd (run "foobar")) ["control generator not found: foobar"]
    equal (run "set 1") ([[(0, 1)]], [])
    equal (run "set (add1 1)") ([[(0, 2)]], [])
    equal (run "set (add1 (add1 1))") ([[(0, 3)]], [])
    stringsLike (snd (run "set (add1 1 2)")) ["too many arguments"]
    where
    addOne :: Derive.ValCall
    addOne = Derive.valCall module_ "add" mempty "doc" $ Sig.call
        (Sig.required "v" "doc") $
        \val _ -> pure (val + 1 :: Double)

test_inst_call :: Test
test_inst_call = do
    let run inst = DeriveTest.extract DeriveTest.eAttributes $
            DeriveTest.deriveTracksSetup withInst ""
            [(inst, [(0, 1, "sn")])]
        withInst = DeriveTest.withSynthsSimple
            [("i1", "s/1"), ("with-call", "s/with-call")]
            [synth]
    equal (run ">i1") ([], ["note generator not found: sn"])
    equal (run ">with-call") (["+snare"], [])
    where
    synth = UiTest.makeSynth "s" patches
    code = MidiInst.noteGenerators [("sn", DUtil.attributesNote Attrs.snare)]
    patches =
        [ MidiInst.code #= code $ MidiInst.makePatch $
            Patch.attributeMap #= Patch.unpitchedKeymap [(Attrs.snare, 42)] $
            UiTest.makePatch "with-call"
        , MidiInst.makePatch $ UiTest.makePatch "1"
        ]

test_events_around :: Test
test_events_around = do
    -- Ensure sliced inverting notes still have access to prev and next events
    -- via the track_around hackery.
    let logs = extract $ DeriveTest.deriveTracksSetup withCall ""
            [ (">", [(0, 1, ""), (1, 1, "around"), (2, 1, "")])
            , ("*twelve", [(0, 0, "4c"), (2, 0, "4d")])
            ]
        withCall = CallTest.withNoteGenerator "around" cAround
        extract = DeriveTest.rLogStrings
    equal logs ["prev: [0.0]", "next: [2.0]"]
    where
    cAround = CallTest.generator $ Sub.inverting $ \args -> do
        Log.warn $ "prev: " <> showt (map Event.start (Args.prevEvents args))
        Log.warn $ "next: " <> showt (map Event.start (Args.nextEvents args))
        pure Stream.empty

test_track_dynamic :: Test
test_track_dynamic = do
    let eScaleInst dyn =
            (envLookup EnvKey.scale env, envLookup EnvKey.instrument env)
            where env = Derive.stateEnviron dyn
        blockId = UiTest.defaultBlockId
    let res = DeriveTest.deriveBlocks
            [ ("b", [("*legong", [(0, 0, "1")]), (">i1", [(0, 1, "sub")])])
            , ("sub", [(">", [(0, 1, "")]), ("*", [(0, 0, "2")])])
            ]
    equal (eTrackDynamic eScaleInst res)
        [ ((UiTest.bid "b", 1), ("legong", "Nothing"))
        , ((UiTest.bid "b", 2), ("legong", "i1"))
        , ((UiTest.bid "sub", 1), ("legong", "i1"))
        , ((UiTest.bid "sub", 2), ("legong", "i1"))
        ]

    -- I get TrackDynamics from the *legong track even when there are 0 events
    -- on it.
    let res = DeriveTest.deriveTracks ""
            [(">i1", [(0, 1, "")]), ("*legong", [])]
    equal (eTrackDynamic eScaleInst res)
        [ ((blockId, 1), ("legong", "i1"))
        , ((blockId, 2), ("legong", "i1"))
        ]

    -- Controls for the note track come from the uninverted version, env from
    -- inverted version.  Otherwise, Dynamic is from the first slice.
    let res = DeriveTest.deriveTracksLinear ""
            [ ("dyn", [(0, 0, ".5")])
            , (">", [(0, 1, ""), (1, 1, ""), (2, 1, "")])
            , ("*legong", [])
            , ("dyn", [(0, 0, ".25"), (1, 0, ".5"), (2, 0, ".75")])
            , ("dyn", [(0, 0, ".25"), (1, 0, ".5"), (2, 0, ".75")])
            ]
    let eDyn = fmap (Signal.toPairs . ScoreT.valOf)
            . DeriveTest.lookupControl Controls.dynamic
            -- Map.lookup Controls.dynamic (Derive.state_signals dyn)
        eScale = envLookup EnvKey.scale . Derive.stateEnviron
        allTracks = [(blockId, n) | n <- [1..5]]
    equal (eTrackDynamic eScale res)
        (zip allTracks ("twelve" : repeat "legong"))
    equal (eTrackDynamic eDyn res)
        (zip allTracks
            [ Just [(-RealTime.larger, 1)] -- because of initial dyn
            , Just [(0, 0.5)]
            , Just [(0, 0.5)]
            , Just [(0, 0.5)]
            , Just [(0, 0.125)]
            ])

test_track_dynamic_consistent :: Test
test_track_dynamic_consistent = do
    -- Ensure that all parts of the Dynamic come from the same derivation of
    -- the track.
    let run extract = lookup (UiTest.bid "sub", 1) $
            eTrackDynamic extract $ DeriveTest.deriveBlocks
            [ ("top",
                [ ("> | env = a", [(0, 1, "sub")])
                , ("> | env = b | c = 1", [(0, 1, "sub")])
                ])
            , ("sub=ruler", [(">", [(0, 1, "")])])
            ]
        eEnv = envLookup "env" . Derive.stateEnviron
        eControl = fmap (Signal.toPairs . ScoreT.valOf)
            . DeriveTest.lookupControl "c"
    -- %c is only set in the env=b branch, so it shouldn't be set when env=a.
    equal (run eEnv) (Just "a")
    equal (run eControl) (Just Nothing)

test_prev_val :: Test
test_prev_val = do
    -- Test the prev_val and saved_val stuff in 'EvalTrack.derive_track'.
    let run = DeriveTest.extract Score.initialDynamic
            . DeriveTest.deriveTracks ""
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

envLookup :: Env.Key -> Env.Environ -> String
envLookup key = prettys . Env.lookup key

eTrackDynamic :: (Derive.Dynamic -> a) -> Derive.Result
    -> [((BlockId, TrackNum), a)]
eTrackDynamic extract = map (first (second UiTest.tidTracknum))
    . Map.toList . fmap extract . Derive.rTrackDynamic

test_track_dynamic_invert :: Test
test_track_dynamic_invert = do
    -- Ensure the correct TrackDynamic is collected even in the presence of
    -- inversion.
    let run = eTrackDynamic (eEnv . Derive.stateEnviron)
            . DeriveTest.deriveTracks ""
        eEnv e = (lookup EnvKey.instrument e, lookup EnvKey.scale e)
        lookup val = prettys . Env.lookup val
    -- Both tracks get *legong, even though >inst has to be inverted to see it.
    equal (run [(">i", [(0, 0, "")]), ("*legong", [(0, 0, "1")])])
        [ ((UiTest.defaultBlockId, 1), ("i", "legong"))
        , ((UiTest.defaultBlockId, 2), ("i", "legong"))
        ]

test_note_end :: Test
test_note_end = do
    let run = DeriveTest.deriveTracksSetup withCall ""
        withCall = CallTest.withControlGenerator "g" gen
    let result = run [(">", [(0, 4, "")]), ("dyn", [(0, 0, "g")])]
    equal (DeriveTest.extract DeriveTest.eDyn result)
        ([[(0, 0), (4, 1)]], [])
    where
    gen :: Derive.Generator Derive.Control
    gen = CallTest.generator1 $ \args -> do
        (start, end) <- Args.rangeOrNoteEnd args
        start <- Derive.real start
        end <- Derive.real end
        pure $ Signal.fromPairs [(start, 0), (end, 1)]

-- * test orphans

test_orphans :: Test
test_orphans = do
    let extract = DeriveTest.extractEvents Score.eventStart
    let run = extract . DeriveTest.deriveTracksSetup
            (withCalls <> DeriveTest.withLinear) ""
        withCalls = CallTest.withNoteGenerator "show" showSubs
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
    showSubs :: Derive.Generator Derive.Note
    showSubs = CallTest.generator $ \args -> do
        let subs = Derive.ctxSubTracks (Derive.passedCtx args)
        Log.warn $ pretty subs
        pure Stream.empty

test_record_empty_tracks :: Test
test_record_empty_tracks = do
    -- Ensure that TrackWarps and TrackDynamics are collected for empty tracks.
    let run = DeriveTest.deriveTracksLinear ""
        trackWarps = concatMap (Set.toList . TrackWarp.twTracks)
            . Derive.rTrackWarps
        trackDyn = Map.keys . Derive.rTrackDynamic

    let result = run [(">i1", []), (">i2", []), (">i3", [(0, 1, "")])]
    equal (trackWarps result) (map UiTest.mkTid [1, 2, 3])
    equal (trackDyn result)
        (map (((,) UiTest.defaultBlockId) . UiTest.mkTid) [1, 2, 3])

test_empty_parent_track :: Test
test_empty_parent_track = do
    -- Ensure orphan tracks pick the instrument up from the parent.
    -- Well, the absentee parent, since they're orphans.
    let run = DeriveTest.extract extract . DeriveTest.deriveTracksLinear ""
        extract e = (Score.eventStart e, DeriveTest.eInstrument e)
    equal (run [(">i1", [(0, 1, "t")]), (">", [(0, 1, "")])]) ([(0, "i1")], [])
    equal (run [(">i1", []), (">", [(0, 1, "")])]) ([(0, "i1")], [])

test_two_level_orphans :: Test
test_two_level_orphans = do
    -- Orphan extraction should be recursive, in case there are multiple
    -- intervening empty tracks.
    let run = DeriveTest.extract extract . DeriveTest.deriveTracksLinear ""
        extract e = (DeriveTest.eNote e, DeriveTest.eAttributes e)
    equal (run
        [ (">i", [(0, 1, "+a")])
        , (">", [(1, 1, "+b")])
        , (">", [(2, 1, "+c")])
        , (">", [(0, 1, ""), (1, 1, ""), (2, 1, "")])
        , ("*", [(0, 0, "4c"), (1, 0, "4d"), (2, 0, "4e")])
        ])
        ([((0, 1, "4c"), "+a"), ((1, 1, "4d"), "+b"), ((2, 1, "4e"), "+c")],
            [])

test_orphan_ranges :: Test
test_orphan_ranges = do
    -- These test TrackTree.track_end, indirectly by making sure it clips
    -- or doesn't clip signal correctly.
    let run = DeriveTest.extract DeriveTest.eNns
            . DeriveTest.deriveTracksLinear ""
    -- Each note has only its control.
    equal (run
        [ (">", [(1, 3, "(")])
        , (">", [])
        , (">", [(1, 1, "--a"), (2, 1, "--b")])
        , ("*", [(1, 0, "4c"), (2, 0, "4d")])
        ])
        ([[(1, 60), (2, 60)], [(2, 62)]], [])

    -- The note has the pitch after its end.
    equal (run
        [ ("*", [(0, 0, "4c"), (10, 0, "4d")])
        , (">", [])
        , (">", [(0, 1, "")])
        ])
        ([[(0, 60), (10, 60), (10, 62)]], [])

-- * misc

test_parse_error :: Test
test_parse_error = do
    let run = first (map Score.eventStart) . DeriveTest.extractLogs
            . Stream.toList . Derive.rEvents . DeriveTest.deriveTracks ""
    let (events, logs) = run [(">", [(1, 2, "- 4:12")])]
    equal events []
    equal (map (fmap Stack.toUi . Log.msgStack) logs)
        [Just [(Just (UiTest.bid "b1"), Just (UiTest.tid "b1.t1"),
            Just (1, 3))]]

test_exception_reverts_state :: Test
test_exception_reverts_state = do
    let run = DeriveTest.extract (DeriveTest.eEnviron "a")
            . DeriveTest.deriveTracks ""
    equal (run [(">", [(0, 1, "a=b | err"), (1, 1, "")])])
        ([Nothing], ["note generator not found: err"])
