-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | This also tests Derive.Note and Derive.Control since it uses them to
    piece together a complete deriver.
-}
module Derive.Derive_test where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Util.Log as Log
import qualified Util.Lists as Lists
import qualified Cmd.Simple as Simple
import qualified Derive.Attrs as Attrs
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Env as Env
import qualified Derive.PSignal as PSignal
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.Stack as Stack
import qualified Derive.Tempo as Tempo
import qualified Derive.TrackWarp as TrackWarp
import qualified Derive.Warp as Warp

import qualified Instrument.Common as Common
import qualified Midi.Key as Key
import qualified Midi.Midi as Midi
import qualified Perform.Midi.Patch as Patch
import qualified Perform.Midi.Types as Midi.Types
import qualified Perform.NN as NN
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import qualified Perform.Transport as Transport

import qualified Ui.Id as Id
import qualified Ui.Skeleton as Skeleton
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest

import           Global
import           Types
import           Util.Test


test_basic :: Test
test_basic = do
    -- verify the three phases of derivation
    -- 1: derivation to score events
    let res = DeriveTest.deriveTracks ""
            [ (">i1", [(0, 16, ""), (16, 16, "")])
            , ("*", [(0, 0, "4c"), (16, 0, "4c#")])
            , ("dyn", [(0, 0, "1"), (16, 0, ".5")])
            ]
    let ((perfEvents, mmsgs), logs) =
            DeriveTest.performDefaults (Derive.rEvents res)
    equal (eEvents res) ([(0, 16, ""), (16, 16, "")], [])

    -- 2: conversion to midi perf events
    let evt = (,,,) "i1"
    equal (map extractPerfEvent perfEvents)
        [ evt 0 16 (mkstack (0, 16))
        , evt 16 16 (mkstack (16, 32))
        ]

    -- 3: performance to midi protocol events
    equal (DeriveTest.noteOnVel mmsgs) [(0, 60, 127), (16000, 61, 64)]
    equal logs []
    where
    {-
        b1
        t1  t2  t3  (t1)
        >i1 *   dyn (note-track)
        |   4c  1   |
        |           |
        v           v
        |   4c# .5  |
        |           |
        v           v
    -}
    mkstack (s, e) = Stack.fromOutermost
        [ blockCall UiTest.defaultBlockId
        , Stack.Serial 0
        , Stack.Block UiTest.defaultBlockId
        , Stack.Track (UiTest.mkTid 1)
        -- track title and event for note track
        , Stack.Call "note-track"
        , Stack.Region s e
        , Stack.Call "note"
        , Stack.Serial 0
        , Stack.Track (UiTest.mkTid 2)
        , Stack.Track (UiTest.mkTid 3)
        -- t1 shows up again inverted
        , Stack.Track (UiTest.mkTid 1)
        , Stack.Call "note-track" -- inverted note track is >
        , Stack.Region s e
        ]
    blockCall bid = Stack.Call $ "block " <> showt bid
    extractPerfEvent (Midi.Types.Event start dur patch _controls _pitch
            _svel _evel stack) =
        (Midi.Types.patchName patch, RealTime.toSeconds start,
            RealTime.toSeconds dur, stack)

test_qualified_symbol :: Test
test_qualified_symbol = do
    let run = DeriveTest.extract DeriveTest.eAttributes
            . DeriveTest.deriveTracks ""
    equal (run [(">", [(0, 2, "bali.gong.cycle \"(+a) 1")])])
        (["+a", "+a"], [])

test_round_pitch :: Test
test_round_pitch = do
    -- A note sufficiently close to 4c becomes 4c.
    let ((_, mmsgs), _) =
            DeriveTest.performResult DeriveTest.performDefaults $
            DeriveTest.deriveTracks ""
                [(">i1", [(0, 1, "")]), ("*", [(0, 0, "3b 99.99")])]
    -- 100 means it's not getting a dyn, and is using the default
    equal (DeriveTest.noteOnVel mmsgs) [(0, Key.c4, 127)]

test_override_default_pitch :: Test
test_override_default_pitch = do
    let f title pitch = DeriveTest.extract DeriveTest.eNns $
            DeriveTest.deriveTracks title [(">", [(4, 4, "")]), ("*", pitch)]
    equal (f "" [(4, 0, "4c"), (6, 0, "4d")])
        ([[(4, NN.c4), (6, NN.c4), (6, NN.d4)]], [])
    -- Both overriding PITCH works, and it picks up defaults properly.
    equal (f "*PITCH = set-or-move | set-or-move-time=2"
            [(4, 0, "4c"), (6, 0, "4d")])
        ([[(4, NN.c4), (6, NN.c4), (8, NN.d4)]], [])


test_attributes :: Test
test_attributes = do
    -- Test that attributes work, through derivation and performance.
    let convertLookup = DeriveTest.makeConvertLookup allocs db
        allocs = Simple.allocations
            [("i1", ("s/i1", Simple.Midi [("wdev", 0)]))]
        db = UiTest.makeDb [("s", [patch])]
            where
            patch = Patch.attributeMap #= attrMap $ Patch.patch (-1, 1) "i1"
        keyswitches = Patch.singleKeyswitches $ map (first Attrs.attrs)
            [ (["a1", "a2"], 0)
            , (["a0"], 1)
            , (["a1"], 2)
            , (["a2"], 3)
            ]
        keymap = Patch.unpitchedKeymap [(Attrs.attr "km", 42)]
        attrMap = Common.AttributeMap $ case (keyswitches, keymap) of
            (Common.AttributeMap a, Common.AttributeMap b) -> a ++ b
    let res = DeriveTest.deriveTracks ""
            [ (">i1 | +a1", [(0, 1, "+a0"), (1, 1, "+a2")])
            , ("*twelve", [(0, 0, "4c")])
            ]
        ((_, mmsgs), logs) = DeriveTest.perform convertLookup
            allocs (Derive.rEvents res)

    -- Attribute inheritance thing works.
    equal (DeriveTest.extract DeriveTest.eAttributes res)
        (["+a0+a1", "+a1+a2"], [])
    equal (map DeriveTest.showLog logs)
        -- TODO re-enable when Convert.warn_unused_attributes is configurable
        -- ["attrs have no match in keyswitches or keymap of >s/ks: +a1"]
        []
    -- Attrs get the proper keyswitches and keymap keys.
    equal (noteOnKeys mmsgs) [1, Key.c4, 0, Key.c4]

noteOnKeys :: [Midi.WriteMessage] -> [Midi.Key]
noteOnKeys msgs =
    [nn | Midi.ChannelMessage _ (Midi.NoteOn nn _) <- map Midi.wmsgMsg msgs]

test_stack :: Test
test_stack = do
    let extract = fst . DeriveTest.extract Score.eventStack
    let stacks = extract $ DeriveTest.deriveBlocks
            [ ("b0", [(">i1", [(0, 1, ""), (1, 1, "sub")])])
            , ("sub", [(">", [(0, 1, ""), (1, 1, "")])])
            ]
    let block = Stack.Block . UiTest.bid
        blockCall bid = Stack.Call $ "block " <> showt (UiTest.bid bid)
        track name num = Stack.Track (UiTest.mkTidName name num)
        call = Stack.Call
    equal (map (map Stack.unparseUiFrame . Stack.toUi) stacks)
        [ ["test/b0 test/b0.t1 0-1"]
        , ["test/b0 test/b0.t1 1-2", "test/sub test/sub.t1 0-1"]
        , ["test/b0 test/b0.t1 1-2", "test/sub test/sub.t1 1-2"]
        ]

    let b0 s e =
            [ blockCall "b0", Stack.Serial 0, block "b0", track "b0" 1
            , call "note-track", Stack.Region s e
            ]
        sub s e =
            [ blockCall "sub", Stack.Serial 0, block "sub", track "sub" 1
            , call "note-track", Stack.Region s e, call "note", Stack.Serial 0
            ]
    equal stacks $ map Stack.fromOutermost
        [ b0 0 1 ++ [Stack.Call "note", Stack.Serial 0]
        , b0 1 2 ++ sub 0 1
        , b0 1 2 ++ sub 1 2
        ]

test_stack_after_exception :: Test
test_stack_after_exception = do
    -- The stack is properly rewound after an exception.  This actually tests
    -- that the state is rolled back if event evaluation produces an exception,
    -- which is more directly tested in "Derive.EvalTrack_test".  Amazingly,
    -- it didn't for a very long time, and the bug was only revealed in this
    -- way.
    let result = DeriveTest.deriveBlocks
            [ ("top -- >>i1 = \"(%dyn = set .8)",
                [(">", [(0, 4, "b1"), (4, 4, "b2")])])
            , ("b1", [(">i1", [])])
            , ("b2", [(">i1", [])])
            ]
    let extract = fmap Stack.prettyUi_ . Log.msgStack
    equal (mapMaybe extract $ DeriveTest.rLogs result)
        [ "top top.t1 0-4 / b1 b1.t1 *"
        , "top top.t1 4-8 / b2 b2.t1 *"
        ]

test_simple_subderive :: Test
test_simple_subderive = do
    let (events, msgs) = DeriveTest.extract DeriveTest.eNote $
            DeriveTest.deriveBlocks
            [ ("parent", [(">i1", [(0, 2, "sub"), (2, 1, "sub")])])
            , ("sub=ruler", UiTest.regularNotes 2)
            ]
    equal msgs []
    equal events
        [ (0, 1, "3c"), (1, 1, "3d")
        , (2, 0.5, "3c"), (2.5, 0.5, "3d")
        ]

test_subderive :: Test
test_subderive = do
    let run evts = DeriveTest.deriveBlocks
            [ ("b0",
                [ ("tempo", [(0, 0, "2")])
                , (">i1", evts)
                ])
            , ("sub=ruler", [(">i2", [(1, 1, "--sub1")])])
            , ("empty", [(">i1", [])])
            ]
    -- I used to test recursive call, but now that the block call doesn't
    -- catch that explicitly it means I get random crap before it finally
    -- aborts due to the call stack limit.
    let (events, msgs) = DeriveTest.rSplit $ run
            [(0, 1, "nosuch"), (1, 1, "empty"), (3, 1, "--x")]
    -- errors don't stop derivation, and an empty sub-block is ignored
    equal (map DeriveTest.eEvent events) [(1.5, 0.5, "--x")]
    stringsLike (map DeriveTest.showLog msgs) ["not found: nosuch"]
    equal (map (DeriveTest.showStack . Log.msgStack) msgs)
        ["b0 b0.t2 0-1: note-track"]

    let res = run [(0, 8, "--b1"), (8, 8, "sub"), (16, 1, "--b2")]
        (events, msgs) = DeriveTest.rSplit res
    equal (map DeriveTest.eEvent events)
        [(0, 4, "--b1"), (6, 2, "--sub1"), (8, 0.5, "--b2")]
    equal (map Score.eventInstrument events) ["i1", "i2", "i1"]
    equal msgs []

    let b0 pos = (UiTest.bid "b0", [(UiTest.mkTidName "b0" 1, pos),
            (UiTest.mkTidName "b0" 2, pos)])
        sub pos = (UiTest.bid "sub", [(UiTest.mkTidName "sub" 1, pos)])
    equal (map (invTempo res) (Lists.range 0 10 2))
        [[b0 0], [b0 4], [b0 8, sub 0], [b0 12, sub 1], [b0 16, sub 2], []]

    -- For eyeball verification.
    -- pprint (r_events res)
    -- pprint $ zip [0,2..] $ map (inv_tempo res) (Lists.range 0 10 2)
    -- pprint $ Derive.state_track_warps state

test_subderive_timing :: Test
test_subderive_timing = do
    -- Just make sure that sub-blocks stretch to the correct times.
    let (events, logs) = eEvents $ DeriveTest.deriveBlocks
            [ ("p",
                [ ("tempo", [(0, 0, ".5")])
                , (">i1", [(0, 2, "sub"), (5, 1, "sub")])
                ])
            , ("sub=ruler", [(">i1", [(0, 1, ""), (1, 1, "")])])
            ]
    equal events
        [ (0, 2, ""), (2, 2, "")
        , (10, 1, ""), (11, 1, "")
        ]
    equal logs []

test_subderive_error :: Test
test_subderive_error = do
    let run evts = eEvents $ DeriveTest.deriveBlocks
            [ ("b0", [ (">i1", evts) ])
            , ("sub", [("blah *error syntax", [(1, 1, "--sub1")]), (">", [])])
            ]
    let (events, logs) = run [(0, 1, "sub")]
    equal events []
    stringsLike logs ["track title: parse error"]

test_subderive_multiple :: Test
test_subderive_multiple = do
    -- make sure subderiving a block with multiple tracks works correctly
    let ((_, mmsgs), logs) =
            DeriveTest.performResult DeriveTest.performDefaults $
            DeriveTest.deriveBlocks
                [ ("b0",
                    [ ("tempo", [(0, 0, "2")])
                    , ("dyn", [(0, 0, "1"), (8, 0, "i 0")])
                    , (">i1", [(0, 8, "sub")])
                    ])
                , ("sub=ruler",
                    [ (">", [(0, 1, ""), (1, 1, "")])
                    , ("*twelve", [(0, 0, "4c"), (1, 0, "4d")])
                    , (">", [(0, 1, ""), (1, 1, "")])
                    , ("*twelve", [(0, 0, "5c"), (1, 0, "5d")])
                    ])
                ]
    equal (DeriveTest.noteOnVel mmsgs)
        [ (0, Key.c5, 127), (0, Key.c4, 127)
        , (2000, Key.d5, 64), (2000, Key.d4, 64)
        ]
    equal logs []

test_multiple_subderive :: Test
test_multiple_subderive = do
    -- make sure a sequence of sub calls works
    let res = DeriveTest.deriveBlocks
            [ ("b0", [(">i1", [(0, 2, "sub"), (2, 2, "sub"), (4, 2, "sub")])])
            , ("sub=ruler", [(">", [(0, 1, "--sub1")])])
            ]
    equal (eEvents res)
        ([(0, 2, "--sub1"), (2, 2, "--sub1"), (4, 2, "--sub1")], [])

    -- Empty inst inherits calling inst.
    equal (fst (DeriveTest.extract Score.eventInstrument res))
        (replicate 3 "i1")

    let pos = map (invTempo res) (Lists.range 0 6 1)
    let b0 pos = (UiTest.bid "b0", [(UiTest.mkTidName "b0" 1, pos)])
        sub pos = (UiTest.bid "sub", [(UiTest.mkTidName "sub" 1, pos)])
    equal (map List.sort pos)
        [ [b0 0, sub 0], [b0 1, sub 0.5], [b0 2, sub 0, sub 1]
        , [b0 3, sub 0.5] , [b0 4, sub 0, sub 1]
        , [b0 5, sub 0.5], [b0 6, sub 1]
        ]

test_tempo_compose :: Test
test_tempo_compose = do
    let run tempo events subTempo = eEvents $ DeriveTest.deriveBlocks
            [ ("b0", [("tempo", tempo), (">i1", events)])
            , ("sub=ruler",
                [ ("tempo", subTempo)
                , (">", [(0, 1, ""), (1, 1, "")])
                ])
            ]

    equal (run [(0, 0, "1")] [(2, 2, "sub")] [(0, 0, "1")]) $
        ([(2, 1, ""), (3, 1, "")], [])
    -- Tempo of the sub doesn't matter since it will be stretched to fit.
    equal (run [(0, 0, "1")] [(2, 2, "sub")] [(0, 0, "2")]) $
        ([(2, 1, ""), (3, 1, "")], [])
    equal (run [(0, 0, "1")] [(2, 2, "sub")] [(0, 0, ".5")]) $
        ([(2, 1, ""), (3, 1, "")], [])

    -- Make sure the top level block doesn't get stretched.
    equal (run [(0, 0, "2")] [(0, 2, "--1"), (2, 2, "sub"), (4, 2, "--2")]
            [(0, 0, ".5")]) $
        ([(0, 1, "--1"), (1, 0.5, ""), (1.5, 0.5, ""), (2, 1, "--2")], [])

    equal (run [(0, 0, "1"), (2, 0, "2")] [(0, 2, "sub")] [(0, 0, "1")]) $
        ([(0, 1, ""), (1, 1, "")], [])
    equal (run [(0, 0, "2"), (4, 0, ".5")] [(0, 4, "sub"), (4, 4, "sub")]
            [(0, 0, "1")]) $
        ([(0, 1, ""), (1, 1, ""), (2, 4, ""), (6, 4, "")], [])

    -- TODO test when the subblock has a tempo too

test_warp_ops :: Test
test_warp_ops = do
    let run op = DeriveTest.eval Ui.empty (op record)
        record = do
            x0 <- Derive.real (0 :: ScoreTime)
            x1 <- Derive.real (2 :: ScoreTime)
            pure [x0, x1]

    equal (run id) $ Right [0, 2]
    equal (run (Derive.stretch 2)) $ Right [0, 4]
    equal (run (Derive.at 2)) $ Right [2, 4]
    equal (run (Derive.at 1 . Derive.stretch 2)) $ Right [1, 5]
    equal (run (Derive.at 2 . Derive.stretch 2)) $ Right [2, 6]
    equal (run (Derive.at 2 . Derive.stretch 0.5)) $ Right [2, 3]
    equal (run (Derive.stretch 2 . Derive.at 1)) $ Right [2, 6]
    equal (run (Derive.stretch 2 . Derive.at 2)) $ Right [4, 8]
    equal (run (Derive.stretch 0.5 . Derive.at 2)) $ Right [1, 2]
    equal (run (Derive.place 1 2)) $ Right [1, 5]

    -- test compose
    let plain = Warp.fromSignal $ Signal.fromPairs [(0, 0), (100, 100)]
        slow = Warp.fromSignal $ Signal.fromPairs [(0, 0), (100, 200)]

    equal (run (Internal.warp plain)) $ Right [0, 2]
    equal (run (Derive.at 2 . Internal.warp plain)) $ Right [2, 4]
    equal (run (Derive.stretch 2 . Internal.warp plain)) $ Right [0, 4]

    equal (run (Internal.warp plain . Internal.warp plain)) $ Right [0, 2]
    equal (run (Internal.warp plain . Internal.warp slow)) $ Right [0, 4]
    equal (run (Internal.warp slow . Internal.warp plain)) $ Right [0, 4]
    equal (run (Internal.warp slow . Internal.warp slow)) $ Right [0, 8]
    equal (run (Derive.stretch 2 . Internal.warp slow)) $ Right [0, 8]
    equal (run (Derive.stretch 2 . Internal.warp slow . Internal.warp slow)) $
        Right [0, 16]

    equal (run (Internal.warp slow)) $ Right [0, 4]
    equal (run (Derive.stretch 2 . Internal.warp slow)) $ Right [0, 8]
    equal (run (Derive.at 1 . Internal.warp slow)) $ Right [1, 5]
    equal (run (Derive.at 1 . Derive.stretch 2 . Derive.stretch 2)) $
        Right [1, 9]
    equal (run (Derive.at 1 . Derive.stretch 2 . Internal.warp slow)) $
        Right [1, 9]

    equal (run (Internal.warp slow . Derive.at 1)) $ Right [2, 6]
    equal (run (Derive.at 1 . Derive.stretch 2 . Internal.warp slow
            . Internal.warp slow)) $
        Right [1, 17]

test_real_to_score_round_trip :: Test
test_real_to_score_round_trip = do
    let f doWarp = DeriveTest.eval Ui.empty $
            doWarp (Derive.realToScore =<< Derive.scoreToReal 1)
    let slow = Warp.fromSignal $ Signal.fromPairs [(0, 0), (100, 200)]
    equal (f id) (Right 1)
    equal (f (Derive.at 5)) (Right 1)
    equal (f (Derive.stretch 5)) (Right 1)
    equal (f (Derive.stretch 5 . Derive.at 5)) (Right 1)
    equal (f (Internal.warp slow . Derive.stretch 5 . Derive.at 5)) (Right 1)
    equal (f (Derive.stretch 5 . Derive.at 5 . Internal.warp slow)) (Right 1)

test_shift_controls :: Test
test_shift_controls = do
    let setControls = DeriveTest.modifyDynamic $ \st -> st
            { Derive.stateEnviron = Env.fromList
                [ ( "cont"
                  , toSignal $ Signal.fromPairs [(0, 1), (2, 2), (4, 0)]
                  )
                ]
            , Derive.statePitch = DeriveTest.psignal [(0, "4c")]
            }
    let run op = DeriveTest.extractRun extract $
            DeriveTest.run Ui.empty (setControls >> op get)
            where
            get = do
                conts <- Derive.getControlMap
                psig <- Internal.getDynamic Derive.statePitch
                pure (conts, psig)
            extract (conts, pitch) =
                (unsignal conts, first Signal.toPairs (PSignal.toNn pitch))
            unsignal =
                Signal.toPairs . ScoreT.valOf . snd . head . Map.toList
    equal (run id) $ Right ([(0, 1), (2, 2), (4, 0)], ([(0, 60)], []))
    equal (run $ Derive.shiftControls 2) $
        Right ([(2, 1), (4, 2), (6, 0)], ([(2, 60)], []))

toSignal :: Signal.Control -> DeriveT.Val
toSignal = DeriveT.VSignal . ScoreT.untyped

test_tempo_funcs1 :: Test
test_tempo_funcs1 = do
    let ([tTid, tid1], uiState) = UiTest.runMkblock
            [ ("tempo", [(0, 0, "2")])
            , (">i1", [(0, 8, "--b1"), (8, 8, "--b2"), (16, 1, "--b3")])
            ]
        bid = UiTest.defaultBlockId
    let res = DeriveTest.deriveBlock uiState bid
    equal (DeriveTest.rLogStrings res) []

    -- [(BlockId, [(TrackId, ScoreTime)])]
    let b0 pos = (bid, [(tTid, pos), (tid1, pos)])
    equal (map (invTempo res) [0, 2, 4, 6]) [[b0 0], [b0 4], [b0 8], [b0 12]]
    equal (invTempo res (RealTime.fromScore UiTest.defaultBlockEnd)) []

    equal (map (rTempo res bid tTid) (Lists.range 0 10 2))
        (map ((:[]) . RealTime.seconds) (Lists.range 0 5 1))

test_tempo_funcs2 :: Test
test_tempo_funcs2 = do
    let ([tTid1, tid1, tTid2, tid2], uiState) =
            UiTest.runMkblock
            -- UiTest.run Ui.empty $ UiTest.mkblocks $ (:[]) $ ("b0",) $
                [ ("tempo", [(0, 0, "2")])
                , (">i1", [(0, 8, "--b1"), (8, 8, "--b2"), (16, 1, "--b3")])
                , ("tempo", [(0, 0, "1")])
                , (">i2", [(0, 16, "--2b1")])
                ]
        bid = UiTest.defaultBlockId
    let res = DeriveTest.deriveBlock uiState bid
    equal (DeriveTest.rLogStrings res) []
    equal (map (rTempo res bid tTid1) (Lists.range 0 10 2))
        (map ((:[]) . RealTime.seconds) (Lists.range 0 5 1))
    equal (map (rTempo res bid tTid2) (Lists.range 0 10 2))
        (map ((:[]) . RealTime.seconds) (Lists.range 0 10 2))
    let b0 pos = (bid, [(tTid1, pos), (tid1, pos)])
        b1 pos = (bid, [(tTid2, pos), (tid2, pos)])
    equal (map (invTempo res) [0, 2, 4, 6])
        [[b0 0, b1 0], [b0 4, b1 2], [b0 8, b1 4], [b0 12, b1 6]]

    -- test multiple tempo
    -- test subderive

-- | Map through inv tempo and sort the results since their order isn't
-- relevant.
invTempo :: Derive.Result -> RealTime -> [(BlockId, [(TrackId, ScoreTime)])]
invTempo res =
    map (second List.sort) . List.sort . rInvTempo res Transport.StopAtEnd

test_tempo_funcs_multiple_subblocks :: Test
test_tempo_funcs_multiple_subblocks = do
    -- A single score time can imply multiple real times.
    let res = DeriveTest.deriveBlocks
            [ ("parent", [(">i1", [(0, 1, "sub"), (1, 1, "sub")])])
            , ("sub=ruler", [(">i1", [(0, 1, "")])])
            ]
    equal (rTempo res (UiTest.bid "sub") (UiTest.mkTidName "sub" 1) 0.5)
        [0.5, 1.5]

test_fractional_pitch :: Test
test_fractional_pitch = do
    -- A pitch that requires pitch bends should distribute across multiple
    -- channels.  Yes, this is also tested in Perform.Midi.Perform, but this
    -- also tests that the pitch signal is trimmed properly by
    -- Note.trim_pitches.
    let ((_perfEvents, mmsgs), logs) =
            DeriveTest.performResult DeriveTest.performDefaults $
                DeriveTest.deriveTracks ""
                    [ (">i1", [(0, 16, ""), (16, 16, "")])
                    , ("*just", [(0, 16, "4c"), (16, 16, "4d")])
                    ]

    equal logs []
    equal [(chan, nn) | Midi.ChannelMessage chan (Midi.NoteOn nn _)
            <- map Midi.wmsgMsg mmsgs]
        [(1, Key.c4), (2, Key.d4)]

test_control :: Test
test_control = do
    let ((perfEvents, mmsgs), logs) =
            DeriveTest.performResult DeriveTest.performDefaults res
        res = DeriveTest.deriveTracks ""
            [ (">i1", [(0, 1, ""), (1, 1, "")])
            , ("*", [(0, 1, "4c"), (1, 1, "4c#")])
            , ("cc1", [(0, 0, "1"), (1, 0, "i .75"), (2, 0, "i 0")])
            ]

    -- Cursory checks, more detailed checks are in more Note_test and
    -- Control_test.
    equal logs []
    equal (fst $ eEvents res) [(0, 1, ""), (1, 1, "")]
    equal (length perfEvents) 2

    -- Just make sure it did in fact emit ccs.
    check ("has cc: " <> pretty mmsgs) $
        any Midi.isCc (map Midi.wmsgMsg mmsgs)

test_make_inverse_tempo_func :: Test
test_make_inverse_tempo_func = do
    -- This is actually also tested in test_subderive.
    -- TODO and it belongs in TrackWarp_test now
    let trackId = Id.TrackId (UiTest.mkid "warp")
        warp = Tempo.tempoToWarp (Signal.constant 2)
        trackWarps = [TrackWarp.TrackWarp
                0 2 UiTest.defaultBlockId (Set.singleton trackId) warp]
    let f = TrackWarp.inverseTempoFunc trackWarps Transport.StopAtEnd
        withBlock pos = [(UiTest.defaultBlockId, [(trackId, pos)])]
    -- Fast tempo means ScoreTime passes quickly relative to Timestamps.
    -- Second 2 at tempo 2 is trackpos 4, which is at the end of the block.
    equal (map (f . RealTime.seconds) [0..3])
        [withBlock 0, withBlock 2, withBlock 4, []]

test_tempo_roundtrip :: Test
test_tempo_roundtrip = do
    let trackId = Id.TrackId (UiTest.mkid "warp")
        warp = Tempo.tempoToWarp (Signal.constant 0.987)
        trackWarps = [TrackWarp.TrackWarp
                0 10 UiTest.defaultBlockId (Set.singleton trackId) warp]
    let inv = TrackWarp.inverseTempoFunc trackWarps Transport.StopAtEnd
        tempo = TrackWarp.tempoFunc trackWarps
    let rtimes = concatMap (tempo UiTest.defaultBlockId trackId)
            (Lists.range 0 3 1)
        stimes = concatMap inv rtimes
    pprint rtimes
    pprint stimes
    -- expected failure
    -- equal (map snd (concatMap snd stimes)) [0..3]

test_named_pitch :: Test
test_named_pitch = do
    let run op = DeriveTest.eval Ui.empty (op $ Derive.namedNnAt "psig" 2)
        pitch = DeriveTest.mkpitch12 "4c"
        withConst pname = Derive.withNamedPitch pname
            (PSignal.constant pitch)
    equal (run (withConst "psig")) (Right (Just 60))
    leftLike (run (withConst "bad")) "no named pitch #psig"

test_block_end :: Test
test_block_end = do
    -- Make sure the pitch for the sub block event is trimmed to the end
    -- of the block, since there's no next event for it.
    let res = DeriveTest.extract DeriveTest.eNns $ DeriveTest.deriveBlocks
            [ ("p",
                [ (">i1", [(0, 1, "sub"), (1, 1, "")])
                , ("*twelve", [(0, 0, "5d"), (1, 0, "5e")])
                ])
            , ("sub", [(">", [(0, 1, "")])])
            ]
    equal res ([[(0, 74), (1, 74)], [(1, 76)]], [])

-- * regression

-- These tests are derived from debugging and test specific things that went
-- wrong.  I don't think they're likely enough to come up again that I want
-- to figure out the right place to test, but as long as they exist I might
-- as well keep them for regressions.

test_regress_pedal :: Test
test_regress_pedal = do
    -- Make sure a pedal halfway through a note really only turns on halfway
    -- through the note.
    let ((_perfEvents, mmsgs), _logs) = DeriveTest.performResult
            DeriveTest.performDefaults (deriveBlocks blocks)
    let pedalOn =
            [ (ts, c)
            | (ts, Midi.ChannelMessage _ (Midi.ControlChange 64 c))
                <- DeriveTest.extractMidi mmsgs
            , c /= 0
            ]
    equal pedalOn [(12500, 127)]
    where
    blocks = [(("b10",
        [("pedal", [(12.5, 2, "`ped`")]),
         (">i1", [ (10.0, 5.0, "")]),
         ("*", [ (10.0, 0.0, "4f")])
        ]), [(1, 2), (2, 3)])]

test_regress_event_end1 :: Test
test_regress_event_end1 = do
    -- Ensure that notes get the proper next event even when it has been
    -- sliced off.  Previously it extended to the block end and the notes
    -- wound up with too much pitch signal.
    let res = deriveBlocks blocks
        extract e = (Score.eventStart e, Score.eventDuration e,
                DeriveTest.eNns e)
    equal (DeriveTest.extract extract res)
        ( [ (0, 2, [(0, 60), (2, 60)])
          , (2, 2, [(2, 62)])
          ]
        , []
        )
    where
    blocks = [(("b0", b0), [(1, 2), (2, 3)])]
    b0 =
        [ (">", [(0, 0, "`arp-up`")])
        , (">i1", [(0, 2, ""), (2, 2, "")])
        , ("*", [(0, 0, "4c"), (2, 0, "4d")])
        ]

test_regress_event_end2 :: Test
test_regress_event_end2 = do
    let res = deriveBlocks blocks
        extract e = (Score.eventStart e, Score.eventDuration e,
                DeriveTest.eNns e)
    equal (DeriveTest.extract extract res)
        ( [ (5, 2, [(5, 60), (7, 60)])
          , (7, 2, [(7, 62)])
          ]
        , []
        )
    where
    blocks = [(("b0",
       [(">", [(5, 0, "`arp-up`")]),
        (">", [(5, 2, ""), (7, 2, "")]),
        ("*", [(5, 0, "4c"), (7, 0, "4d")])
        ]),
      [(1, 2), (2, 3)])]

deriveBlocks :: [(UiTest.BlockSpec, [Skeleton.Edge])] -> Derive.Result
deriveBlocks blocks = DeriveTest.deriveBlock state (UiTest.bid blockName)
    where
    state = UiTest.exec Ui.empty (UiTest.mkblocksSkel blocks)
    ((blockName, _), _) : _ = blocks

-- * util

rTempo :: Derive.Result -> Transport.TempoFunction
rTempo = TrackWarp.tempoFunc . Derive.rTrackWarps

rInvTempo :: Derive.Result -> Transport.InverseTempoFunction
rInvTempo = TrackWarp.inverseTempoFunc . Derive.rTrackWarps

eEvents :: Derive.Result -> ([(RealTime, RealTime, Text)], [Text])
eEvents = DeriveTest.extract DeriveTest.eEvent
