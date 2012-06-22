module Derive.Call.BlockUtil_test where
import qualified Data.Map as Map

import Util.Control
import Util.Test
import qualified Derive.Call.CallTest as CallTest
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score

import qualified Perform.Signal as Signal


test_compile = do
    let controls = map Score.event_controls
        pitches = map DeriveTest.e_pitch

    let derive track = DeriveTest.extract id $ DeriveTest.derive_tracks
            [ ("tempo", [(0, 0, "2")])
            , track
            , (">i1", [(0, 1, ""), (1, 1, ""), (2, 1, "")])
            , ("c1", [(0, 0, "3"), (1, 0, "2"), (2, 0, "1")])
            ]

    let (_, logs) = derive ("*c2", [(0, 0, ".1")])
    strings_like logs ["unknown ScaleId \"c2\""]

    let mkcont vals = Map.union Derive.initial_controls
            (Map.singleton (Score.Control "c1")
            (Score.untyped (Signal.signal vals)))
        no_pitch = []

    let (events, logs) = derive ("*twelve", [(0, 0, ".1")])
    strings_like logs ["call not found: .1"]
    equal (controls events)
        [mkcont [(0, 3)], mkcont [(0.5, 2)], mkcont [(1, 1)]]
    equal (pitches events) [no_pitch, no_pitch, no_pitch]

    let (events, logs) = derive
            ("*twelve", [(0, 0, "4c"), (4, 0, "4d"), (12, 0, "i (4e)")])
    let complete_psig = Signal.signal $
                [(0, 60), (2, 62)] ++ DeriveTest.signal_interpolate 2 62 6 64
    let psig trunc = map (second Signal.y_to_nn) $
            Signal.unsignal $ Signal.truncate trunc complete_psig

    equal logs []
    -- The pitch signal gets truncated so it doesn't look like the note's decay
    -- wants to change pitch.
    equal (pitches events) [psig 1, psig 2, psig 6]

test_extract_orphans = do
    let extract = fst . DeriveTest.extract Score.event_start
    let run tracks = extract $
            DeriveTest.linear_derive_tracks with_calls tracks
        with_calls = CallTest.with_note_call "show" show_subs
    -- uncovered events are still played
    equal (run
            [ (">1", [(1, 1, "show")])
            , (">2", [(0, 1, ""), (1, 1, ""), (2, 1, "")])
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
    show_subs :: Derive.NoteCall
    show_subs = Derive.stream_generator "show" $ \_args -> do
        -- let subs = Derive.info_sub_tracks (Derive.passed_info args)
        -- Log.warn $ show (Slice_test.extract_tree subs)
        return []

test_empty_parent_track = do
    -- Ensure orphan tracks pick the instrument up from the parent.
    -- Well, the absentee parent, they're orphans.
    let run = DeriveTest.extract extract . DeriveTest.linear_derive_tracks id
        extract e = (Score.event_start e, DeriveTest.e_inst e)
    equal (run [(">i1", [(0, 1, "t")]), (">", [(0, 1, "")])]) ([(0, "i1")], [])
    equal (run [(">i1", []), (">", [(0, 1, "")])]) ([(0, "i1")], [])
