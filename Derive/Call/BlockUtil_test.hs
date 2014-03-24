-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.BlockUtil_test where
import qualified Data.Map as Map
import qualified Data.Set as Set

import Util.Control
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.Call.CallTest as CallTest
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import qualified Derive.TrackWarp as TrackWarp

import qualified Perform.Signal as Signal


test_compile = do
    let controls = map (Map.filterWithKey (\k _ -> not (Controls.is_private k))
            . Score.event_controls)
        pitches = map DeriveTest.e_nns

    let derive track = DeriveTest.extract id $ DeriveTest.derive_tracks ""
            [ ("tempo", [(0, 0, "2")])
            , track
            , (">i1", [(0, 1, ""), (1, 1, ""), (2, 1, "")])
            , ("c1", [(0, 0, "3"), (1, 0, "2"), (2, 0, "1")])
            ]

    let (_, logs) = derive ("*c2", [(0, 0, ".1")])
    strings_like logs ["get_scale: unknown \\*c2"]

    let mkcont vals = Map.union Derive.initial_controls
            (Map.singleton "c1" (Score.untyped (Signal.signal vals)))
        no_pitch = []

    let (events, logs) = derive ("*twelve", [(0, 0, ".1")])
    strings_like logs ["not found: .1"]
    equal (controls events)
        [mkcont [(0, 3)], mkcont [(0.5, 2)], mkcont [(1, 1)]]
    equal (pitches events) [no_pitch, no_pitch, no_pitch]

    let (events, logs) = derive
            ("*twelve", [(0, 0, "4c"), (4, 0, "4d"), (12, 0, "i (4e)")])
    let complete_psig = Signal.signal $
                [(0, 60), (2, 62)] ++ DeriveTest.signal_interpolate 2 62 6 64
    let psig trunc = map (second Signal.y_to_nn) $
            Signal.unsignal $ Signal.drop_after trunc complete_psig

    equal logs []
    -- The pitch signal gets truncated so it doesn't look like the note's decay
    -- wants to change pitch.
    equal (pitches events) [psig 1, psig 2, psig 7]

test_extract_orphans = do
    let extract = fst . DeriveTest.extract Score.event_start
    let run = extract . DeriveTest.derive_tracks_with_ui with_calls
            DeriveTest.with_linear ""
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
    show_subs = Derive.make_call "test" "show" mempty "doc" $
        Sig.call0 $ \_ -> do
            -- let subs = Derive.info_sub_tracks (Derive.passed_info args)
            -- Log.warn $ show (Slice_test.extract_tree subs)
            return []

test_record_empty_tracks = do
    -- Ensure that TrackWarps and TrackDynamics are collected for empty tracks.
    let run = DeriveTest.derive_tracks_linear ""
        track_warps = concatMap (Set.toList . TrackWarp.tw_tracks)
            . Derive.r_track_warps
        track_dyn = Map.keys . (\(Derive.TrackDynamic d) -> d)
            . Derive.r_track_dynamic

    let result = run [(">i1", []), (">i2", []), (">i3", [(0, 1, "")])]
    equal (track_warps result) (map UiTest.mk_tid [1, 2, 3])
    equal (track_dyn result)
        (map (((,) UiTest.default_block_id) . UiTest.mk_tid) [1, 2, 3])

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

test_empty_parent_track = do
    -- Ensure orphan tracks pick the instrument up from the parent.
    -- Well, the absentee parent, since they're orphans.
    let run = DeriveTest.extract extract . DeriveTest.derive_tracks_linear ""
        extract e = (Score.event_start e, DeriveTest.e_inst e)
    equal (run [(">i1", [(0, 1, "t")]), (">", [(0, 1, "")])]) ([(0, "i1")], [])
    equal (run [(">i1", []), (">", [(0, 1, "")])]) ([(0, "i1")], [])

test_control_call = do
    let run tracks = DeriveTest.extract extract $ DeriveTest.derive_blocks
            [ ("top", tracks)
            , ("c=ruler", [("%", [(0, 0, "1"), (1, 0, ".5"), (2, 0, "--")])])
            ]
        extract e = (Score.event_start e, Score.initial_dynamic e)
        dyn_call = ("dyn", [(0, 2, "c"), (2, 2, "c")])
    equal (run $ dyn_call : UiTest.regular_notes 4)
        ([(0, 1), (1, 0.5), (2, 1), (3, 0.5)], [])
    equal (run $ UiTest.regular_notes 4 ++ [dyn_call])
        ([(0, 1), (1, 0.5), (2, 1), (3, 0.5)], [])
