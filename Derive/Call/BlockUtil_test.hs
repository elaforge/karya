-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.BlockUtil_test where
import qualified Data.Map as Map

import Util.Control
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Environ as Environ
import qualified Derive.Score as Score

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
            Signal.unsignal $ Signal.drop_at_after trunc complete_psig

    equal logs []
    -- The pitch signal gets truncated so it doesn't look like the note's decay
    -- wants to change pitch.
    equal (pitches events) [psig 1, psig 2, psig 7]

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

test_track_voice = do
    let run skel = DeriveTest.extract extract
            . DeriveTest.derive_tracks_with_ui id (DeriveTest.with_skel skel) ""
        extract :: Score.Event -> Maybe Int
        extract = DeriveTest.e_environ_val Environ.track_voice
        track inst = (inst, [(0, 1, "")])
    equal (run [] [track ">s/1", track ">s/2", track ">s/1", track ">"])
        ([Just 0, Just 0, Just 1, Nothing], [])
    equal (run [(1, 2), (1, 3)] [(">s/1", []), track ">s/1", track ">s/1"])
        ([Just 1, Just 2], [])
    equal (run [(1, 2), (1, 3)] [("dyn", []), track ">s/1", track ">s/1"])
        ([Just 0, Just 1], [])
