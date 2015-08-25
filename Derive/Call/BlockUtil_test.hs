-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.BlockUtil_test where
import qualified Data.Map as Map

import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Score as Score

import qualified Perform.Signal as Signal


test_compile = do
    let controls = map Score.event_transformed_controls
        pitches = map DeriveTest.e_nns

    let derive tracks = DeriveTest.extract id $ DeriveTest.derive_tracks "" $
            ("tempo", [(0, 0, "2")]) : tracks
    strings_like
        (snd $ derive [(">", [(0, 1, "")]), ("*bogus-scale", [(0, 0, ".1")])])
        ["get_scale: unknown \\*bogus-scale"]

    let mkcont vals = Map.union Derive.initial_controls
            (Map.singleton "c1" (Score.untyped (Signal.signal vals)))
        no_pitch = []
    let (events, logs) = derive
            [ ("*", [(0, 0, ".1")])
            , (">", [(0, 1, ""), (1, 1, ""), (2, 1, "")])
            , ("c1", [(0, 0, "3"), (1, 0, "2"), (2, 0, "1")])
            ]
    strings_like logs ["not found: .1"]
    equal (controls events)
        [mkcont [(0, 3)], mkcont [(0.5, 2)], mkcont [(1, 1)]]
    equal (pitches events) [no_pitch, no_pitch, no_pitch]

    let (events, logs) = derive
            [ (">", [(0, 4, ""), (4, 4, ""), (8, 4, "")])
            , ("*", [(0, 0, "4c"), (4, 0, "4d"), (8, 0, "i (4e)")])
            ]
    equal logs []
    -- The pitch signal gets truncated so it doesn't look like the note's decay
    -- wants to change pitch.
    equal (pitches events)
        [ [(0, 60)]
        , [(2, 62), (3, 63)]
        , [(4, 64)]
        ]

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
            . DeriveTest.derive_tracks_setup (DeriveTest.with_skel skel) ""
        extract :: Score.Event -> Maybe Int
        extract = DeriveTest.e_environ_val EnvKey.track_voice
        track inst = (inst, [(0, 1, "")])
    equal (run [] [track ">i1", track ">i2", track ">i1", track ">"])
        ([Just 0, Just 0, Just 1, Nothing], [])
    equal (run [(1, 2), (1, 3)] [(">i1", []), track ">i1", track ">i1"])
        ([Just 1, Just 2], [])
    equal (run [(1, 2), (1, 3)] [("dyn", []), track ">i1", track ">i1"])
        ([Just 0, Just 1], [])
