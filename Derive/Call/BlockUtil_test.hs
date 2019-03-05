-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.BlockUtil_test where
import qualified Data.Map as Map

import qualified Derive.Args as Args
import qualified Derive.Call as Call
import qualified Derive.Call.CallTest as CallTest
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.EnvKey as EnvKey
import qualified Derive.PSignal as PSignal
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT

import qualified Perform.Signal as Signal
import qualified Ui.UiTest as UiTest

import           Util.Test


test_compile = do
    let controls = map Score.event_controls
        pitches = map DeriveTest.e_nns_old

    let derive tracks = DeriveTest.extract id $ DeriveTest.derive_tracks "" $
            ("tempo", [(0, 0, "2")]) : tracks
    strings_like
        (snd $ derive [(">", [(0, 1, "")]), ("*bogus-scale", [(0, 0, ".1")])])
        ["*unknown scale: bogus-scale"]

    let mkcont vals = Map.union Derive.initial_controls
            (Map.singleton "c1" (ScoreT.untyped (Signal.from_pairs vals)))
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
    -- TODO well, not any more.  Since segments are continuous now, it's up to
    -- the performer to clip at the end time.
    equal (pitches events)
        [ [(0, 60)]
        , [(2, 62), (4, 64)]
        , [(4, 64)]
        ]

test_pitch_map_note = do
    let run next = DeriveTest.extract DeriveTest.e_start_note
            .  DeriveTest.derive_tracks_setup
                (CallTest.with_note_generator "g" (note_gen next)) ""
    let pitches = ("*", [(0, 0, "4c"), (1, 0, "4d"), (2, 0, "4e")])
        notes = (">", [(0, 1, ""), (1, 1, "g"), (2, 1, "")])
    equal (run True [notes, pitches]) ([(0, "4c"), (1, "4e"), (2, "4e")], [])
    equal (run False [notes, pitches]) ([(0, "4c"), (1, "4c"), (2, "4e")], [])
    strings_like (snd $ run False [(">", [(1, 1, "g"), (2, 1, "")]), pitches])
        ["no prev/next"]
    -- Also works if the pitch track is above.
    equal (run True [pitches, notes]) ([(0, "4c"), (1, "4e"), (2, "4e")], [])
    equal (run False [pitches, notes]) ([(0, "4c"), (1, "4c"), (2, "4e")], [])
    where
    note_gen :: Bool -> Derive.Generator Derive.Note
    note_gen next = CallTest.generator $ \args -> do
        pitch <- Derive.require "no prev/next" =<< if next
            then Args.lookup_next_pitch args
            else Args.lookup_prev_pitch args
        Call.place args $ Call.pitched_note pitch

test_pitch_map_pitch = do
    let run next = DeriveTest.extract DeriveTest.e_start_note
            .  DeriveTest.derive_tracks_setup
                (CallTest.with_pitch_generator "g" (pitch_gen next)) ""
            . UiTest.note_track
    let notes = [(0, 1, "4c"), (1, 1, "g"), (2, 1, "4e")]
    equal (run True notes) ([(0, "4c"), (1, "4e"), (2, "4e")], [])
    equal (run False notes) ([(0, "4c"), (1, "4c"), (2, "4e")], [])
    where
    pitch_gen :: Bool -> Derive.Generator Derive.Pitch
    pitch_gen next = CallTest.generator1 $ \args -> do
        pitch <- if next
            then Args.lookup_next_pitch args
            else Args.lookup_prev_pitch args
        start <- Args.real_start args
        return $ PSignal.from_pairs $ maybe [] (\p -> [(start, p)]) pitch

test_control_call = do
    let run tracks = DeriveTest.extract extract $ DeriveTest.derive_blocks
            [ ("top", tracks)
            , ("c=ruler", [("%", [(0, 0, "1"), (1, 0, ".5"), (2, 0, "--|")])])
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
