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


test_compile :: Test
test_compile = do
    let controls = map (Map.lookup "c1" . Score.eventControls)
        pitches = map DeriveTest.eNns

    let derive tracks = DeriveTest.extract id $ DeriveTest.deriveTracks "" $
            ("tempo", [(0, 0, "2")]) : tracks
    stringsLike
        (snd $ derive [(">", [(0, 1, "")]), ("*bogus-scale", [(0, 0, ".1")])])
        ["*unknown scale: bogus-scale"]

    let mkcont = ScoreT.untyped . Signal.fromPairs
        noPitch = []
    let (events, logs) = derive
            [ ("*", [(0, 0, ".1")])
            , (">", [(0, 1, ""), (1, 1, ""), (2, 1, "")])
            , ("c1", [(0, 0, "3"), (1, 0, "2"), (2, 0, "1")])
            ]
    stringsLike logs ["not found: .1"]
    equal (controls events) $
        map Just [mkcont [(0, 3)], mkcont [(0.5, 2)], mkcont [(1, 1)]]
    equal (pitches events) [noPitch, noPitch, noPitch]

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
        [ [(0, 60), (2, 60)]
        , [(2, 62), (4, 64)]
        , [(4, 64)]
        ]

test_pitch_map_note :: Test
test_pitch_map_note = do
    let run next = DeriveTest.extract DeriveTest.eStartNote
            .  DeriveTest.deriveTracksSetup
                (CallTest.withNoteGenerator "g" (noteGen next)) ""
    let pitches = ("*", [(0, 0, "4c"), (1, 0, "4d"), (2, 0, "4e")])
        notes = (">", [(0, 1, ""), (1, 1, "g"), (2, 1, "")])
    equal (run True [notes, pitches]) ([(0, "4c"), (1, "4e"), (2, "4e")], [])
    equal (run False [notes, pitches]) ([(0, "4c"), (1, "4c"), (2, "4e")], [])
    stringsLike (snd $ run False [(">", [(1, 1, "g"), (2, 1, "")]), pitches])
        ["no prev/next"]
    -- Also works if the pitch track is above.
    equal (run True [pitches, notes]) ([(0, "4c"), (1, "4e"), (2, "4e")], [])
    equal (run False [pitches, notes]) ([(0, "4c"), (1, "4c"), (2, "4e")], [])
    where
    noteGen :: Bool -> Derive.Generator Derive.Note
    noteGen next = CallTest.generator $ \args -> do
        pitch <- Derive.require "no prev/next" =<< if next
            then Args.lookupNextPitch args
            else Args.lookupPrevPitch args
        Call.place args $ Call.pitchedNote pitch

test_pitch_map_pitch :: Test
test_pitch_map_pitch = do
    let run next = DeriveTest.extract DeriveTest.eStartNote
            .  DeriveTest.deriveTracksSetup
                (CallTest.withPitchGenerator "g" (pitchGen next)) ""
            . UiTest.noteTrack
    let notes = [(0, 1, "4c"), (1, 1, "g"), (2, 1, "4e")]
    equal (run True notes) ([(0, "4c"), (1, "4e"), (2, "4e")], [])
    equal (run False notes) ([(0, "4c"), (1, "4c"), (2, "4e")], [])
    where
    pitchGen :: Bool -> Derive.Generator Derive.Pitch
    pitchGen next = CallTest.generator1 $ \args -> do
        pitch <- if next
            then Args.lookupNextPitch args
            else Args.lookupPrevPitch args
        start <- Args.realStart args
        pure $ PSignal.fromPairs $ maybe [] (\p -> [(start, p)]) pitch

test_control_call :: Test
test_control_call = do
    let run tracks = DeriveTest.extract extract $ DeriveTest.deriveBlocks
            [ ("top", tracks)
            , ("c=ruler", [("%", [(0, 0, "1"), (1, 0, ".5"), (2, 0, "--|")])])
            ]
        extract e = (Score.eventStart e, Score.initialDynamic e)
        dynCall = ("dyn", [(0, 2, "c"), (2, 2, "c")])
    equal (run $ dynCall : UiTest.regularNotes 4)
        ([(0, 1), (1, 0.5), (2, 1), (3, 0.5)], [])
    equal (run $ UiTest.regularNotes 4 ++ [dynCall])
        ([(0, 1), (1, 0.5), (2, 1), (3, 0.5)], [])

test_track_voice :: Test
test_track_voice = do
    let run skel = DeriveTest.extract extract
            . DeriveTest.deriveTracksSetup (DeriveTest.withSkel skel) ""
        extract :: Score.Event -> Maybe Int
        extract = DeriveTest.eEnvironVal EnvKey.trackVoice
        track inst = (inst, [(0, 1, "")])
    equal (run [] [track ">i1", track ">i2", track ">i1", track ">"])
        ([Just 0, Just 0, Just 1, Nothing], [])
    equal (run [(1, 2), (1, 3)] [(">i1", []), track ">i1", track ">i1"])
        ([Just 1, Just 2], [])
    equal (run [(1, 2), (1, 3)] [("dyn", []), track ">i1", track ">i1"])
        ([Just 0, Just 1], [])
