-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.C.Prelude.NoteTransformer_test where
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Derive.TrackWarp as TrackWarp

import Global
import Types


test_sequence :: Test
test_sequence = do
    let run start dur call = runBlocks $ makeSubs start dur call
    equal (run 0 2 "sequence sub-cd") ([(0, 1, "4c"), (1, 1, "4d")], [])
    equal (run 0 1 "sequence sub-cd") ([(0, 0.5, "4c"), (0.5, 0.5, "4d")], [])
    equal (run 0 3 "sequence sub-cd sub-e")
        ([(0, 1, "4c"), (1, 1, "4d"), (2, 1, "4e")], [])
    equal (run 0 3 "sequence \"(t-dia=1 | sub-e) sub-cd")
        ([(0, 1, "4f"), (1, 1, "4c"), (2, 1, "4d")], [])
    -- Duration is the sum of callees, so I can sequence sequences.
    equal (run 0 4 "sequence \"(sequence sub-e sub-e) sub-cd")
        ([(0, 1, "4e"), (1, 1, "4e"), (2, 1, "4c"), (3, 1, "4d")], [])

    -- Notes stretch with event duration.
    equal (run 2 1 "sequence sub-cd") ([(2, 0.5, "4c"), (2.5, 0.5, "4d")], [])
    equal (run 2 6 "sequence sub-cd sub-e")
        ([(2, 2, "4c"), (4, 2, "4d"), (6, 2, "4e")], [])

test_sequence_realtime :: Test
test_sequence_realtime = do
    let run = runBlocks
    equal (run $ makeSubs 0 2 "sequence-rt sub-cd")
        ([(0, 1, "4c"), (1, 1, "4d")], [])
    equal (run $ makeSubs 0 3 "sequence-rt sub-cd sub-e")
        ([(0, 1, "4c"), (1, 1, "4d"), (2, 1, "4e")], [])
    let tempoSubs start dur call =
            [ ("top", [(">", [(start, dur, call)])])
            , ("sub-cd=ruler", ("tempo", [(0, 0, ".5")])
                : UiTest.noteTrack [(0, 1, "4c"), (1, 1, "4d")])
            , ("sub-e=ruler", UiTest.noteTrack [(0, 1, "4e")])
            ]
    equal (run $ tempoSubs 0 5 "sequence-rt sub-cd sub-e")
        ([(0, 2, "4c"), (2, 2, "4d"), (4, 1, "4e")], [])
    -- Translate and stretch.
    equal (run $ tempoSubs 2 5 "sequence-rt sub-cd sub-e")
        ([(2, 2, "4c"), (4, 2, "4d"), (6, 1, "4e")], [])
    equal (run $ tempoSubs 2 10 "sequence-rt sub-cd sub-e")
        ([(2, 4, "4c"), (6, 4, "4d"), (10, 2, "4e")], [])
    equal (run $ tempoSubs 2 2.5 "sequence-rt sub-cd sub-e")
        ([(2, 1, "4c"), (3, 1, "4d"), (4, 0.5, "4e")], [])

    equal (run $ tempoSubs 2 6 "sequence \"(sequence-rt sub-cd sub-e) sub-e")
        ([(2, 2, "4c"), (4, 2, "4d"), (6, 1, "4e"), (7, 1, "4e")], [])

test_parallel :: Test
test_parallel = do
    let run start dur call = runBlocks $ makeSubs start dur call
    equal (run 0 2 "parallel sub-cd") ([(0, 1, "4c"), (1, 1, "4d")], [])
    equal (run 0 2 "parallel sub-cd sub-e")
        ([(0, 1, "4c"), (0, 1, "4e"), (1, 1, "4d")], [])
    equal (run 0 4 "parallel sub-cd sub-e")
        ([(0, 2, "4c"), (0, 2, "4e"), (2, 2, "4d")], [])

runBlocks :: [UiTest.BlockSpec] -> ([(RealTime, RealTime, Text)], [Text])
runBlocks = DeriveTest.extract DeriveTest.eNote . DeriveTest.deriveBlocks

makeSubs :: ScoreTime -> ScoreTime -> Text -> [UiTest.BlockSpec]
makeSubs start dur call =
    [ ("top", [(">", [(start, dur, call)])])
    , ("sub-cd=ruler", UiTest.noteTrack [(0, 1, "4c"), (1, 1, "4d")])
    , ("sub-e=ruler", UiTest.noteTrack [(0, 1, "4e")])
    ]

test_multiple :: Test
test_multiple = do
    let run = DeriveTest.extract extract . DeriveTest.deriveTracks ""
        extract e = (Score.eventStart e, DeriveTest.eInstrument e)
    equal (run [("> | multiple \"(inst=i1) \"(inst=i2)", [(0, 1, "")])])
        ([(0, "i1"), (0, "i2")], [])

test_track_warps :: Test
test_track_warps = do
    let run notes = extract $ DeriveTest.deriveBlocks
            [ ("top", [(">", notes)])
            , ("sub=ruler", UiTest.regularNotes 4)
            ]
        extract = map eTrackWarp . Derive.rTrackWarps
        eTrackWarp tw =
            (TrackWarp.twBlock tw, TrackWarp.twStart tw, TrackWarp.twEnd tw)
        top = UiTest.bid "top"
        sub = UiTest.bid "sub"

    equal (run [(0, 4, "sub")]) [(sub, 0, 4), (top, 0, 4)]
    equal (run [(0, 2, "clip | sub")]) [(sub, 0, 2), (top, 0, 2)]
    equal (run [(1, 2, "clip | sub")]) [(sub, 1, 3), (top, 0, 3)]
    equal (run [(0, 2, "clip | sub"), (4, 2, "clip | sub")])
        [(sub, 0, 2), (sub, 4, 6), (top, 0, 6)]

    equal (run [(0, 2, "Clip | sub")]) [(sub, 0, 2), (top, 0, 2)]
    equal (run [(1, 2, "Clip | sub")]) [(sub, 1, 3), (top, 0, 3)]

    equal (run [(0, 7, "loop | sub")]) [(sub, 0, 4), (sub, 4, 7), (top, 0, 7)]
    equal (run [(2, 4, "tile | sub")]) [(sub, 2, 4), (sub, 4, 6), (top, 0, 6)]
    equal (run [(2, 4, "repeat 2 | sub")])
        [(sub, 2, 4), (sub, 4, 6), (top, 0, 6)]

test_clip :: Test
test_clip = do
    let run top = runSub DeriveTest.eStartDur [(">", top)]
            [(">", [(0, 1, ""), (1, 1, "")])]
    -- make sure out of range notes are clipped
    equal (run [(0, 1, "clip | sub")]) ([(0, 1)], [])

    -- sub goes *4/2 + 1 ==> [(1, 2), (3, 2)]
    -- I want            ==> [(1, 1), (2, 1)]
    -- so to put in 1:1 time, (-1) (*0.5)
    equal (run [(1, 4, "clip | sub")]) ([(1, 1), (2, 1)], [])

    -- sub goes *1.5/2 + 1 ==> [(1, 0.75), (1.75, 0.75)]
    -- I want              ==> [(1, 1), (2, 1)]
    -- so (-1) (* 1/.75)
    -- notes that overlap the end are shortened
    equal (run [(1, 1.5, "clip | sub")]) ([(1, 1), (2, 0.5)], [])

    -- A note at the end is clipped.
    equal (runSub DeriveTest.eStartDur
            [(">", [(0, 1, "clip | sub")])] [(">", [(0, 0, ""), (1, 0, "")])])
        ([(0, 0)], [])

    -- clip works even when it's not directly a block call.
    equal (run [(1, 1.5, "^b=sub | clip | b")]) ([(1, 1), (2, 0.5)], [])

    let run2 = runSub DeriveTest.eNote [(">", [(0, 2, "clip | sub")])]
    equal (run2 (UiTest.regularNotes 4)) ([(0, 1, "3c"), (1, 1, "3d")], [])
    equal (run2 (UiTest.negativeRegularNotes 4))
        ([(1, -1, "3c"), (2, -1, "3d")], [])

test_clip_start :: Test
test_clip_start = do
    let run top = runSub DeriveTest.eNote [(">", top)]
    -- Aligned to the end.
    equal (run [(0, 2, "Clip | sub")] (UiTest.regularNotes 1))
        ([(1, 1, "3c")], [])
    -- Get the last two notes.
    equal (run [(0, 2, "Clip | sub")] (UiTest.regularNotes 3))
        ([(0, 1, "3d"), (1, 1, "3e")], [])
    -- If events are negative, omit ones at the start.
    --     Clip--->
    -- <-3c<-3d<-3e
    equal (run [(0, 2, "Clip | sub")] (UiTest.negativeRegularNotes 3))
        ([(1, -1, "3d"), (2, -1, "3e")], [])

test_loop :: Test
test_loop = do
    let run = runSub DeriveTest.eStartDur
    equal (run [(">", [(0, 4, "loop | sub")])] [(">", [(0, 1, "")])])
        ([(0, 1), (1, 1), (2, 1), (3, 1)], [])
    -- Cuts off the last event.
    let sub = [(">", [(0, 1, ""), (1, 3, "")])]
    equal (run [(">", [(0, 5, "loop | sub")])] sub)
        ([(0, 1), (1, 3), (4, 1)], [])

test_tile :: Test
test_tile = do
    let run top = runSub Score.eventStart [(">", top)]
            [(">", [(0, 1, ""), (1, 3, "")])]
    -- If it starts at 0, it's just like 'loop'.
    equal (run [(0, 5, "tile | sub")]) ([0, 1, 4], [])
    equal (run [(1, 5, "tile | sub")]) ([1, 4, 5], [])
    equal (run [(9, 5, "tile | sub")]) ([9, 12, 13], [])

test_repeat :: Test
test_repeat = do
    let run top = runSub DeriveTest.eNote [(">", top)]
            (UiTest.noteTrack [(0, 1, "4c"), (1, 1, "4d")])
    equal (run [(0, 4, "repeat 2 | sub")])
        ([(0, 1, "4c"), (1, 1, "4d"), (2, 1, "4c"), (3, 1, "4d")], [])
    equal (run [(0, 6, "repeat 3 | sub")])
        ([ (0, 1, "4c"), (1, 1, "4d"), (2, 1, "4c"), (3, 1, "4d")
         , (4, 1, "4c"), (5, 1, "4d")
         ], [])
    equal (run [(2, 4, "repeat 2 | sub")])
        ([(2, 1, "4c"), (3, 1, "4d"), (4, 1, "4c"), (5, 1, "4d")], [])

runSub :: (Score.Event -> a) -> [UiTest.TrackSpec] -> [UiTest.TrackSpec]
    -> ([a], [Text])
runSub extract top sub = DeriveTest.extract extract $ DeriveTest.deriveBlocks
    [("top", top), ("sub=ruler", sub)]
