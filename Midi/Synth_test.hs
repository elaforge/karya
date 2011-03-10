module Midi.Synth_test where
import qualified Data.Map as Map
import Util.Test

import qualified Midi.Midi as Midi
import Midi.Midi (ChannelMessage(..))
import qualified Midi.Synth as Synth

import qualified Perform.RealTime as RealTime


test_run = do
    let f = extract id . run . map mkmsg
    let (active, notes, ws) = f [(0, 0, NoteOn 64 38), (1000, 0, NoteOff 64 38)]
    equal active []
    equal notes
        [ Synth.Note (ts 0) (Just (ts 1000)) 64 38
            [(ts 0, 64)] Map.empty (Midi.WriteDevice "dev", 0)
        ]
    equal ws []

    let state = run $ map mkmsg
            [ (0, 0, NoteOn 64 38), (10, 0, NoteOn 64 38)
            , (20, 0, ControlChange 1 100)
            , (20, 0, ControlChange 2 100)
            , (30, 0, ControlChange 2 50)
            ]
    putStrLn $ Synth.pretty_state state

test_control = do
    let f = extract (Map.assocs . Synth.note_controls) . run . map mkmsg
    let (active, notes, ws) = f
            [ (0, 0, NoteOn 64 38)
            , (100, 0, ControlChange 2 100)
            , (200, 0, ControlChange 2 50)
            , (1000, 0, NoteOff 64 38)
            ]
    equal active []
    equal notes
        [ [(Synth.CC 2, [(ts 100, 100), (ts 200, 50)])]
        ]
    equal ws []

    let (active, notes, ws) = f
            [ (0, 0, NoteOn 64 38)
            , (100, 0, ControlChange 2 100)
            , (100, 0, NoteOn 70 100)
            , (200, 0, ControlChange 2 50)
            , (1000, 0, NoteOff 64 38)
            ]
    equal active [[(Synth.CC 2, [(ts 200, 50)])]]
    equal notes [[(Synth.CC 2, [(ts 100, 100), (ts 200, 50)])]]
    equal ws []

test_warns = do
    let f = extract Synth.note_start . run . map mkmsg
    let (active, notes, ws) = f
            [ (0, 0, NoteOn 64 38)
            , (100, 1, ControlChange 2 100)
            , (200, 2, PitchBend 1)
            , (1000, 0, NoteOff 64 38)
            ]
    equal active []
    equal notes [ts 0]
    equal ws
        [(ts 100, "CC 2 without note"), (ts 200, "pitch bend without note")]
    -- pprint $ (extract id . run . map mkmsg)
    equal (f
            [ (0, 0, NoteOn 64 38)
            , (100, 0, NoteOn 64 100)
            , (200, 0, NoteOff 64 0)
            ])
        ([], [ts 0, ts 100], [(ts 100, "double note on")])

    equal (f [(100, 0, NoteOn 64 10), (0, 0, NoteOn 68 10)])
        ([ts 0, ts 100], [], [(ts 0, "timestamp less than previous: .1s")])

test_pitch = do
    let f = extract Synth.note_pitch . run . map mkmsg
    equal (f
        [ (0, 0, NoteOn 64 38)
        , (100, 0, PitchBend 1)
        , (200, 0, PitchBend (-1))
        , (1000, 0, NoteOff 64 38)
        ])
        ([], [[(ts 0, 64), (ts 100, 63), (ts 200, 65)]], [])

run = Synth.run Synth.empty_state

ts = RealTime.milliseconds

extract :: (Synth.Note -> a) -> Synth.State
    -> ([a], [a], [(RealTime.RealTime, String)])
extract extract_note state =
    ( map extract_note (concat (Map.elems (Synth.state_active state)))
    , map extract_note (Synth.state_notes state)
    , [(Midi.wmsg_ts wmsg, msg) | (wmsg, msg) <- Synth.state_warns state]
    )

mkmsg (ts, chan, msg) = Midi.WriteMessage
    (Midi.WriteDevice "dev")
    (RealTime.milliseconds ts)
    (Midi.ChannelMessage chan msg)
