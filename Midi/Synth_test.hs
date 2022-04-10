-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Midi.Synth_test where
import qualified Data.Map as Map

import Util.Test
import qualified Midi.Key as Key
import qualified Midi.Midi as Midi
import Midi.Midi (ChannelMessage(..))
import qualified Midi.State as State
import qualified Midi.Synth as Synth

import qualified Perform.NN as NN
import qualified Perform.RealTime as RealTime
import Global


test_run :: Test
test_run = do
    let f = extract id id . run . map mkmsg
    let (active, notes, ws) =
            f [(0, 0, NoteOn Key.c4 38), (1000, 0, NoteOff Key.c4 38)]
    equal active []
    equal notes
        [ Synth.Note (ts 0) (ts 1000) Key.c4 38 NN.c4 [] Map.empty
            (Midi.write_device "dev", 0)
        ]
    equal ws []

    let state = run $ map mkmsg
            [ (0, 0, NoteOn Key.c4 38), (10, 0, NoteOn Key.c4 38)
            , (20, 0, ControlChange 1 100)
            , (20, 0, ControlChange 2 100)
            , (30, 0, ControlChange 2 50)
            ]
    match (untxt (Synth.pretty_state state))
        "active:*dev:0 60nn*warns:*sounding notes: *dev:0 60nn*"

test_control :: Test
test_control = do
    let f = extract controls controls . run . map mkmsg
        controls = Map.assocs . Synth.note_controls
    let (active, notes, ws) = f
            [ (0, 0, NoteOn 64 38)
            , (100, 0, ControlChange 2 100)
            , (200, 0, ControlChange 2 50)
            , (1000, 0, NoteOff 64 38)
            ]
    equal active []
    equal notes
        [ [(State.CC 2, [(ts 100, 100), (ts 200, 50)])]
        ]
    equal ws []

    let (active, notes, ws) = f
            [ (0, 0, NoteOn 64 38)
            , (100, 0, ControlChange 2 100)
            , (100, 0, NoteOn 70 100)
            , (200, 0, ControlChange 2 50)
            , (1000, 0, NoteOff 64 38)
            ]
    equal active [[(State.CC 2, [(ts 100, 100), (ts 200, 50)])]]
    equal notes [[(State.CC 2, [(ts 100, 100), (ts 200, 50)])]]
    equal ws []

test_warns :: Test
test_warns = do
    let f = extract Synth.note_start Synth.note_start . run . map mkmsg
    let (active, notes, warns) = f
            [ (0, 0, NoteOn 64 38)
            , (100, 0, NoteOn 64 100)
            , (200, 0, NoteOff 64 0)
            ]
    equal active []
    equal notes [ts 100] -- one one cancelled
    equal (map fst warns) [ts 100, ts 200]
    strings_like (map (untxt . snd) warns)
        ["sounding notes: [dev:0 64nn", "multiple sounding notes:"]

    equal (f [(100, 0, NoteOn 64 10), (0, 0, NoteOn 68 10)])
        ([ts 0, ts 100], [], [(ts 0, "timestamp less than previous: .1s")])

test_pitch :: Test
test_pitch = do
    let f = extract Synth.note_pitch id . run . map mkmsg
    let (active, notes, warns) = f
            [ (0, 0, PitchBend 0.5)
            , (0, 0, NoteOn 64 38)
            , (100, 0, PitchBend 1)
            , (200, 0, PitchBend (-1))
            , (1000, 0, NoteOff 64 38)
            ]
    equal active []
    equal (map Synth.note_pitch notes) [64.5]
    equal (map Synth.note_pitches notes) [[(ts 100, 65), (ts 200, 63)]]
    equal warns []

run :: [Midi.WriteMessage] -> Synth.State
run = Synth.run Synth.empty_state

ts :: Integer -> RealTime.RealTime
ts = RealTime.milliseconds

extract :: (Synth.SoundingNote -> a) -> (Synth.Note -> b) -> Synth.State
    -> ([a], [b], [(RealTime.RealTime, Text)])
extract active done state =
    ( map active (concat (Map.elems (Synth.state_active state)))
    , map done (Synth.state_notes state)
    , [(Midi.wmsg_ts wmsg, msg) | (wmsg, msg) <- Synth.state_warns state]
    )

mkmsg :: (Integer, Midi.Channel, ChannelMessage) -> Midi.WriteMessage
mkmsg (ts, chan, msg) = Midi.WriteMessage
    (Midi.write_device "dev") (RealTime.milliseconds ts)
    (Midi.ChannelMessage chan msg)
