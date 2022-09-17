-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Midi.Synth_test where
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Midi.Midi as Midi
import           Midi.Midi (ChannelMessage(..))
import qualified Midi.State as State
import qualified Midi.Synth as Synth

import qualified Perform.RealTime as RealTime

import           Global
import           Util.Test


-- These tests overlap with each other.

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
    strings_like (map snd warns)
        [ "sounding notes: [dev:0 e4(64) 0s--"
        , "multiple sounding notes:"
        ]
    equal (f [(100, 0, NoteOn 64 10), (0, 0, NoteOn 68 10)])
        ([ts 0, ts 100], [], [(ts 0, "timestamp less than previous: .1s")])

test_pretty :: Test
test_pretty = do
    let f = map Text.strip . Text.lines . Synth.pretty_state . run . map mkmsg
        chan0 = in_order . map (0,)
    strings_like (f $ in_order
        [ (0, NoteOn 64 10), (0, NoteOn 64 20)
        , (0, NoteOff 64 30), (0, NoteOff 64 40)
        ])
        [ "warns:"
        -- overlapped NoteOn
        , "1s dev:0 NoteOn e4(64) 20: sounding notes: [dev:0 e4(64) 0s--"
        -- ambiguous NoteOff
        , "2s dev:0 NoteOff e4(64) 30: multiple sounding notes: [dev:0 e4(64)\
            \ 1s--"
        -- extra NoteOff, assuming the previous cancelled both
        , "3s dev:0 NoteOff e4(64) 40: no sounding notes"
        , "notes:"
        -- should probably be 0s--2s
        , "dev:0 e4(64) 1s--2s: vel:14"
        ]

    equal (f $ chan0 [PitchBend 0, NoteOn 60 10, PitchBend 0, NoteOn 60 0])
        [ "notes:"
        , "dev:0 c4(60) 1s--3s: vel:0a"
        ]
    equal (f $ chan0
        [PitchBend 1, NoteOn 60 10, PitchBend 0.5, PitchBend 0, NoteOn 60 0])
        [ "notes:"
        , "dev:0 c4(60) 1s--4s: vel:0a p:[(1s, 62nn), (2s, 61nn), (3s, 60nn)]"
        ]

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
    equal (map Synth.note_pitch notes) [65]
    equal (map Synth.note_pitches notes) [[(ts 100, 66), (ts 200, 62)]]
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

in_order :: [(Midi.Channel, ChannelMessage)]
    -> [(Integer, Midi.Channel, ChannelMessage)]
in_order msgs =
    [ (ts * 1000, chan, msg)
    | (ts, (chan, msg)) <- zip [0..] msgs
    ]
