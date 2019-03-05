-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Load.Midi_test where
import qualified Data.Map as Map

import qualified Util.Seq as Seq
import qualified Cmd.Load.Midi as Midi
import qualified Derive.Controls as Controls
import qualified Midi.Key as Key
import qualified Midi.Midi as M
import qualified Ui.Skeleton as Skeleton

import           Util.Test


test_convert_tracks = do
    let f = Midi.convert_tracks . map (fmap mkmidi)
    let midi_tracks =
            [ ("a", [on Key.c4, on Key.d4, off Key.c4, off Key.d4])
            , ("b", [on Key.e4, cc 42, off Key.e4])
            ]
    let (tracks, skel, warns) = f midi_tracks
    equal (map (fmap Map.toList) tracks)
        [ (">a", [(0, (2, ""))])
        , ("*", [(0, (0, "4c"))])
        , ("dyn", [(0, (0, "`0x`ff"))])
        , (">a", [(1, (2, ""))])
        , ("*", [(1, (0, "4d"))])
        , ("dyn", [(1, (0, "`0x`ff"))])

        , (">b", [(0, (2, ""))])
        , ("*", [(0, (0, "4e"))])
        , ("dyn", [(0, (0, "`0x`ff"))])
        , ("mod", [(1, (0, "`0x`54"))])
        ]
    equal (Skeleton.flatten skel)
        [(1, 2), (2, 3), (4, 5), (5, 6), (7, 8), (8, 9), (9, 10)]
    equal warns []

test_split_track = do
    let f = map extract . fst . Midi.split_track
            . map (fmap (M.ChannelMessage 0))
        extract (Midi.NoteTrack notes pitches controls) =
            (Map.toList notes, Map.toList pitches,
                Map.toList (Map.map Map.toList controls))
        dyn ps = [(Controls.dynamic, [(p, (0, "`0x`ff")) | p <- ps])]
        notes ps = [(p, (d, "")) | (p, d) <- ps]
        pitches ps = [(p, (0, n)) | (p, n) <- ps]
    equal (f [(0, on Key.c4), (1, off Key.c4)])
        [ (notes [(0, 1)], pitches [(0, "4c")], dyn [0])
        ]
    equal (f [(0, on Key.c4), (0, on Key.d4), (1, off Key.c4), (1, off Key.d4)])
        [ (notes [(0, 1)], pitches [(0, "4c")], dyn [0])
        , (notes [(0, 1)], pitches [(0, "4d")], dyn [0])
        ]
    equal (f [(0, on Key.c4), (1, off Key.c4), (1, on Key.d4),
            (2, off Key.d4)])
        [ (notes [(0, 1), (1, 1)], pitches [(0, "4c"), (1, "4d")], dyn [0, 1])
        ]
    equal (f [(0, on Key.c4), (0, on Key.d4), (1, off Key.c4),
            (2, on Key.e4), (3, off Key.e4), (4, off Key.d4)])
        [ (notes [(0, 1), (2, 1)], pitches [(0, "4c"), (2, "4e")], dyn [0, 2])
        , (notes [(0, 4)], pitches [(0, "4d")], dyn [0])
        ]

test_collect_notes = do
    let f = Midi.collect_notes . mkmidi
    equal (f [on Key.c4]) ([], [(0, Key.c4)])
    equal (f [on Key.c4, off Key.c4]) ([(0, 1, Key.c4, 127, [])], [])
    equal (f [on Key.c4, on Key.d4, cc 42, off Key.c4, off Key.d4])
        ( [ (0, 3, Key.c4, 127, [(2, (1, 42))])
          , (1, 4, Key.d4, 127, [(2, (1, 42))])
          ]
        , []
        )
    equal (f [on Key.c4, cc 32, cc 42, off Key.c4])
        ([(0, 3, Key.c4, 127, [(1, (1, 32)), (2, (1, 42))])], [])
    equal (f [cc 42, on Key.c4, off Key.c4])
        ([(1, 2, Key.c4, 127, [])], [])
    equal (f [on Key.c4, off Key.c4, cc 42])
        ([(0, 1, Key.c4, 127, [])], [])

cc :: M.ControlValue -> M.ChannelMessage
cc = M.ControlChange 1

on, off :: M.Key -> M.ChannelMessage
on key = M.NoteOn key 127
off key = M.NoteOff key 127

mkmidi :: [M.ChannelMessage] -> [Midi.Midi]
mkmidi = zip (Seq.range_ 0 1) . map (M.ChannelMessage 0)
