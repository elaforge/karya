-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Pakhawaj patch.  The notation is based on mridangam notation.
module Local.Instrument.Kontakt.Pakhawaj where
import qualified Midi.Key2 as Key2
import qualified Cmd.Instrument.CUtil as CUtil
import qualified Cmd.Instrument.Drums as Drums
import qualified Cmd.Instrument.MidiInst as MidiInst

import Derive.Score (attr)
import qualified Derive.TrackLang as TrackLang
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.NN as NN
import qualified Local.Instrument.Kontakt.Mridangam as Mridangam
import qualified Local.Instrument.Kontakt.Util as Util


patches :: [MidiInst.Patch]
patches = [(patch "pakhawaj" pitched_notes, code)]
    where
    patch name notes = CUtil.pitched_drum_patch notes $
        Instrument.patch $ Instrument.instrument name [] Mridangam.pb_range
    code = Mridangam.make_code all_notes both_calls

pitched_notes :: CUtil.PitchedNotes
(pitched_notes, _pitched_notes) = CUtil.drum_pitched_notes all_notes $
    CUtil.make_keymap Key2.c_2 Key2.c_1 12 NN.fs3
        [ [ki]
        , [ge]
        , [tet]
        , [te]
        , [ne]
        , [na, nam]
        , [din]
        , [ta]
        , [di]
        ]

-- | Create calls for all simultaneous left and right hand combinations, and
-- key bindings for a few common ones.
both_calls :: [(TrackLang.CallId, [TrackLang.CallId], Maybe Char)]
both_calls = Mridangam.make_both left_notes right_notes special_names
    [ ("D", 'c')
    , ("E", 'v')
    ]
    where
    special_names =
        [ ("D", ["o", "u"]) -- dha
        , ("E", ["o", "k"]) -- dhet
        ]

write_ksp :: IO ()
write_ksp = mapM_ (uncurry Util.write)
    [ ("pakhawaj.ksp.txt", Util.drum_mute_ksp "pakhawaj" pitched_notes stops)
    ]

-- | The symbols follow the same scheme as mridangam.
left_notes, right_notes :: [Drums.Note]
stops :: [(Drums.Group, [Drums.Group])]
(left_notes, right_notes, stops) = (left_notes, right_notes, stops)
    where
    left_notes = concat $
        [ group t_closed
            [ n 'a' "-" ki 0.5
            , n 'z' "+" ki 1
            ]
        , group t_open
            [ n 's' "." ge 0.5
            , n 'x' "o" ge 1
            ]
        ]
    right_notes = concat $
        [ group v_closed
            [ n '1' "l" tet 0.5
            , n 'q' "k" tet 1
            , n '2' "p" ne 0.75
            , n 'w' "t" te 1
            ]
        , group v_sadam
            [ n '3' "m" nam 1
            , n 'e' "n" na 1
            , n 'r' "d" din 1
            ]
        , group v_chapu
            [ n 't' "u" ta 1
            ]
        , group v_dheem [n 'y' "i" di 1]
        ]

    stops =
        [ (t_closed, [t_open])
        , (v_closed, [v_sadam, v_chapu, v_dheem])
        , (v_sadam, [v_chapu, v_dheem])
        , (v_chapu, [v_dheem])
        ]
    v_closed = "v-closed"
    v_sadam = "v-sadam"
    v_chapu = "v-chapu"
    v_dheem = "v-dheem"
    t_closed = "t-closed"
    t_open = "t-open"
    group name = map $ \n -> n { Drums.note_group = name }
    n = Drums.note_dyn

all_notes :: [Drums.Note]
all_notes = left_notes ++ right_notes

-- right
di = attr "di"
din = attr "din"
na = attr "na" -- like mridangam nam, but no muting with the finger
nam = attr "nam"
ne = attr "ne" -- closed stroke with pinky and ring finger
ta = attr "ta"
tet = attr "tet"
te = attr "te"

-- left
ki = attr "ki"
ge = attr "ge"
