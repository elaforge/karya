-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Pakhawaj patch.  The notation is based on mridangam notation.
module Local.Instrument.Kontakt.Pakhawaj where
import qualified Midi.Key2 as Key2
import qualified Cmd.Instrument.CUtil as CUtil
import qualified Cmd.Instrument.Drums as Drums
import qualified Cmd.Instrument.MidiInst as MidiInst

import qualified Derive.Attrs as Attrs
import qualified Derive.Call.India.Pakhawaj as Pakhawaj
import Derive.Call.India.Pakhawaj (Stroke(..))
import qualified Derive.Expr as Expr

import qualified Perform.NN as NN
import qualified Local.Instrument.Kontakt.Mridangam as Mridangam
import qualified Local.Instrument.Kontakt.Util as Util
import Global


patches :: [MidiInst.Patch]
patches = [MidiInst.code #= code $ patch "pakhawaj" pitched_notes]
    where
    patch name notes = CUtil.pitched_drum_patch notes $
        MidiInst.named_patch (-24, 24) name []
    code = Mridangam.make_code pitched_strokes NN.c4 all_notes both_calls

pitched_notes :: CUtil.PitchedNotes
(pitched_notes, _pitched_notes) = CUtil.drum_pitched_notes all_notes $
    CUtil.make_cc_keymap Key2.c_1 12 Key2.fs2
        [ [attr Ka]
        , [attr Ge]
        , [attr Tet]
        , [attr Te]
        , [attr Ne]
        , [attr Na, nam]
        , [din]
        , [attr Ta]
        , [attr Di]
        ]
    where attr = Pakhawaj.stroke_to_attribute

-- | Create calls for all simultaneous left and right hand combinations, and
-- key bindings for a few common ones.
both_calls :: [(Expr.Symbol, [Expr.Symbol], Maybe Char)]
both_calls = Mridangam.make_both left_notes right_notes special_names
    [ ("D", 'c')
    , ("T", 'f')
    , ("E", 'v')
    ]
    where
    special_names =
        [ ("D", ["o", "u"]) -- dha
        , ("E", ["o", "k"]) -- dhet
        , ("T", ["+", "k"]) -- thet
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
    left_notes = concat
        [ group l_closed
            [ n 'a' "-" (attr Ka) 0.5
            , n 'z' "+" (attr Ka) 1
            ]
        , group l_open
            [ n 's' "." (attr Ge) 0.5
            , n 'x' "o" (attr Ge) 1
            ]
        ]
    right_notes = concat
        [ group r_closed
            [ n '1' "l" (attr Tet) 0.5
            , n 'q' "k" (attr Tet) 1
            , n '2' "p" (attr Ne)  0.75
            , n 'w' "t" (attr Te)  1
            ]
        , group r_syahi
            [ n '3' "m" nam 1
            , n 'e' "n" (attr Na) 1
            , n 'r' "d" din 1
            ]
        , group r_syahi_open
            [ n 't' "u" (attr Ta) 1
            ]
        , group r_dheem [n 'y' "i" (attr Di) 1]
        ]

    stops =
        [ (l_closed, [l_open])
        , (r_closed, [r_syahi, r_syahi_open, r_dheem])
        , (r_syahi, [r_syahi_open, r_dheem])
        , (r_syahi_open, [r_dheem])
        ]
    r_closed = "r-closed"
    r_syahi = "r-syahi"
    r_syahi_open = "r-syahi_open"
    r_dheem = "r-dheem"
    l_closed = "l-closed"
    l_open = "l-open"
    group name = map $ \n -> n { Drums.note_group = name }
    n = Drums.note_dyn
    attr = Pakhawaj.stroke_to_attribute

pitched_strokes :: [Attrs.Attributes]
pitched_strokes = map Pakhawaj.stroke_to_attribute [Ge, Na, Ta, Di]

all_notes :: [Drums.Note]
all_notes = left_notes ++ right_notes

-- mridangam strokes
din = Attrs.attr "din"
nam = Attrs.attr "nam"
