-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Local.Instrument.Kontakt.Pakhawaj where
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Midi.Key2 as Key2
import qualified Cmd.Instrument.CUtil as CUtil
import qualified Cmd.Instrument.Drums as Drums
import qualified Cmd.Instrument.MidiInst as MidiInst

import qualified Derive.Instrument.DUtil as DUtil
import qualified Derive.Score as Score
import Derive.Score (attr)
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.NN as NN
import qualified Local.Instrument.KontaktUtil as KontaktUtil
import Global


patches :: [MidiInst.Patch]
patches = [(patch "pakhawaj" pitched_notes, code)]
    where
    -- TODO copy paste from mridangam
    patch name notes = CUtil.pitched_drum_patch notes $
        Instrument.patch $ Instrument.instrument name [] pb_range
    code = MidiInst.note_generators call_code
        <> MidiInst.cmd (CUtil.insert_call char_to_call)
    call_code = concat
        [ CUtil.drum_calls Nothing notes
        , DUtil.multiple_calls [(call, subcalls) | (call, subcalls, _) <- both]
        ]
    char_to_call = Map.fromList $ concat
        [ [(Drums.note_char n, Drums.note_name n) | n <- notes]
        , [(char, call) | (call, _, Just char) <- both]
        ]

pb_range :: Instrument.PbRange
pb_range = (-24, 24)

pitched_notes :: CUtil.PitchedNotes
pitched_notes = make $ CUtil.make_keymap Key2.c_2 Key2.c_1 12 NN.fs3
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
    where
    make :: [(Score.Attributes, CUtil.KeyswitchRange)] -> CUtil.PitchedNotes
    make keymap = do
        note <- notes
        let Just ks_range = lookup (Drums.note_attrs note) keymap
        return (note, ks_range)

-- TODO copy paste from mridangam
-- | Create calls for all simultaneous left and right hand combinations, and
-- key bindings for a few common ones.
both :: [(TrackLang.CallId, [TrackLang.CallId], Maybe Char)]
both = [(call, subcalls, lookup call keys) | (call, subcalls) <- pairs]
    where
    keys = -- TODO different from mridangam
        [ ("k+", 'd')
        , ("uo", 'c') -- dha
        , ("ko", 'v') -- dhet
        ]
    pairs =
        [ (TrackLang.Symbol $ u rcall <> u lcall, [rcall, lcall])
        | lcall <- map Drums.note_name left
        , rcall <- map Drums.note_name right
        , Text.length (u lcall) == 1
        , Text.length (u rcall) == 1
        ]
    u = TrackLang.unsym

write_ksp :: IO ()
write_ksp = mapM_ (uncurry KontaktUtil.write)
    [ ("pakhawaj.ksp.txt", KontaktUtil.drum_mute_ksp "pakhawaj"
        pitched_notes stops)
    ]

-- | The symbols follow the same scheme as mridangam.
left, right :: [Drums.Note]
stops :: [(Drums.Group, [Drums.Group])]
(left, right, stops) = (left, right, stops)
    where
    left = concat $
        [ group t_closed
            [ n 'a' "-" ki 0.5
            , n 'z' "+" ki 1
            ]
        , group t_open
            [ n 'x' "o" ge 1
            , n 's' "." ge 0.5
            ]
        ]
    right = concat $
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

notes :: [Drums.Note]
notes = left ++ right

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
