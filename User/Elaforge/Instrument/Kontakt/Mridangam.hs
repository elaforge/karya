-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Mridangam patch.
module User.Elaforge.Instrument.Kontakt.Mridangam where
import qualified Data.Map as Map

import qualified Cmd.Instrument.CUtil as CUtil
import qualified Cmd.Instrument.Drums as Drums
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Cmd.Instrument.Mridangam as Mridangam
import qualified Cmd.Instrument.Mridangam as M

import qualified Derive.Attrs as Attrs
import qualified Midi.Key as Key
import qualified Midi.Key2 as Key2
import qualified Midi.Midi as Midi

import qualified Perform.NN as NN
import qualified User.Elaforge.Instrument.Kontakt.Util as Util

import Global


patches :: [MidiInst.Patch]
patches =
    [ code NN.d4 $ patch "mridangam-d" notes_d
    , code NN.g4 $ patch "mridangam-g" notes_g
    , code NN.g4 $ patch "mridangam-old" pitched_notes_old
    ]
    where
    patch name notes = CUtil.pitched_drum_patch notes $
        MidiInst.named_patch (-24, 24) name []
    code natural_nn = MidiInst.code #= Mridangam.code CUtil.MidiThru natural_nn

notes_d, notes_g :: CUtil.PitchedNotes
(notes_d, _unmapped_notes_d) = make_notes Key.gs3
(notes_g, _unmapped_notes_g) = make_notes Key.d4
    -- The given pitch is the natural pitch of the instrument.  The root note
    -- is the bottom of the pitch range.

make_notes :: Midi.Key
    -> (CUtil.PitchedNotes, ([Drums.Note], [Attrs.Attributes]))
make_notes root_nn = CUtil.drum_pitched_notes Mridangam.all_notes $
    CUtil.make_cc_keymap Key2.c_1 12 root_nn
        [ [M.tha]
        , [M.thom, M.gumki, M.gumki <> Attrs.up, M.thom <> Attrs.dry]
        , [M.ki]
        , [M.ta]
        , [M.nam]
        , [M.din]
        , [M.arai <> M.chapu, M.muru <> M.chapu]
        , [M.kin, M.dheem]
        , [M.tan]
        ]

write_ksp :: IO ()
write_ksp = mapM_ (uncurry Util.write)
    -- Util.drum_mute_ksp ignores the root pitch so I don't need to worry about
    -- 'notes_g'.
    [ ("mridangam.ksp.txt", Util.drum_mute_ksp "mridangam"
        notes_d Mridangam.stops)
    , ("mridangam-old.ksp.txt", Util.drum_mute_ksp "mridangam"
        pitched_notes_old Mridangam.stops)
    ]

pitched_notes_old :: CUtil.PitchedNotes
(pitched_notes_old, _pitched_notes_old) =
    CUtil.drum_pitched_notes Mridangam.all_notes $ Map.fromList $ map make
    -- left
    [ (M.tha, (Key.g_1, Key.e0))
    , (M.thom, (Key.g0, Key.e1))
    , (M.thom <> Attrs.staccato, (Key.g1, Key.e2))
    -- right
    , (M.ki, (Key.g2, Key.e3))
    , (M.ta, (Key.g3, Key.e4))
    , (M.nam, (Key.g4, Key.e5))
    , (M.din, (Key.g5, Key.e6))
    , (M.din <> Attrs.v2, (Key.g6, Key.e7))
    , (M.dheem, (Key.g7, Key.e8))
    , (M.arai <> M.chapu, (Key.g8, Key.e9))
    , (M.muru <> M.chapu, (Key.g9, Key.g9))
    ]
    where make (attrs, (low, high)) = (attrs, ([], low, high, Key.e4))
