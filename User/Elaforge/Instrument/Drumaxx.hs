-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Image-Line's Drumaxx softsynth.
module User.Elaforge.Instrument.Drumaxx where
import qualified Data.Set as Set

import Midi.Key
import qualified Midi.Midi as Midi
import qualified Cmd.Instrument.CUtil as CUtil
import qualified Cmd.Instrument.Drums as Drums
import qualified Cmd.Instrument.MidiInst as MidiInst

import Derive.Attrs
import qualified Derive.Instrument.DUtil as DUtil
import Global


synth :: MidiInst.Synth
synth = MidiInst.synth "drumaxx" "Imagine-Line Drumaxx" patches

patches :: [MidiInst.Patch]
patches =
    [ MidiInst.code #= CUtil.drum_code_ CUtil.MidiThru (map fst stroke_keys) $
        CUtil.drum_patch stroke_keys $ MidiInst.default_patch pb_range []
    , MidiInst.code #= composite_code $
        MidiInst.doc #= composite_doc $
        MidiInst.named_patch pb_range "comb" []
    ]
    where
    composite_code = MidiInst.note_generators
        [(call, composite call) | call <- map (Drums._name . fst) stroke_keys]
    composite call = DUtil.redirect_pitch "comb" ""
        (Just (Set.fromList ["mix", "fbk"])) call Nothing
    composite_doc = "This drum takes a pitch signal, which is then sent\
        \ to the `composite-pitched` instrument, which is a tuned comb filter.\
        \ The audio routing has to be set up in the VST host."
    pb_range = (-24, 24)

-- | The octave numbers on the drumaxx are one greater than the standard
-- usage.  This is for \"Acoustic 2 FG\".  I'll have to come up with
-- a standard mapping later.
stroke_keys :: [(Drums.Stroke, Midi.Key)]
stroke_keys =
    [ (Drums.c_bd, c2)
    , (Drums.c_bd2, b1)
    , (Drums.c_sn, d2)
    , (Drums.c_sn2, e2)
    , (Drums.stroke 'c' "sn3" (snare <> v3), ds2)
    , (Drums.c_rim, cs2)
    , (Drums.c_ltom, g2)
    , (Drums.c_mtom, b2)
    , (Drums.c_htom, c3)
    , (Drums.c_hh, fs2)
    , (Drums.c_ohh, as2)
    , (Drums.c_ride, ds3)
    , (Drums.c_crash, cs3)
    ]
