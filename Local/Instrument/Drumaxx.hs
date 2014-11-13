-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Image-Line's Drumaxx softsynth.
module Local.Instrument.Drumaxx where
import qualified Data.Set as Set

import Midi.Key
import qualified Midi.Midi as Midi
import qualified Cmd.Instrument.CUtil as CUtil
import qualified Cmd.Instrument.Drums as Drums
import qualified Cmd.Instrument.MidiInst as MidiInst

import Derive.Attrs
import qualified Derive.Instrument.DUtil as DUtil
import qualified Perform.Midi.Instrument as Instrument
import Global


load :: FilePath -> IO [MidiInst.SynthDesc]
load _dir = return $ MidiInst.make $
    (MidiInst.softsynth "drumaxx" "Image-Line Drumaxx" pb_range [])
    { MidiInst.modify_wildcard = CUtil.drum_patch note_keys
    , MidiInst.code = code
    , MidiInst.extra_patches = patches
    }

pb_range = (-24, 24)

code :: MidiInst.Code
code = CUtil.drum_code Nothing (map fst note_keys)

patches :: [MidiInst.Patch]
patches = map (MidiInst.with_code composite_code)
    [ Instrument.text #= "This drum takes a pitch signal, which is then sent\
        \ to the `composite-pitched` instrument, which is a tuned comb filter.\
        \ The audio routing has to be set up in the VST host." $
        MidiInst.patch pb_range "comb" []
    ]
    where
    composite_code = MidiInst.note_generators
        [(call, composite call) | call <- map (Drums.note_name . fst) note_keys]
    composite call = DUtil.redirect_pitch "comb"
        "" (Just (Set.fromList ["mix", "fbk"]))
        call Nothing

-- | The octave numbers on the drumaxx are one greater than the standard
-- usage.  This is for \"Acoustic 2 FG\".  I'll have to come up with
-- a standard mapping later.
note_keys :: [(Drums.Note, Midi.Key)]
note_keys =
    [ (Drums.c_bd, c2)
    , (Drums.c_bd2, b1)
    , (Drums.c_sn, d2)
    , (Drums.c_sn2, e2)
    , (Drums.Note "sn3" (snare <> v3) 'c' 1, ds2)
    , (Drums.c_rim, cs2)
    , (Drums.c_ltom, g2)
    , (Drums.c_mtom, b2)
    , (Drums.c_htom, c3)
    , (Drums.c_hh, fs2)
    , (Drums.c_ohh, as2)
    , (Drums.c_ride, ds3)
    , (Drums.c_crash, cs3)
    ]
