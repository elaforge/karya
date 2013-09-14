-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Image-Line's Drumaxx softsynth.
module Local.Instrument.Drumaxx where
import Util.Control
import Midi.Key
import qualified Midi.Midi as Midi
import qualified Cmd.Instrument.Drums as Drums
import qualified Cmd.Instrument.CUtil as CUtil
import Derive.Attrs
import qualified Derive.Score as Score
import qualified Perform.Midi.Instrument as Instrument
import qualified App.MidiInst as MidiInst


load :: FilePath -> IO [MidiInst.SynthDesc]
load _dir = return $ MidiInst.make $
    (MidiInst.softsynth "dmx" "Image-Line Drumaxx" pb_range [])
    { MidiInst.modify_wildcard = CUtil.drum_instrument notes
    , MidiInst.code = code
    , MidiInst.extra_patches = patches
    }

pb_range = (-24, 24)

code :: MidiInst.Code
code = MidiInst.note_generators (CUtil.drum_calls (map fst notes))
    <> MidiInst.cmd (CUtil.drum_cmd notes)

patches :: [MidiInst.Patch]
patches = MidiInst.with_code code $ map make_patch
    [ Instrument.text #= "This drum takes a pitch signal, which is then sent\
        \ to the >reak/comb instrument, which is a tuned comb filter.\
        \ The audio routing has to be set up in the VST host." $
        composite $ MidiInst.patch pb_range "comb" []
    ]
    where
    -- Since this drum has pitches and continuous controls, its notes need to
    -- have duration.
    make_patch = Instrument.unset_flag Instrument.Triggered
        . CUtil.drum_instrument notes
    composite = Instrument.add_composite (Score.instrument "reak" "comb")
        Nothing ["mix", "fbk"]

-- | The octave numbers on the drumaxx are one greater than the standard
-- usage.  This is for \"Acoustic 2 FG\".  I'll have to come up with
-- a standard mapping later.
notes :: [(Drums.Note, Midi.Key)]
notes =
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
