-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Audio Modeling's SWAM.
module User.Elaforge.Instrument.Swam where
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Derive.Attrs as Attrs
import qualified Derive.Controls as Controls
import qualified Instrument.InstTypes as InstTypes
import qualified Midi.CC as CC
import qualified Perform.Midi.Patch as Patch

import           Global


synth :: MidiInst.Synth
synth =
    MidiInst.synth "swam" "Audio Modeling SWAM" $
        MidiInst.synth_controls [] patches
    where
    patches = map string ["violin", "viola", "cello", "bass"]

string :: InstTypes.Name -> MidiInst.Patch
string name = MidiInst.pressure $
    MidiInst.patch#Patch.mode_map #= modes $
    MidiInst.patch#Patch.attribute_map #= keyswitches $
    MidiInst.named_patch (-24, 24) name controls
    where
    controls =
        [ (CC.mod, Controls.vib)
        , (CC.vib_speed, Controls.vib_speed)
        , (CC.pan, Controls.pan)
        , (15, "bow-force")
        , (16, "bow-pos")
        , (17, "bow-noise")
        -- <64 or >=64
        , (64, Controls.pedal) -- called sustain, but I use that elsewhere
        ]
    -- CC breakpoints are <=42, <=84, >=85
    keyswitches = Patch.cc_keyswitches_permute
        [ (32, [(mempty, 10), (Attrs.pizz, 60), (Attrs.legno, 100)])
        , (39, [(mempty, 10), (Attrs.harm, 60), (Attrs.harm<>Attrs.third, 100)])
        , (65, [(mempty, 0), (Attrs.mute, 127)]) -- con sord
        ]
    modes = Patch.cc_mode_map
        [ ("gesture", 33, [("expr", 10), ("bipolar", 60), ("bowing", 100)])
        , ("fingering", 36, [("mid", 10), ("bridge", 60), ("nut", 100)])
        , ("bow-lift", 37, [("f", 10), ("t", 80)])
        , ("bow-start", 38, [("d", 10), ("u", 80)])
        ]
