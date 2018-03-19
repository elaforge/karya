-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Image-Line's Morphine softsynth.
module User.Elaforge.Instrument.Morphine where
import qualified Cmd.Instrument.MidiInst as MidiInst


synth :: MidiInst.Synth
synth = MidiInst.synth "morphine" "Image-Line Morphine" patches

patches :: [MidiInst.Patch]
patches = [MidiInst.default_patch (-12, 12) []]
