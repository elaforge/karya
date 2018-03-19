-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Applied Accoustic's Tassman softsynth.
module User.Elaforge.Instrument.Tassman where
import qualified Cmd.Instrument.MidiInst as MidiInst


synth :: MidiInst.Synth
synth = MidiInst.synth "tassman" "Applied Accoustics Tassman" patches

patches :: [MidiInst.Patch]
patches = [MidiInst.default_patch (-24, 24) []]
