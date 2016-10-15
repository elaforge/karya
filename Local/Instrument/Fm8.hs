-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Native Instruments FM8 softsynth.
module Local.Instrument.Fm8 where
import qualified Cmd.Instrument.MidiInst as MidiInst


synth :: MidiInst.Synth
synth =
    MidiInst.synth "fm8" "Native Instruments FM8" $
        MidiInst.synth_controls controls patches
    where
    controls =
        [ (4, "fm8-control-1"), (11, "fm8-control-2")
        , (16, "morph-x"), (17, "morph-y")
        ]
    patches = [MidiInst.default_patch (-96, 96) []]
