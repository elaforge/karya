-- Copyright 2022 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module User.Generic.Instrument.OBXd where
import qualified Data.String as String

import qualified Cmd.Instrument.MidiInst as MidiInst


synth :: MidiInst.Synth
synth =
    MidiInst.synth "obxd" "https://www.discodsp.com/obxd/" $
        MidiInst.synth_controls controls
        [MidiInst.default_patch pb_range []]
    where
    controls = []

pb_range :: (Int, Int)
pb_range = (-2, 2)
