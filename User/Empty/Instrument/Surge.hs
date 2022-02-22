-- Copyright 2022 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module User.Empty.Instrument.Surge where
import qualified Data.String as String

import qualified Cmd.Instrument.MidiInst as MidiInst


synth :: MidiInst.Synth
synth =
    MidiInst.synth "surge" "https://surge-synthesizer.github.io/index.html" $
        MidiInst.synth_controls controls
        [MidiInst.default_patch pb_range []]
    where
    controls = [(40 + n, String.fromString $ "macro" <> show n) | n <- [1..8]]

-- PB range when in MPE mode.
pb_range :: (Int, Int)
pb_range = (-48, 48)
