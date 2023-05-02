-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Native Instruments' Massive softsynth.
module User.Elaforge.Instrument.Massive where
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Derive.ScoreT as ScoreT

import           Global


synth :: MidiInst.Synth
synth = MidiInst.synth "massive" "Native Instrument Massive" $
    MidiInst.synth_controls controls patches
    where
    controls = (1, "macro1") :
        [ (18 + n, ScoreT.Control $ "macro" <> showt n)
        | n <- [2..8]
        ]

patches :: [MidiInst.Patch]
patches = [MidiInst.default_patch (-24, 24) []]
