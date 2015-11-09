-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Native Instruments' Massive softsynth.
module Local.Instrument.Massive where
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Derive.Score as Score
import qualified Perform.Midi.Instrument as Instrument
import Global


load :: FilePath -> IO (Maybe MidiInst.Synth)
load _dir = return $ Just $
    MidiInst.with_patches (map MidiInst.with_empty_code patches) $
    Instrument.synth "massive" "Native Instruments Massive" controls
    where
    controls = (1, "macro1")
        : [(18 + n, Score.unchecked_control $ "macro" <> showt n) | n <- [2..8]]

patches :: [Instrument.Patch]
patches = [Instrument.default_patch (-24, 24) []]
