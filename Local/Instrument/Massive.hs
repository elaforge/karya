-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Native Instruments' Massive softsynth.
module Local.Instrument.Massive where
import qualified Midi.Midi as Midi
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Derive.Score as Score
import qualified Perform.Midi.Instrument as Instrument
import Global


load :: FilePath -> IO [MidiInst.SynthDesc]
load _dir = return $ MidiInst.make $
    (MidiInst.softsynth "massive" "Native Instruments Massive" (-24, 24)
            controls)
        { MidiInst.extra_patches = map MidiInst.with_empty_code patches }

controls :: [(Midi.Control, Score.Control)]
controls = (1, "macro1")
    : [(18 + n, Score.unchecked_control $ "macro" <> showt n) | n <- [2..8]]

patches :: [Instrument.Patch]
patches = []
