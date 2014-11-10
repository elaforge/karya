-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Applied Accoustic's Tassman softsynth.
module Local.Instrument.Tassman where
import qualified Cmd.Instrument.MidiInst as MidiInst


load :: FilePath -> IO [MidiInst.SynthDesc]
load _dir = return $ MidiInst.make $
    MidiInst.softsynth "tassman" "Applied Accoustics Tassman" (-24, 24) controls

controls =
    [
    ]
