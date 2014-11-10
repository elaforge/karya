-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Image-Line's Morphine softsynth.
module Local.Instrument.Morphine where
import qualified Cmd.Instrument.MidiInst as MidiInst


load :: FilePath -> IO [MidiInst.SynthDesc]
load _dir = return $ MidiInst.make $
    MidiInst.softsynth "morphine" "Image-Line Morphine" (-12, 12) controls

controls =
    [
    ]
