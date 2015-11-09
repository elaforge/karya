-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Image-Line's Morphine softsynth.
module Local.Instrument.Morphine where
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Perform.Midi.Instrument as Instrument


load :: FilePath -> IO (Maybe MidiInst.Synth)
load _dir = return $ Just $ MidiInst.with_patches patches $
    Instrument.synth "morphine" "Image-Line Morphine" []

patches :: [MidiInst.Patch]
patches = map MidiInst.with_empty_code
    [ Instrument.default_patch (-12, 12) []
    ]
