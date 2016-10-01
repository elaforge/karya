-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | E-mu morpheus module.
module Local.Instrument.Morpheus where
import System.FilePath ((</>))

import qualified Midi.Midi as Midi
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Derive.Score as Score
import qualified Perform.Midi.Patch as Patch
import qualified Instrument.InstTypes as InstTypes
import qualified Instrument.Parse as Parse
import Global


synth_name :: InstTypes.SynthName
synth_name = "morpheus"

load :: FilePath -> IO (Maybe MidiInst.Synth)
load = MidiInst.load_synth (const mempty) synth_name "E-mu Morpheus"

make_db :: FilePath -> IO ()
make_db dir = do
    patches <- map MidiInst.patch_from_pair <$>
        Parse.patch_file (dir </> untxt synth_name)
    patches <- return $
        map (MidiInst.patch#Patch.defaults#Patch.pitch_bend_range #= (-12, 12))
            patches
    MidiInst.save_synth dir synth_name patches

synth_controls :: [(Midi.Control, Score.Control)]
synth_controls =
    -- Definitions depend on the preset.
    [ (1, "a"), (2, "b"), (3, "c"), (4, "d")
    , (64, "switch1"), (65, "switch2"), (66, "switch3")
    ]
