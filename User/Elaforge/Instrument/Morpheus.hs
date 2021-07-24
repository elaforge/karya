-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | E-mu morpheus module.
module User.Elaforge.Instrument.Morpheus where
import           System.FilePath ((</>))

import qualified App.Config as Config
import qualified App.Path as Path
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Derive.ScoreT as ScoreT
import qualified Instrument.InstT as InstT
import qualified Instrument.Parse as Parse
import qualified Midi.Midi as Midi
import qualified Perform.Midi.Patch as Patch

import           Global


synth_name :: InstT.SynthName
synth_name = "morpheus"

load :: Path.AppDir -> IO (Maybe MidiInst.Synth)
load = MidiInst.load_synth (const mempty) synth_name "E-mu Morpheus"

make_db :: Path.AppDir -> IO ()
make_db app_dir = do
    let fname = Path.to_absolute app_dir Config.instrument_dir
            </> untxt synth_name
    patches <- map MidiInst.patch_from_pair <$> Parse.patch_file fname
    patches <- return $
        map (MidiInst.patch#Patch.defaults#Patch.pitch_bend_range
                #= Just (-12, 12))
            patches
    MidiInst.save_synth app_dir synth_name patches

synth_controls :: [(Midi.Control, ScoreT.Control)]
synth_controls =
    -- Definitions depend on the preset.
    [ (1, "a"), (2, "b"), (3, "c"), (4, "d")
    , (64, "switch1"), (65, "switch2"), (66, "switch3")
    ]
