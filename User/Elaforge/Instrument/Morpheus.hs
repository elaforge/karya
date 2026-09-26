-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | E-mu morpheus module.
module User.Elaforge.Instrument.Morpheus where
import           System.FilePath ((</>))

import qualified App.Config as Config
import qualified App.Path as Path
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Cmd.Instrument.MidiInstDb as MidiInstDb
import qualified Derive.ScoreT as ScoreT
import qualified Instrument.InstT as InstT
import qualified Instrument.Parse as Parse
import qualified Midi.Midi as Midi
import qualified Perform.Midi.Patch as Patch

import           Global


synthName :: InstT.SynthName
synthName = "morpheus"

load :: Path.AppDir -> IO (Maybe MidiInst.Synth)
load = MidiInstDb.loadSynth (const mempty) synthName "E-mu Morpheus"

makeDb :: Path.AppDir -> IO ()
makeDb appDir = do
    let fname = Path.toAbsolute appDir Config.instrumentDir
            </> untxt synthName
    patches <- map MidiInst.patchFromPair <$> Parse.patchFile fname
    patches <- pure $
        map (MidiInst.patch#Patch.defaults#Patch.pitchBendRange
                #= Just (-12, 12))
            patches
    MidiInstDb.saveSynth appDir synthName patches

synthControls :: [(Midi.Control, ScoreT.Control)]
synthControls =
    -- Definitions depend on the preset.
    [ (1, "a"), (2, "b"), (3, "c"), (4, "d")
    , (64, "switch1"), (65, "switch2"), (66, "switch3")
    ]
