-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Synth.Sampler.Patch where
import qualified Data.Map as Map

import qualified Util.Seq as Seq
import qualified Cmd.Instrument.ImInst as ImInst
import qualified Instrument.Common as Common
import qualified Perform.Im.Patch as Im.Patch
import qualified Perform.Pitch as Pitch
import qualified Synth.Sampler.Sample as Sample
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import Global


type Error = Text

db :: FilePath -> [Patch] -> Db
db rootDir patches = Db
    { _rootDir = rootDir
    , _patches = Map.fromList $ Seq.key_on _name patches
    }

data Db = Db {
    -- | Base directory for patches.  Samples are in _rootDir / _name.
    _rootDir :: !FilePath
    , _patches :: !(Map Note.PatchName Patch)
    }

data Patch = Patch {
    _name :: Note.PatchName
    , _convert :: Note.Note -> Either Error Sample.Sample
    -- | Karya configuration.
    --
    -- Putting code here means that the sampler has to link in a large portion
    -- of karya.  To avoid this I'd have to maintain a separate DB in the
    -- sequencer that matches up by name.  For the moment, linking in the extra
    -- code doesn't seem like a problem.
    , _karyaPatch :: ImInst.Patch
    }

-- | Make a simple patch of a single sample.
simple :: Note.PatchName -> Sample.SamplePath -> Pitch.NoteNumber -> Patch
simple name filename sampleNn = Patch
    { _name = name
    , _convert = \note -> do
        pitch <- tryJust "no pitch" $ Note.initialPitch note
        dyn <- tryJust "no dyn" $ Note.initial Control.dynamic note
        return $ Sample.Sample
            { filename = filename
            , offset = 0
            , envelope = Signal.constant dyn
            , ratio = Signal.constant $
                Sample.pitchToRatio (Pitch.nn_to_hz sampleNn) pitch
            }
    , _karyaPatch = ImInst.make_patch $
        Im.Patch.Patch
            { patch_controls = Control.supportPitch <> Control.supportDyn
            , patch_attribute_map = Common.attribute_map []
            , patch_flags = mempty
            }
    }
