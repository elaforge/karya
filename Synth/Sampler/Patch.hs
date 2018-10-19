-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Synth.Sampler.Patch where
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Identity as Identity
import qualified Data.Map as Map

import qualified Util.Log as Log
import qualified Util.Seq as Seq
import qualified Cmd.Instrument.ImInst as ImInst
import qualified Perform.Im.Patch as Im.Patch
import qualified Perform.Pitch as Pitch
import qualified Synth.Sampler.Sample as Sample
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import Global
import Synth.Types


db :: FilePath -> [Patch] -> Db
db rootDir patches = Db
    { _rootDir = rootDir
    , _patches = Map.fromList $ Seq.key_on _name patches
    }

data Db = Db {
    -- | Base directory for patches.  Samples are in '_rootDir' / '_dir'.
    _rootDir :: !FilePath
    , _patches :: !(Map Note.PatchName Patch)
    }

data Patch = Patch {
    _name :: Note.PatchName
    -- | Root dir for samples, relative to '_rootDir'.  This is not the same
    -- as '_name' because multiple patches may share a sample directory.
    , _dir :: FilePath
    -- | Find a sample.  Returns (newDuration, sample) since the decay time
    -- might extend the duration.
    , _convert :: Note.Note -> ConvertM (RealTime, Sample.Sample)
    , _preprocess :: [Note.Note] -> [Note.Note]
    -- | Karya configuration.
    --
    -- Putting code here means that the sampler has to link in a large portion
    -- of karya.  To avoid this I'd have to maintain a separate DB in the
    -- sequencer that matches up by name.  For the moment, linking in the extra
    -- code doesn't seem like a problem.
    , _karyaPatch :: ImInst.Patch
    }

patch :: Note.PatchName -> Patch
patch name = Patch
    { _name = name
    , _dir = untxt name
    , _convert = const $ Except.throwError "not implemented"
    , _preprocess = id
    , _karyaPatch = ImInst.make_patch Im.Patch.patch
    }

-- | Make a simple patch of a single sample.
simple :: Note.PatchName -> Sample.SamplePath -> Pitch.NoteNumber -> Patch
simple name filename sampleNn = (patch name)
    { _convert = \note -> do
        pitch <- tryJust "no pitch" $ Note.initialPitch note
        dyn <- tryJust "no dyn" $ Note.initial Control.dynamic note
        return $ (Note.duration note,) $ Sample.Sample
            { filename = filename
            , offset = 0
            , envelope = Signal.constant dyn
            , ratio = Signal.constant $
                Sample.pitchToRatio (Pitch.nn_to_hz sampleNn) pitch
            }
    , _karyaPatch = ImInst.make_patch $ Im.Patch.patch
        { Im.Patch.patch_controls = Control.supportPitch <> Control.supportDyn
        }
    }

type ConvertM a = Log.LogT (Except.ExceptT Error Identity.Identity) a
type Error = Text

convert :: Patch -> Note.Note
    -> Either Error ((RealTime, Sample.Sample), [Log.Msg])
convert note = runConvert . _convert note

runConvert :: ConvertM a -> Either Error (a, [Log.Msg])
runConvert = Identity.runIdentity . Except.runExceptT . Log.run
