-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Synth.Sampler.Patch2 where
import qualified Data.Map as Map

import qualified Util.Seq as Seq
import qualified Cmd.Instrument.ImInst as ImInst
import qualified Synth.Sampler.Sample as Sample
import qualified Synth.Shared.Note as Note

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
    , _karyaPatch :: ImInst.Patch
    }
