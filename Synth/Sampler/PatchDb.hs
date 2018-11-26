-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Export a 'synth' with all the supported patches.
module Synth.Sampler.PatchDb (db, synth) where
import qualified Data.Map as Map

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Instrument.ImInst as ImInst
import qualified Instrument.Inst as Inst
import qualified Synth.Sampler.Patch as Patch
import qualified Synth.Sampler.Patch.Mridangam as Mridangam
import qualified Synth.Sampler.Patch.Reyong as Reyong
import qualified Synth.Sampler.Patch.Wayang as Wayang
import qualified Synth.Shared.Config as Config


db :: Patch.Db
db = Patch.db Config.unsafeSamplerRoot $ concat
    [ Reyong.patches
    , Wayang.patches
    , Mridangam.patches
    , [Patch.simple "test" "open.flac" 60]
    ]

-- | Declaration for "Local.Instrument".
synth :: Inst.SynthDecl Cmd.InstrumentCode
synth = ImInst.synth Config.samplerName "音 sampler"
    [ (name, Patch._karyaPatch patch)
    | (name, patch) <- Map.toList (Patch._patches db)
    ]
