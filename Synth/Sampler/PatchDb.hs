-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Export a 'synth' with all the supported patches.
module Synth.Sampler.PatchDb (db, synth) where
import qualified Data.Map as Map

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Instrument.ImInst as ImInst
import qualified Instrument.Inst as Inst
import qualified Perform.Im.Patch as Im.Patch
import qualified Synth.Sampler.Patch as Patch
import qualified Synth.Sampler.Patch.KendangBali as KendangBali
import qualified Synth.Sampler.Patch.Mridangam as Mridangam
import qualified Synth.Sampler.Patch.Rambat as Rambat
import qualified Synth.Sampler.Patch.Reyong as Reyong
import qualified Synth.Sampler.Patch.Wayang as Wayang
import qualified Synth.Shared.Config as Config

import           Global


db :: Patch.Db
db = Patch.db Config.unsafeSamplerRoot $ concat
    [ KendangBali.patches
    , Mridangam.patches
    , Rambat.patches
    , Reyong.patches
    , Wayang.patches
    , [Patch.DbPatch $ Patch.simple "test" "open.flac" 60]
    ]

-- | Declaration for "Local.Instrument".
synth :: Inst.SynthDecl Cmd.InstrumentCode
synth = Inst.SynthDecl Config.samplerName "音 sampler" $
    map (second make) (Map.toList (Patch._patches db))
    where
    make (Patch.DbPatch p) = Inst.Inst
        { inst_backend = Inst.Im $ patch
            { Im.Patch.patch_controls = Im.Patch.patch_controls patch
                <> Patch.standardControls
            }
        , inst_common = ImInst.make_code <$> common
        }
        where ImInst.Patch patch common = Patch._karyaPatch p
    make (Patch.DbDummy (Patch.Dummy _ common)) = Inst.Inst
        { inst_backend = Inst.Dummy
        , inst_common = ImInst.make_code <$> common
        }
