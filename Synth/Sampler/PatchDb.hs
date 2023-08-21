-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Export a 'synth' with all the supported patches.
module Synth.Sampler.PatchDb (db, synth) where
import qualified Data.Map as Map

import qualified Util.Maps as Maps
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Instrument.ImInst as ImInst
import qualified Instrument.Inst as Inst
import qualified Perform.Im.Patch as Im.Patch
import qualified Synth.Faust.EffectC as EffectC
import qualified Synth.Sampler.Patch as Patch
import qualified Synth.Sampler.Patch.Break as Break
import qualified Synth.Sampler.Patch.CengCeng as CengCeng
import qualified Synth.Sampler.Patch.Java as Java
import qualified Synth.Sampler.Patch.Kajar as Kajar
import qualified Synth.Sampler.Patch.KendangBali as KendangBali
import qualified Synth.Sampler.Patch.LittleGong as LittleGong
import qualified Synth.Sampler.Patch.Metronome as Metronome
import qualified Synth.Sampler.Patch.Mridangam as Mridangam
import qualified Synth.Sampler.Patch.Rambat as Rambat
import qualified Synth.Sampler.Patch.Reyong as Reyong
import qualified Synth.Sampler.Patch.Sample as Sample
import qualified Synth.Sampler.Patch.ScGamelan as ScGamelan
import qualified Synth.Sampler.Patch.Wayang as Wayang
import qualified Synth.Sampler.Patch.Zheng as Zheng
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Control as Control

import           Global


db :: Patch.Db
db = Patch.db Config.unsafeSamplerRoot $ concat
    [ Break.patches
    , CengCeng.patches
    , Java.patches
    , Kajar.patches
    , KendangBali.patches
    , LittleGong.patches
    , Metronome.patches
    , Mridangam.patches
    , Rambat.patches
    , Reyong.patches
    , Sample.patches
    , ScGamelan.patches
    , Wayang.patches
    , Zheng.patches
    , [Patch.simple "test" "open.flac" 60]
    ]

-- | Declaration for "Local.Instrument".
synth :: Inst.SynthDecl Cmd.InstrumentCode
synth = Inst.SynthDecl Config.samplerName "音 sampler" $
    map (second make) (Map.toList (Patch._patches db))
    where
    make p = ImInst.make_inst $
        ImInst.patch#Im.Patch.controls %= update (Patch._effect p) $
        Patch._karyaPatch p
    update effect controls = mconcat
        [ maybe mempty effectControls effect
        , controls
        , Patch.standardControls
        ]

effectControls :: Patch.EffectConfig -> Map Control.Control Text
effectControls (Patch.EffectConfig name toEffectControl) =
    case Map.lookup name EffectC.patches of
        Just (Right effect) ->
            Map.mapKeys (\c -> Map.findWithDefault c c toScoreControl) $
                (("Effect: " <> name <> ": ") <>) . snd <$>
                EffectC._controls effect
        _ -> mempty
    where
    toScoreControl = Maps.invert toEffectControl
