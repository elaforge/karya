-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Export a 'synth' with all the supported patches.
module Synth.Sampler.PatchDb (db2, db, synth) where
import qualified Data.Map as Map

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Instrument.ImInst as ImInst
import qualified Derive.Attrs as Attrs
import qualified Instrument.Inst as Inst
import qualified Synth.Sampler.Patch as Patch
import qualified Synth.Sampler.Patch.Wayang as Wayang
import qualified Synth.Sampler.Patch2 as Patch2
import qualified Synth.Shared.Config as Config


db2 :: Patch2.Db
db2 = Patch2.db "../data/sampler" $ concat
    [ Wayang.patches
    ]

db :: Patch.Db
db = Patch.Db
    { _patches = Map.fromList
        [ ("test",) $ Patch.patch "test"
            [ ("cek.wav", attrs cek Patch.sample)
            , ("open.wav", attrs open $ Patch.pitchedSample 60)
            ]
        ]
    , _rootDir = "Synth/Sampler/instruments"
    }

cek, open :: Attrs.Attributes
cek = Attrs.attr "cek"
open = Attrs.attr "open"

attrs :: Attrs.Attributes -> Patch.Sample -> Patch.Sample
attrs attrs sample = sample { Patch.attributes = attrs }

-- | Declaration for "Local.Instrument".
synth :: Inst.SynthDecl Cmd.InstrumentCode
synth = ImInst.synth Config.samplerName "音 sampler"
    [ (name, Patch2._karyaPatch patch)
    | (name, patch) <- Map.toList (Patch2._patches db2)
    ]
