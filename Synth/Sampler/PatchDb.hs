-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Export a 'synth' with all the supported patches.
module Synth.Sampler.PatchDb (db, synth) where
import qualified Data.Map as Map

import qualified Cmd.Cmd as Cmd
import qualified Derive.Attrs as Attrs
import qualified Instrument.Inst as Inst
import qualified Synth.Sampler.Patch as Patch
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Note as Note


type Db = Map.Map Note.PatchName Patch.Patch

db :: Db
db = Map.fromList
    [ ("test",) $ Patch.patch "test"
        [ ("cek.wav", attrs cek Patch.sample)
        , ("open.wav", attrs open $ Patch.pitchedSample 60)
        ]
    ]

cek, open :: Attrs.Attributes
cek = Attrs.attr "cek"
open = Attrs.attr "open"

attrs :: Attrs.Attributes -> Patch.Sample -> Patch.Sample
attrs attrs sample = sample { Patch.attributes = attrs }

-- | Declaration for "Local.Instrument".
synth :: Inst.SynthDecl Cmd.InstrumentCode
synth = Inst.SynthDecl Config.samplerName "音 sampler"
    [(name, Patch.makeInst patch) | (name, patch) <- Map.toList db]
