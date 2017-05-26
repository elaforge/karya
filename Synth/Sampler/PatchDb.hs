-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Export a 'synth' with all the supported patches.
module Synth.Sampler.PatchDb (db, synth) where
import qualified Data.Map as Map

import qualified Cmd.Cmd as Cmd
import qualified Instrument.Inst as Inst
import qualified Synth.Sampler.Patch as Patch
import qualified Synth.Shared.Types as Types
import Synth.Shared.Types (attribute)


type Db = Map.Map Types.PatchName Patch.Patch

db :: Db
db = Map.fromList
    [ (,) "inst" $ Patch.instrument "inst"
        [ ("cek.wav", attrs cek Patch.sample)
        , ("open.wav", attrs open $ Patch.pitchedSample 60)
        ]
    ]

cek, open :: Types.Attributes
cek = attribute "cek"
open = attribute "open"

attrs :: Types.Attributes -> Patch.Sample -> Patch.Sample
attrs attrs sample = sample { Patch.attributes = attrs }

-- | Declaration for "Local.Instrument".
synth :: Inst.SynthDecl Cmd.InstrumentCode
synth = Inst.SynthDecl "sampler" "音 sampler"
    [(name, Patch.makeInst patch) | (name, patch) <- Map.toList db]
