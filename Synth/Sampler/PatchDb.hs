-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Synth.Sampler.PatchDb where
import qualified Data.Map as Map

import qualified Cmd.Cmd as Cmd
import qualified Instrument.Inst as Inst
import qualified Synth.Sampler.Patch as Patch
import Synth.Sampler.Patch (attr)
import Global


type Db = Map.Map Patch.Name Patch.Patch

db :: Db
db = Map.fromList
    [ (,) "inst" $ Patch.instrument "inst"
        [ ("cek.wav", attrs cek Patch.sample)
        , ("open.wav", attrs open $ Patch.pitchedSample 60)
        ]
    ]

cek, open :: Patch.Attributes
cek = attr "cek"
open = attr "open"

attrs :: Patch.Attributes -> Patch.Sample -> Patch.Sample
attrs attrs sample = sample { Patch.attributes = attrs }

-- | Declaration for "Local.Instrument".
synth :: Inst.SynthDecl Cmd.InstrumentCode
synth = Inst.SynthDecl "sampler" "音 sampler"
    (map (second Patch.makeInst) (Map.toList db))
