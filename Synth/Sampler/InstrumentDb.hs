-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Synth.Sampler.InstrumentDb where
import qualified Data.Map as Map

import qualified Synth.Sampler.Instrument as Instrument
import Synth.Sampler.Instrument (attr)


type Db = Map.Map Instrument.Name Instrument.Instrument

db :: Db
db = Map.fromList
    [ (,) "inst" $ Instrument.instrument "inst"
        [ ("cek.wav", attrs cek Instrument.sample)
        , ("open.wav", attrs open $ Instrument.pitchedSample 60)
        ]
    ]

cek, open :: Instrument.Attributes
cek = attr "cek"
open = attr "open"

attrs :: Instrument.Attributes -> Instrument.Sample -> Instrument.Sample
attrs attrs sample = sample { Instrument.attributes = attrs }
