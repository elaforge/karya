module Synth.Sampler.InstrumentDb where
import qualified Data.Map as Map

import qualified Synth.Sampler.Instrument as Instrument


type Db = Map.Map Instrument.Name Instrument.Instrument

db :: Db
db = Map.fromList
    [ (,) "inst" $ Instrument.instrument "inst"
        [ ("cek.wav", attr cek Instrument.sample)
        , ("open.wav", attr open $ Instrument.pitchedSample 60)
        ]
    ]

cek, open :: Instrument.Attribute
cek = "cek"
open = "open"

attr :: Instrument.Attribute -> Instrument.Sample -> Instrument.Sample
attr attr sample = sample { Instrument.attribute = attr }
