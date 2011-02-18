-- | Calls for a western style drumset.
module Derive.Instrument.Drums where
import Util.Control

import Derive.Attrs
import qualified Derive.Derive as Derive
import qualified Derive.Instrument.Util as Util


traps :: Derive.NoteCallMap
traps = Derive.make_calls $ map (second Util.with_attrs)
    [ ("sn", snare)
    , ("bd", bd)
    , ("hh", hh)
    , ("ohh", open @+ hh)
    , ("phh", pedal @+ hh)
    , ("crash", crash)
    , ("tom", tom)
    , ("htom", high @+ tom)
    , ("mtom", middle @+ tom)
    , ("ltom", low @+ tom)
    ]

-- TODO other drum style ornaments like double strikes, rolls, etc.

hang :: Derive.NoteCallMap
hang = Derive.make_calls $ map (second Util.with_attrs)
    [ ("`pang2`", edge)
    , ("`da3`", slap)
    , ("`zhong1`", middle)
    , ("`zhi3`", knuckle)
    ]
