-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Saih gender wayang.
module Derive.Scale.Wayang where
import qualified Derive.Scale as Scale
import qualified Derive.Scale.BaliScales as BaliScales
import qualified Derive.Scale.Util as Util

import qualified Perform.Pitch as Pitch


scale_id :: Pitch.ScaleId
scale_id = Pitch.ScaleId "wayang"

scale :: Scale.Scale
scale = Util.add_doc (BaliScales.scale scale_id scale_map)
    "Saih gender wayang."

scale_map :: BaliScales.ScaleMap
scale_map = BaliScales.scale_map 4 umbang isep

umbang :: [Pitch.NoteNumber]
umbang = map Pitch.nn
    [ 53 -- 6.., pemade begin, dong

    , 55.15 -- 1.
    , 57.73
    , 60.4
    , 62.95 -- 5., pemade middle, ding
    , 64.7 -- 6., kantilan begin

    , 67.57 -- 1 -- "middle C", deng
    , 69.45
    , 72.1
    , 74.83 -- 5, pemade end, ding
    , 76.85

    , 79.48 -- deng
    , 81.63
    , 84.12
    , 86.88 -- 5^, kantilan end, ding
    ]

isep :: [Pitch.NoteNumber]
isep = map Pitch.nn
    [ 52.3 -- dong

    , 54.55
    , 57.35
    , 59.85
    , 62.5 -- ding
    , 64.45 -- 6., kantilan begin

    , 67.26
    , 69.25
    , 71.81
    , 74.63 -- 5, pemade end, ding
    , 76.73

    , 79.35 -- deng
    , 81.51
    , 84
    , 86.78 -- 5^, kantilan end, ding
    ]
