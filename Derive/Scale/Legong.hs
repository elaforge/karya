-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Saih Pelegongan.
--
-- Tuning for my gender rambat.
--
-- TODO: pengisep and pengumbang
module Derive.Scale.Legong where
import qualified Derive.Scale as Scale
import qualified Derive.Scale.BaliScales as BaliScales
import qualified Derive.Scale.Util as Util

import qualified Perform.Pitch as Pitch


scale_id :: Pitch.ScaleId
scale_id = Pitch.ScaleId "legong"

scale :: Scale.Scale
scale = Util.add_doc (BaliScales.scale scale_id scale_map)
    "Saih pelegongan. Tuning from my gender rambat."

scale_map :: BaliScales.ScaleMap
scale_map = BaliScales.scale_map 1 umbang isep

umbang :: [Pitch.NoteNumber]
umbang = map Pitch.nn
    [ 50.8 -- 2.., ugal begin
    , 51.82 -- 3.., rambat begin
    , 55.7
    , 56.82 -- 6.., trompong begin

    , 60.73
    , 62.8 -- 2., pemade begin
    , 63.35 -- 3., reyong begin
    , 67.7
    , 68.2

    , 72.46 -- 1
    , 73.9 -- 2, kantilan begin
    , 75.5
    , 79.4 -- 5, trompong end
    , 80.5

    , 84.46 -- 1^, rambat end, pemade end
    , 86
    , 87.67
    , 91.74 -- 5^, reyong end
    , 92.5

    , 96.46 -- 1^^, kantilan end
    ]

isep :: [Pitch.NoteNumber]
isep = umbang -- TODO
