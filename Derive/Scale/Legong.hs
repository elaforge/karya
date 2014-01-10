-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Saih Pelegongan.
--
-- Tuning from my gender rambat.
--
-- TODO: pengisep and pengumbang
module Derive.Scale.Legong where
import qualified Derive.Scale as Scale
import qualified Derive.Scale.BaliScales as BaliScales
import qualified Derive.Scale.Scales as Scales

import qualified Perform.Pitch as Pitch

scales :: [Scale.Scale]
scales =
    [ Scales.add_doc "Saih pelegongan, from my instruments." $
        make_scale scale_id absolute_scale
    , make_scale "legong-c" cipher_scale
    , Scales.add_doc
        "Pemade scale. This can be used to give the the same score to both\
            \ pemade and kantilan." $
        make_scale "legong-p" pemade_scale
    , Scales.add_doc
        "Kantilan scale. This can be used to give the the same score to both\
            \ pemade and kantilan." $
        make_scale "legong-k" kantilan_scale
    ]

scale_id :: Pitch.ScaleId
scale_id = "legong"

make_scale :: Pitch.ScaleId -> BaliScales.ScaleMap -> Scale.Scale
make_scale = BaliScales.make_scale "[0-9]ioeua"
    -- TODO I should be able to get the scale pattern directly from the format.
    -- That would come for free if I switched to TheoryFormat

absolute_scale :: BaliScales.ScaleMap
absolute_scale = BaliScales.scale_map 5 1 0 BaliScales.ioeua umbang isep

cipher_scale :: BaliScales.ScaleMap
cipher_scale = BaliScales.scale_map 5 2 1 BaliScales.dotted12356
    (drop 11 umbang) (drop 11 isep)

pemade_scale :: BaliScales.ScaleMap
pemade_scale = BaliScales.scale_map 5 2 1 BaliScales.dotted_ioeua
    (drop 11 umbang) (drop 11 isep)

kantilan_scale :: BaliScales.ScaleMap
kantilan_scale = BaliScales.scale_map 5 3 1 BaliScales.dotted_ioeua
    (drop 16 umbang) (drop 16 isep)

{- | Extended scale.

 @
  1i 1o 1e 1u 1a 2i 2o 2e 2u 2a 3i 3o 3e 3u 3a 4i 4o 4e 4u 4a 5i 5o 5e 5u 5a 6i
  jegog---------
                 calung--------
                    ugal-------------------------
                       rambat-----------------------------------
  11 12 13 15 16 21 22 23 25 26 31 32 33 35 36 41 42 43 45 46 51 52 53 55 56 61
                             trompong---------------------
                                      reyong------------------------------
                                   pemade-----------------------
                                                  kantilan---------------------
  1i 1o 1e 1u 1a 2i 2o 2e 2u 2a 3i 3o 3e 3u 3a 4i 4o 4e 4u 4a 5i 5o 5e 5u 5a 6i
 @
-}
umbang :: [Pitch.NoteNumber]
umbang = extend
    [ 51.82 -- deng, rambat begin
    , 55.7
    , 56.82 -- dang, trompong begin

    , 60.73
    , 62.8 -- dong, pemade begin
    , 63.35 -- deng, reyong begin
    , 67.7
    , 68.2

    , 72.46 -- ding
    , 73.9 -- dong, kantilan begin
    , 75.5
    , 79.4 -- dung, trompong end
    , 80.5

    , 84.46 -- ding, rambat end, pemade end
    , 86
    , 87.67
    , 91.74 -- dung, reyong end
    , 92.5

    , 96.46 -- ding, kantilan end
    ]

extend :: [Pitch.NoteNumber] -> [Pitch.NoteNumber]
extend nns = map (subtract 12) low ++ ding ++ nns
    where
    ding = map (subtract 12) (take 2 (drop 3 nns))
    low = take 5 (ding ++ nns)

isep :: [Pitch.NoteNumber]
isep = umbang -- TODO
