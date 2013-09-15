-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Saih gender wayang.
module Derive.Scale.Wayang where
import qualified Derive.Scale as Scale
import qualified Derive.Scale.BaliScales as BaliScales
import qualified Derive.Scale.Util as Util

import qualified Perform.Pitch as Pitch


scales :: [Scale.Scale]
scales =
    [ Util.add_doc "Saih gender wayang." $
        make_scale scale_id absolute_scale
    , Util.add_doc "Pemade scale. This can be used to give the the same score\
            \ to both pemade and kantilan." $
        make_scale "wayang-p" pemade_scale
    , Util.add_doc "Kantilan scale. This can be used to give the the same score\
            \ to both pemade and kantilan." $
        make_scale "wayang-k" kantilan_scale
    ]

make_scale :: Pitch.ScaleId -> BaliScales.ScaleMap -> Scale.Scale
make_scale = BaliScales.make_scale "[0-9]ioeua"
    -- TODO I should be able to get the scale pattern directly from the format.
    -- That would come for free if I switched to TheoryFormat

scale_id :: Pitch.ScaleId
scale_id = "wayang"

-- | Use ding deng dong dang dung.  I don't know if this is ever actually used
-- for gender, but the notation is compact.
--
-- > 2o  2e  2u  2a  3i  3o  3e  3u  3a  4i  4o  4e  4u  4a  5i
-- > pemade -------------------------------
-- >                     kantilan -----------------------------
absolute_scale :: BaliScales.ScaleMap
absolute_scale = BaliScales.scale_map 5 0 0 (BaliScales.ioeua 0)
    (extend umbang) (extend isep)

pemade_scale :: BaliScales.ScaleMap
pemade_scale =
    BaliScales.scale_map 5 2 1 (drop 1 (BaliScales.ioeua 1)) umbang isep

kantilan_scale :: BaliScales.ScaleMap
kantilan_scale = BaliScales.scale_map 5 3 1 (drop 1 (BaliScales.ioeua 1))
    (drop 5 umbang) (drop 5 isep)

-- | pemade starts at 2o--3i--4i, kanti is 3o--4i--5i
extend :: [Pitch.NoteNumber] -> [Pitch.NoteNumber]
extend nns =
    ding - 36 : map (subtract 24) low ++ map (subtract 12) low
        ++ nns ++ map (+12) high ++ map (+24) high
    where
    ding = nns !! 4
    low = take 5 nns -- oeuai
    high = drop (length nns - 5) nns -- oeuai

umbang :: [Pitch.NoteNumber]
umbang =
    [ 53 -- pemade begin
    , 55.15
    , 57.73
    , 60.4

    , 62.95 -- pemade middle
    , 64.7 -- kantilan begin
    , 67.57
    , 69.45
    , 72.1

    , 74.83 -- pemade end, kantilan middle
    , 76.85
    , 79.48
    , 81.63
    , 84.12
    , 86.88 -- kantilan end
    ]

isep :: [Pitch.NoteNumber]
isep =
    [ 52.3 -- pemade begin

    , 54.55
    , 57.35
    , 59.85
    , 62.5 -- pemade middle
    , 64.45 -- kantilan begin

    , 67.26
    , 69.25
    , 71.81
    , 74.63 -- pemade end, kantilan middle
    , 76.73

    , 79.35
    , 81.51
    , 84
    , 86.78 -- kantilan end
    ]
