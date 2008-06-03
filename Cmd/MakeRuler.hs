{- | Functions to construct and modify rulers and their marklists.
-}
module Cmd.MakeRuler where
import qualified Data.List as List
import qualified Data.Map as Map

import Ui.Types
import qualified Ui.Color as Color
import qualified Ui.Ruler as Ruler

import qualified App.Config as Config

import Util.Test
import Text.Printf


-- * constants

empty_ruler :: Ruler.Ruler
empty_ruler = ruler []

-- | The meter marklist by convention has marks corresponding to the meter of
-- the piece.  Other commands may use this to find out where beats are.
-- By convention, this marklist starts at rank 1 and goes up.  This is so that
-- rank 0 can be used for special cues and whatnot.
meter_marklist :: Ruler.MarklistName
meter_marklist = "meter"

-- ** meter constants

-- | Meters are rulers with repeating marks based on even subdivisions.
-- Each subdivided unit is called a measure, and the MeasureConfig describes
-- the marks at that measure's level, and how many sub-measures it's divided
-- into.
--
-- In traditional notation, one measure level would correspond to the barlines,
-- and the next measure below that would correspond to the meter.  E.g. 3/4
-- would be an unspecified number of measures with 3 divisions, followed by
-- 4 divisions, with unspecified further divisions.  9/8 would be n->3->3->n.
--
-- TODO this doesn't support irregular subdivisions, like 3+3+2, I could do
-- that with a list of divisions.
--
-- (mark_color, (rank, width, zoom_level), divisions)
type MeasureConfig = (Color.Color, (Integer, Integer, Double), Integer)

-- | The mark color defaults to mostly transparent so it looks nice on overlay
-- rulers.
mcolor r g b = Color.rgba r g b 0.35

-- | Colors used for the meter marklist, in order of increasing rank.
meter_colors :: [Color.Color]
meter_colors =
    [ mcolor 0.0 0.0 0.0 -- block section
    , mcolor 0.4 0.3 0.0 -- whole
    , mcolor 1.0 0.4 0.2 -- quarter
    , mcolor 1.0 0.2 0.7 -- 16th
    , mcolor 0.1 0.5 0.1 -- 64th
    , mcolor 0.0 0.0 0.0 -- 256th
    ]

-- | Rank descriptions, used with 'meter_colors' to form [MeasureConfig].
meter_ranks :: [(Integer, Integer, Double)]
meter_ranks =
    [ (0, 3, 0)
    , (1, 3, 0.1)
    , (2, 2, 1)
    , (3, 1, 4)
    , (4, 1, 16)
    , (5, 1, 64)
    ]

-- * constructors

-- | Constructor for "plain" rulers.
ruler :: [(Ruler.MarklistName, Ruler.Marklist)] -> Ruler.Ruler
ruler marklists = Ruler.ruler marklists Config.ruler_bg True False False

-- | Convert a ruler to be suitable as an overlay ruler.
as_overlay :: Ruler.Ruler -> Ruler.Ruler
as_overlay ruler = ruler
    { Ruler.ruler_show_names = False
    , Ruler.ruler_use_alpha = True
    , Ruler.ruler_full_width = True
    }

-- ** construct meters

regular_meter :: TrackPos -> [Integer] -> Ruler.NameMarklist
regular_meter dur divs = Ruler.marklist meter_marklist (make_marks dur configs)
    where configs = List.zip3 meter_colors meter_ranks divs

make_marks :: TrackPos -> [MeasureConfig] -> [Ruler.PosMark]
make_marks dur configs = Map.toAscList $ Map.fromAscListWith (flip const) $
    make_measure "" (TrackPos 0) dur configs

-- | Make a measure and the measure levels beneath it, according to the given
-- MeasureConfigs.
make_measure :: String -> TrackPos -> TrackPos -> [MeasureConfig]
    -> [Ruler.PosMark]
make_measure _ _ _ [] = []
make_measure name pos measure_dur (config:rest_config) =
    concat [(pos_at n, mark_at n) : sub_measure n | n <- [0..divisions-1]]
    where
    sub_measure n =
        make_measure (name_at n) (pos_at n) sub_measure_dur rest_config

    mark_at n = mark_config config (name_at n)
    pos_at n = pos + sub_measure_dur * TrackPos n

    name_at n = name ++ (if null name then "" else ".") ++ show n
    (_, _, divisions) = config
    sub_measure_dur = measure_dur `div` TrackPos divisions

mark_config :: MeasureConfig -> String -> Ruler.Mark
mark_config config name = Ruler.Mark (fromIntegral rank) (fromIntegral width)
        color name (zoom*2) zoom
    where
    (color, (rank, width, zoom), _) = config


-- * testing

m44_configs :: [MeasureConfig]
m44_configs = [(color, rank, 4) | (color, rank) <- zip meter_colors meter_ranks]

m44 = make_marks (TrackPos 64) (take 2 m44_configs)
pretty_marks pos_marks = pslist $
    map (\(p, m) -> printf "%s: \t%d '%s'"
        (show p) (Ruler.mark_rank m) (Ruler.mark_name m))
    pos_marks
