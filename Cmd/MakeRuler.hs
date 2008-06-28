{- | Functions to construct and modify rulers and their marklists.

Since having a single mark at the beginning of the block isn't too useful,
I use a "zero" config for that, and then replace the first mark with a mark
from the first config.

-- a block with 8 measures, each in 4/4
make_meter (TrackPos 1024) [8, 4, 4, 4]

-- a block with 8 measures, each in 3/4
[8, 3, 4]

-}
module Cmd.MakeRuler where
import Prelude hiding (repeat)
import Data.Function
import qualified Data.List as List
import Data.Ratio

import qualified Util.Seq as Seq
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

-- | The mark color defaults to mostly transparent so it looks nice on overlay
-- rulers.
mcolor r g b = Color.rgba r g b 0.35

-- | Configs for marks in order of increasing rank.
-- (color, width, zoom_pixels)
--
-- @zoom_pixels@ is how many pixels of space a mark at this rank must have
-- between its neighbors before it appears.
meter_ranks :: [(Color.Color, Int, Int)]
meter_ranks =
    [ (mcolor 0.3 0.3 0.3, 3, 0)    -- block begin and end
    , (mcolor 0.0 0.0 0.0, 3, 8)    -- block section
    , (mcolor 0.4 0.3 0.0, 2, 8)    -- whole
    , (mcolor 1.0 0.4 0.2, 2, 8)    -- quarter
    , (mcolor 1.0 0.2 0.7, 1, 8)    -- 16th
    , (mcolor 0.1 0.5 0.1, 1, 8)    -- 64th
    , (mcolor 0.0 0.0 0.0, 1, 8)    -- 256th
    ]

-- * constructors

-- | Constructor for "plain" rulers.
ruler :: [Ruler.NameMarklist] -> Ruler.Ruler
ruler marklists = Ruler.ruler marklists Config.ruler_bg True False False

-- | Convert a ruler to be suitable as an overlay ruler.
as_overlay :: Ruler.Ruler -> Ruler.Ruler
as_overlay ruler = ruler
    { Ruler.ruler_show_names = False
    , Ruler.ruler_use_alpha = True
    , Ruler.ruler_full_width = True
    }

-- ** construct meters

-- | Convert the given meter into a \"meter\" marklist.  The mark positions
-- are multiplied by @mult@.
meter_ruler :: Double -> Meter -> Ruler.NameMarklist
meter_ruler mult meter = marks_to_ruler (meter_marks mult meter)

-- | A Meter is a structured description of how a unit of time is broken up
-- into hiererchical sections.  A 'T' represents a mark with the given
-- duration, and a 'D' is a group of Meters.  The rank of each mark is
-- determined by its nesting depth.
--
-- A Meter can be created either by declaring it outright, or by declaring
-- a simpler Meter and further subdividing it.
data Meter = T (Ratio Integer) | D [Meter]
    deriving (Eq, Show)

replace_t :: (Ratio Integer -> Meter) -> Meter -> Meter
replace_t f (D ts) = D (map (replace_t f) ts)
replace_t f (T x) = f x

subdivide :: Int -> Meter -> Meter
subdivide n = replace_t (const (D (replicate n (T 1))))

subdivide_dur :: Meter -> Meter
subdivide_dur = replace_t (\n -> (D (replicate (floor n) (T 1))))

repeat :: Int -> Meter -> Meter
repeat n meter = D $ replicate n meter

-- | Form a meter based on regular subdivision.  E.g. [4, 4] is 4 groups of 4,
-- and [3, 3] is like 9/8.
regular_subdivision :: [Int] -> Meter
regular_subdivision ns = foldr subdivide (T 1) ns

-- ** predefined meters

m44 = regular_subdivision [4, 4, 4, 4]
m332 = repeat 4 $ subdivide 4 $ subdivide_dur $ D (map T [3, 3, 2])

mshow = map snd . meter_marks 1


-- ** meter implementation

-- | Simplified description of a mark with just (time, rank).
type MarkRank = (TrackPos, Int)

-- | Convert a Meter into [MarkRank], which can later be turned into [PosMark].
meter_marks :: Double -> Meter -> [MarkRank]
meter_marks mult meter = map minimum $ List.groupBy ((==) `on` fst) marks
    where marks = dur_to_pos $ convert_meter 0 (map_t (* realToFrac mult) meter)

convert_meter rank (T v) = [(realToFrac v, rank)]
convert_meter rank (D meter) =
    (0, rank) : concatMap (convert_meter (rank+1)) meter

map_t f = replace_t (T . f)

-- | Convert marks with time durations to ones with absolute times.
-- A rank 0 mark will be appended to mark the end of the meter.
dur_to_pos :: [MarkRank] -> [MarkRank]
dur_to_pos marks = timed ++ [(final, 0)]
    where
    (final, timed) =
        List.mapAccumL (\pos (d, rank) -> (pos+d, (pos, rank))) 0 marks

marks_to_ruler :: [MarkRank] -> Ruler.NameMarklist
marks_to_ruler marks = Ruler.marklist meter_marklist pos_marks
    where
    pos_marks = [(track_pos pos, mark dur rank name)
        | ((pos, rank), dur, name) <- zip3 marks durs (mark_names marks)]
    -- I know I just converted dur_to_pos, but go back to dur again, now that
    -- they're at their final positions.
    durs = mark_durs marks
    mark dur rank name =
        let (color, width, pixels) = meter_ranks !! (min rank ranks)
            zoom = if dur == 0
                then 0
                else fromIntegral pixels / realToFrac dur
        in Ruler.Mark rank width color name (zoom*2) zoom
    ranks = length meter_ranks

mark_durs = map mark_dur . List.tails
-- | The duration of a mark is the distance until the next mark of equal or
-- greater (lower) rank.
mark_dur :: [MarkRank] -> TrackPos
mark_dur [] = TrackPos 0
mark_dur ((pos, rank) : marks) = next_pos - pos
    where (next_pos, _) = maybe (0, 0) id (List.find ((<=rank) . snd) marks)


mark_names :: [MarkRank] -> [String]
mark_names marks = map (Seq.join "." . map show . drop 1 . reverse) $ snd $
    List.mapAccumL mkname (-1, []) marks
    where
    mkname (prev_rank, prev_path) (_pos, rank) = ((rank, path), path)
        where
        path
            | rank > prev_rank =
                (if null prev_path then 0 else 1) : zeros ++ prev_path
            | otherwise = inc $ drop (prev_rank - rank) prev_path
        zeros = replicate (rank - prev_rank - 1) 0
        inc xs = case xs of
            (x:xs) -> x+1 : xs
            [] -> []



pretty_marks pos_marks = pslist $
    map (\(p, m) -> printf "%s: \t%d '%s'"
        (show p) (Ruler.mark_rank m) (Ruler.mark_name m))
    pos_marks
