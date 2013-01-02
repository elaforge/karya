{- | Functions to construct meter rulers.

    - a block with 8 measures, each in 4/4

    > 'make_meter' 1024 [8, 4, 4, 4]
-}
module Cmd.Meter where
import Prelude hiding (repeat)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Ratio

import Util.Control
import qualified Util.Seq as Seq
import qualified Ui.Color as Color
import qualified Ui.Ruler as Ruler
import qualified Ui.ScoreTime as ScoreTime

import Types


-- * meter marklist

-- | The meter marklist by convention has marks corresponding to the meter of
-- the piece.  Other commands may use this to find out where beats are.
-- By convention, this marklist starts at rank 1 and goes up.  This is so that
-- rank 0 can be used for special cues and whatnot.
meter :: Ruler.Name
meter = "meter"

type Meter = [(Ruler.Rank, ScoreTime)] -- (rank, duration)

modify_meter :: (Meter -> Meter) -> Ruler.Ruler -> Ruler.Ruler
modify_meter f = Ruler.modify_marklist meter $
    meter_marklist . f . marklist_meter

ruler_meter :: Ruler.Ruler -> Meter
ruler_meter = marklist_meter . Ruler.get_marklist meter

-- | Extract the helf-open range from start to end.
clip :: ScoreTime -> ScoreTime -> Meter -> Meter
clip start end meter =
    map snd $ takeWhile ((<end) . fst) $ dropWhile ((<start) . fst) $
        zip durs meter
    where durs = scanl (+) 0 (map snd meter)

-- | Remove the half-open range.
remove :: ScoreTime -> ScoreTime -> Meter -> Meter
remove start end meter = map snd pre ++ map snd post
    where
    (pre, within) = break ((>=start) . fst) (zip durs meter)
    post = dropWhile ((<end) . fst) within
    durs = scanl (+) 0 (map snd meter)

scale :: ScoreTime -> Meter -> Meter
scale dur meter = map (second (*factor)) meter
    where factor = if dur == 0 then 1 else dur / time_end meter

time_end :: Meter -> ScoreTime
time_end = sum . map snd


-- ** meter constants

-- | The mark color defaults to mostly transparent so it looks nice on overlay
-- rulers.
mcolor :: Double -> Double -> Double -> Color.Color
mcolor r g b = Color.rgba r g b 0.35

-- | Configs for marks in order of increasing rank.
-- @(color, width, zoom_pixels)@
--
-- @zoom_pixels@ is how many pixels of space a mark at this rank must have
-- between its neighbors before it appears.
meter_ranks :: [(Color.Color, Int, Int)]
meter_ranks =
    [ (mcolor 0.3 0.3 0.3, 3, 0)    -- block begin and end
    , (mcolor 0.0 0.0 0.0, 3, 8)    -- block section
    , (mcolor 0.4 0.3 0.0, 2, 8)    -- measure / whole
    , (mcolor 1.0 0.4 0.2, 2, 8)    -- quarter
    , (mcolor 1.0 0.2 0.7, 1, 8)    -- 16th
    , (mcolor 0.1 0.5 0.1, 1, 8)    -- 64th
    , (mcolor 0.0 0.0 0.0, 1, 8)    -- 256th
    ]

-- | These are the conventional meanings for the ranks.
r_block, r_section, r_1, r_4, r_16, r_64, r_256 :: Ruler.Rank
r_block : r_section : r_1 : r_4 : r_16 : r_64 : r_256 : _ = [0..]

rank_to_pixels :: [Int]
rank_to_pixels = [pixels | (_, _, pixels) <- meter_ranks]

-- ** construct meters

-- | An AbstractMeter is a structured description of how a unit of time is
-- broken up into hiererchical sections.  A 'T' represents a mark with the
-- given duration, and a 'D' is a group of Meters.  The rank of each mark is
-- determined by its nesting depth.
--
-- The duration is represented as a Ratio so it remains independent of the
-- eventual block length.  It will be multiplied into an actual ScoreTime
-- by 'make_meter'.
--
-- An AbstractMeter can be created either by declaring it outright, or by
-- declaring a simpler AbstractMeter and further subdividing it.
data AbstractMeter = T (Ratio Integer) | D [AbstractMeter]
    deriving (Eq, Show)

-- | Subdivide each mark into the given number @D@s.  The duration of each
-- mark is lost as each one is unconditionally replaced with the given number
-- of bars.  This has the effect of putting one layer of subdivision under
-- the current structure, provided all @T@s are 1.
subdivide :: Int -> AbstractMeter -> AbstractMeter
subdivide n = replace_t (const (D (replicate n (T 1))))

-- | Subdivide each mark into a number of divisions equal to its whole number
-- value.  This has the effect of creating a layer of structure according to
-- the durations of the @T@s and reducing @T@s to duration 1.
subdivide_dur :: AbstractMeter -> AbstractMeter
subdivide_dur = replace_t (\n -> D (replicate (floor n) (T 1)))

-- | Create a layer that repeats the given meter a certain number of times.
repeat :: Int -> AbstractMeter -> AbstractMeter
repeat n meter = D $ replicate n meter

-- | Form a meter based on regular subdivision.  E.g. [4, 4] is 4 groups of 4,
-- [3, 3] is like 9\/8, and [4, 3] is 4 groups of 3 (12\/8).
regular_subdivision :: [Int] -> AbstractMeter
    -- It's most natural to think of the list as big divisions on the left to
    -- small divisions on the right, so reverse the list.
regular_subdivision ns = foldr subdivide (T 1) (reverse ns)

-- ** predefined meters

m54, m44, m34, m332 :: AbstractMeter
m54 = regular_subdivision [4, 5, 4, 4, 4]
m44 = regular_subdivision [4, 4, 4, 4, 4]
m34 = regular_subdivision [4, 3, 4, 4, 4]
m332 = repeat 4 $ subdivide 4 $ subdivide_dur $ D (map T [3, 3, 2])

-- | 4 sections / 4 measures / 6 8th notes / 2 16th notes / 4 64th notes
m68 :: AbstractMeter
m68 = regular_subdivision [4, 4, 6, 2, 4]

-- *** AbstractMeter utils

-- | It's easier to visualize a meter as a list of its ranks.
mshow :: AbstractMeter -> [Ruler.Rank]
mshow = map fst . make_meter 1

-- | Map the given function over all @T@s in the given AbstractMeter.
replace_t :: (Ratio Integer -> AbstractMeter) -> AbstractMeter -> AbstractMeter
replace_t f (D ts) = D (map (replace_t f) ts)
replace_t f (T x) = f x

meter_length :: AbstractMeter -> Ratio Integer
meter_length (D ms) = sum (map meter_length ms)
meter_length (T d) = d


-- ** meter implementation

-- | Convert an AbstractMeter into a Meter.
make_meter :: Double -> AbstractMeter -> Meter
make_meter stretch meter = group0 marks
    where
    marks = convert 0 (map_t (* realToFrac stretch) meter)
    -- Convert returns an intermediate format where all the ranks coexist at
    -- the same time, by giving them 0 dur.
    group0 dur_rank = case span ((==0) . snd) dur_rank of
        (zeros, (rank, dur) : rest) ->
            (minimum (rank : map fst zeros), dur) : group0 rest
        (_, []) -> []
    convert rank (T v) = [(rank, realToFrac v)]
    convert rank (D meter) = (rank, 0) : concatMap (convert (rank+1)) meter
    map_t :: (Ratio Integer -> Ratio Integer) -> AbstractMeter -> AbstractMeter
    map_t f = replace_t (T . f)

-- | Like 'make_meter', but stretch the meter to fit in the given duration.
fit_meter :: ScoreTime -> AbstractMeter -> Meter
fit_meter dur meter = make_meter stretch meter
    where stretch = ScoreTime.to_double dur / realToFrac (meter_length meter)

make_marklist :: Double -> AbstractMeter -> Ruler.Marklist
make_marklist stretch = meter_marklist . make_meter stretch

meter_marklist :: Meter -> Ruler.Marklist
meter_marklist meter_ = Ruler.marklist (Map.fromList pos_marks)
    where
    pos_marks =
        [(pos, mark rank_dur rank name) | ((rank, _), pos, rank_dur, name)
            <- List.zip4 meter mark_pos (rank_durs meter)
                (rank_names (map fst meter))]
    -- By convention, the block begins and ends with a rank 0 mark.
    meter = (++ [(0, 0)]) $ case meter_ of
        (_, d) : rest -> (0, d) : map (first (max 1)) rest
        [] -> []
    mark_pos = scanl (+) 0 (map snd meter)
    mark rank_dur rank name =
        let (color, width, pixels) = meter_ranks !! min rank ranks_len
            zoom = pixels_to_zoom rank_dur pixels
        in Ruler.Mark
            { Ruler.mark_rank = rank
            , Ruler.mark_width = width
            , Ruler.mark_color = color
            , Ruler.mark_name = name
            , Ruler.mark_name_zoom_level = zoom * 2
            , Ruler.mark_zoom_level = zoom
            }
    ranks_len = length meter_ranks

-- | The rank duration is the duration until the next mark of equal or greater
-- (lower) rank.
rank_durs :: Meter -> [ScoreTime]
rank_durs = map rank_dur . List.tails
    where
    rank_dur [] = 0
    rank_dur ((rank, dur) : meter) = total
        where total = dur + sum (map snd (takeWhile ((>rank) . fst) meter))

-- | Name the MarkRanks in a #.#.# format.
--
-- TODO starts at 0, but maybe I should start at 1?
rank_names :: [Ruler.Rank] -> [String]
rank_names = map (Seq.join "." . map show . drop 1 . reverse)
    . snd . List.mapAccumL mkname (-1, [])
    where
    mkname (prev_rank, prev_path) rank = ((rank, path), path)
        where
        path
            | rank > prev_rank =
                (if null prev_path then 0 else 1) : zeros ++ prev_path
            | otherwise = inc $ drop (prev_rank - rank) prev_path
        zeros = replicate (rank - prev_rank - 1) 0
        inc xs = case xs of
            (x:xs) -> x+1 : xs
            [] -> []

-- | Given a mark duration and the number of pixels it needs to display,
-- return the appropriate zoom factor.
pixels_to_zoom :: ScoreTime -> Int -> Double
pixels_to_zoom dur pixels
    | dur == 0 = 0
    | otherwise = fromIntegral pixels / ScoreTime.to_double dur

marklist_meter :: Ruler.Marklist -> Meter
marklist_meter mlist = zipWith to_dur marks (drop 1 marks)
    where
    marks = Map.toAscList $ Ruler.marklist_map mlist
    -- This drops the last mark, but by convention that's a rank 0 mark intended
    -- to delemit the duration of the second-to-last mark.
    to_dur (p, m) (next_p, _) = (Ruler.mark_rank m, next_p - p)

-- -- | If a marklist has been stretched, the zoom values will need to be
-- -- recalculated.
-- recalculate_zoom :: Ruler.Marklist -> Ruler.Marklist
-- recalculate_zoom mlist =
--     Ruler.marklist [(pos, recalc dur m) | ((pos, m), dur) <- zip marks durs]
--     where
--     recalc dur mark = mark
--         { Ruler.mark_name_zoom_level = zoom * 2
--         , Ruler.mark_zoom_level = zoom
--         }
--         where
--         zoom = pixels_to_zoom dur $ fromMaybe 0 $
--             Seq.at rank_to_pixels (Ruler.mark_rank mark)
--     marks = Ruler.marks_of mlist
--     durs = mark_durs [(pos, Ruler.mark_rank m) | (pos, m) <- marks]
