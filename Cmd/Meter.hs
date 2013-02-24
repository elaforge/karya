{- | Functions to construct meter rulers.

    A meter ruler divides up a block analogous to a staff notation meter.  It's
    actually more general, since the meter just says how to divide up a single
    measure, and only at one level, while the ruler has arbitrary divisions.
    However, in practice, it's convenient to use a similar organization to
    staff notation's meter.  So by convention the ranks are for section,
    measure, half note, etc., and "Cmd.TimeStep" uses these as mnemonics for
    the various ruler ranks it can snap to.

    However, rank 'r_2', which corresponds to TimeStep's @'h'@, doesn't
    necessarily correspond to a half note.  It actually corresponds to the
    division below the measure, which in 3+3/8 is a dotted quarter.  In the
    case of 2/4 it would be a quarter note, but to keep the mnemonic names from
    getting too far from their staff notation counterparts I just skip a rank
    so that 'r_1' and 'r_2' both correspond to the same amount of time.
-}
module Cmd.Meter where
import Prelude hiding (repeat)
import qualified Data.List as List
import qualified Data.Map as Map

import Util.Control
import qualified Util.Seq as Seq
import qualified Ui.Color as Color
import qualified Ui.Ruler as Ruler
import qualified Ui.ScoreTime as ScoreTime

import Types


-- * meter marklist

-- | The meter marklist by convention has marks corresponding to the meter of
-- the piece.  Other commands may use this to find out where beats are.
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
color1, color2 :: Double -> Double -> Double -> Color.Color
color1 r g b = Color.rgba r g b 0.5
color2 r g b = Color.rgba r g b 0.3

-- | Configs for marks in order of increasing rank.
-- @(color, width, zoom_pixels)@
--
-- @zoom_pixels@ is how many pixels of space a mark at this rank must have
-- between its neighbors before it appears.
meter_ranks :: [(Color.Color, Int, Int)]
meter_ranks =
    [ (color1 0.0 0.0 0.0, 3, 8)    -- section
    , (color1 0.4 0.3 0.0, 2, 8)    -- measure / whole

    , (color1 1.0 0.4 0.2, 2, 8)    -- half
    , (color2 1.0 0.4 0.2, 2, 8)    -- quarter

    , (color1 1.0 0.2 0.7, 1, 8)    -- 8th
    , (color2 1.0 0.2 0.7, 1, 8)    -- 16th

    , (color1 0.1 0.5 0.1, 1, 8)    -- 32nd
    , (color2 0.1 0.5 0.1, 1, 8)    -- 64th

    , (color1 0.0 0.0 0.0, 1, 8)    -- 128th
    , (color2 0.0 0.0 0.0, 1, 8)    -- 256th
    ]

-- | These are the conventional meanings for the ranks.
r_section, r_1, r_2, r_4, r_8, r_16, r_32, r_64, r_128, r_256 :: Ruler.Rank
r_section : r_1 : r_2 : r_4 : r_8 : r_16 : r_32 : r_64 : r_128 : r_256 : _ =
  [0..]

-- | By convention, ranks divide up the ruler by dividing it by two for each
-- rank.  This is convenient because that's how staff notation works.  But then
-- the labels wind up being all 0s and 1s, which is not that useful.  The ranks
-- in this list don't receive their own label.
unlabelled_ranks :: [Ruler.Rank]
unlabelled_ranks = List.sort [r_2, r_8, r_32, r_64, r_256]

rank_names :: [(Ruler.Rank, String)]
rank_names = zip [0..]
    ["section", "w", "h", "q", "e", "s", "32", "64", "128", "256"]

rank_to_pixels :: [Int]
rank_to_pixels = [pixels | (_, _, pixels) <- meter_ranks]

-- ** construct meters

-- | An AbstractMeter is a structured description of how a unit of time is
-- broken up into hiererchical sections.  A 'T' represents a mark with the
-- unit duration, and a 'D' is a group of Meters.  The rank of each mark is
-- determined by its nesting depth.
--
-- Previously a 'T' could take a duration, but I didn't wind up using that
-- feature, so I removed it.  So meters have to be built of multiples of a unit
-- duration multiplied by some stretch factor.
--
-- An AbstractMeter can be created either by declaring it outright, or by
-- declaring a simpler AbstractMeter and subdividing or repeating it.
data AbstractMeter = T | D [AbstractMeter]
    deriving (Eq, Show)

-- | Subdivide each mark into the given number @D@s.  The duration of each
-- mark is lost as each one is unconditionally replaced with the given number
-- of bars.  This has the effect of putting one layer of subdivision under
-- the current structure, provided all @T@s are 1.
subdivide :: Int -> AbstractMeter -> AbstractMeter
subdivide n = replace_t (D (replicate n T))

subdivides :: [Int] -> AbstractMeter -> AbstractMeter
subdivides divs meter = foldr subdivide meter (reverse divs)

-- | Create a layer that repeats the given meter a certain number of times.
repeat :: Int -> AbstractMeter -> AbstractMeter
repeat n meter = D $ replicate n meter

repeats :: [Int] -> AbstractMeter -> AbstractMeter
repeats ns meter = foldr repeat meter ns

-- | Form a meter based on regular subdivision.  E.g. [4, 4] is 4 groups of 4,
-- [3, 3] is like 9\/8, and [4, 3] is 4 groups of 3 (12\/8).
regular_subdivision :: [Int] -> AbstractMeter
    -- It's most natural to think of the list as big divisions on the left to
    -- small divisions on the right, so reverse the list.
regular_subdivision ns = foldr subdivide T (reverse ns)

-- ** predefined meters

-- measures/section, half/measure, quarter/half, ...
-- These use 1s to help keep the timestep mnemonics in sync with staff notation
-- durations, as documented in the module haddock.
m54, m44, m34 :: AbstractMeter
m54 = regular_subdivision [4, 5, 2, 2, 2, 2]
m44 = regular_subdivision [4, 2, 2, 2, 2, 2]
m34 = regular_subdivision [4, 3, 2, 2, 2, 2]
m24 = regular_subdivision [8, 1, 2, 2, 2, 2]

m3p3p2_8 :: AbstractMeter
m3p3p2_8 = repeats [4, 1] $ subdivides [2, 2, 2, 2] $
    D [D [T, T, T], D [T, T, T], D [T, T]]

-- | 2+2+2 / 8, 4 quarters per measure
m2p2p2_8 :: AbstractMeter
m2p2p2_8 = regular_subdivision [4, 1, 3, 2, 2, 2, 2]

-- | 3+3 / 8, 2 dotted quarters per measure
m3p3_8 :: AbstractMeter
m3p3_8 = regular_subdivision [4, 1, 2, 3, 2, 2, 2]

-- *** AbstractMeter utils

-- | It's easier to visualize a meter as a list of its ranks.
mshow :: [AbstractMeter] -> [Ruler.Rank]
mshow = map fst . make_meter 1

-- | Map the given function over all @T@s in the given AbstractMeter.
replace_t :: AbstractMeter -> AbstractMeter -> AbstractMeter
replace_t val (D ts) = D (map (replace_t val) ts)
replace_t val T = val

meter_length :: AbstractMeter -> ScoreTime
meter_length (D ms) = sum (map meter_length ms)
meter_length T = 1


-- ** meter implementation

-- | Convert AbstractMeters into a Meter.  The AbstractMeters are concatenated,
-- and each one defines a rank 0.
make_meter :: ScoreTime -> [AbstractMeter] -> Meter
make_meter stretch meters = group0 marks
    where
    marks = concatMap (convert 0) meters
    -- Convert returns an intermediate format where all the ranks coexist at
    -- the same time, by giving them 0 dur.
    group0 dur_rank = case span ((==0) . snd) dur_rank of
        (zeros, (rank, dur) : rest) ->
            (minimum (rank : map fst zeros), dur) : group0 rest
        (_, []) -> []
    convert rank T = [(rank, stretch)]
    convert rank (D m) = (rank, 0) : concatMap (convert (rank+1)) m

-- | Like 'make_meter', but stretch the meter to fit in the given duration.
fit_meter :: ScoreTime -> [AbstractMeter] -> Meter
fit_meter dur meters = make_meter stretch meters
    where stretch = dur / sum (map meter_length meters)

make_marklist :: ScoreTime -> [AbstractMeter] -> Ruler.Marklist
make_marklist stretch = meter_marklist . make_meter stretch

meter_marklist :: Meter -> Ruler.Marklist
meter_marklist meter_ = Ruler.marklist (Map.fromList pos_marks)
    where
    pos_marks =
        [(pos, mark rank_dur rank name) | ((rank, _), pos, rank_dur, name)
            <- List.zip4 meter mark_pos (rank_durs meter)
                (ranks_to_labels (collapse_ranks_for_labels (map fst meter)))]
    -- By convention, the block ends with a rank 0 mark.
    meter = meter_ ++ [(0, 0)]
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

collapse_ranks_for_labels :: [Ruler.Rank] -> [Ruler.Rank]
collapse_ranks_for_labels = map (\r -> r - sub r)
    where sub r = length (takeWhile (<r) unlabelled_ranks)

-- | Name the MarkRanks in a #.#.# format.
--
-- TODO starts at 0, but maybe I should start at 1?
ranks_to_labels :: [Ruler.Rank] -> [String]
ranks_to_labels = map (Seq.join "." . map show . reverse)
    . snd .  List.mapAccumL mkname (-1, [])
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
