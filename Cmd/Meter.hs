-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

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
import qualified Data.Text as Text

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
unlabelled_ranks = [r_2, r_8, r_32, r_64, r_256]

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

-- | Subdivide each mark into the given number @D@s.  This has the effect of
-- putting one layer of subdivision under the current structure.
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

-- | Convert a Meter into a Marklist using the default labels.
meter_marklist :: Meter -> Ruler.Marklist
meter_marklist = meter_marklist_labels
    (text_labels 1 count1_labels . collapse_ranks unlabelled_ranks)

count0 :: [Label]
count0 = map showt [0..]

count1 :: [Label]
count1 = drop 1 count0

count0_labels :: [[Label]]
count0_labels = List.repeat count0

count1_labels :: [[Label]]
count1_labels = List.repeat count1

-- | Convert a Meter into a Marklist, using labels provided by the function,
-- which is probably 'text_labels'.
meter_marklist_labels :: ([Ruler.Rank] -> [Text]) -> Meter -> Ruler.Marklist
meter_marklist_labels make_labels meter_ = Ruler.marklist
    [(pos, mark rank_dur rank name) | ((rank, _), pos, rank_dur, name)
        <- List.zip4 meter mark_pos (rank_durs meter)
            (make_labels (map fst meter))]
    where
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

-- | Given a mark duration and the number of pixels it needs to display,
-- return the appropriate zoom factor.
pixels_to_zoom :: ScoreTime -> Int -> Double
pixels_to_zoom dur pixels
    | dur == 0 = 0
    | otherwise = fromIntegral pixels / ScoreTime.to_double dur

marklist_meter :: Ruler.Marklist -> Meter
marklist_meter mlist = zipWith to_dur marks (drop 1 marks)
    where
    marks = Ruler.ascending 0 mlist
    -- This drops the last mark, but by convention that's a rank 0 mark intended
    -- to delemit the duration of the second-to-last mark.
    to_dur (p, m) (next_p, _) = (Ruler.mark_rank m, next_p - p)

-- * labels

type Label = Text

text_labels :: Int -> [[Label]] -> [Ruler.Rank] -> [Label]
text_labels min_depth labels ranks =
    map (Text.intercalate ".") $ strip $ map (map replace) $
        apply_labels labels ranks
    where
    strip = zipWith take (map (max min_depth . (+1)) ranks)
    replace t = if Text.null t then "-" else t

-- | The ruler gets cluttered if I label every single rank, so combine the ones
-- in the given list with the following rank.
collapse_ranks :: [Ruler.Rank] -> [Ruler.Rank] -> [Ruler.Rank]
collapse_ranks omit = map (\r -> r - sub r)
    where sub r = length (takeWhile (<r) omit)

-- | When labels are created, many of them have the same components as the
-- previous label, e.g. @1.1.1@, @1.1.2@.  Replace the identical components
-- with a placeholder to make the difference more apparent: @1.1.1@, @-.-.2@.
--
-- This doesn't actually look that nice on the UI because it does it for all
-- labels, not just the visible ones.
strip_prefixes :: Text -> [Label] -> [Label]
strip_prefixes replacement =
    map (Text.intercalate ".") . map strip . Seq.zip_prev
        . map (Text.splitOn ".")
    where
    strip (prev, cur) =
        [ if Just c == mp then replacement else c
        | (c, mp) <- Seq.zip_padded2 cur (fromMaybe [] prev)
        ]

-- | Apply the labels according to the ranks.  Each Rank input has
-- a corresponding @[Label]@ output.  Each rank advances the label at the rank's
-- index in the labels, and resets all the labels beneath it.  If a rank runs
-- out of labels, @\"\"@ is emitted.
--
-- The first rank doesn't matter since it always emits the initial state of the
-- labels.
apply_labels :: [[Label]] -> [Ruler.Rank] -> [[Label]]
apply_labels labels =
    (map hd labels :) . snd . List.mapAccumL mk labels . drop 1
    where
    mk state rank = (next, map hd next)
        where next = split rank state
    split rank state = above ++ cur : drop (rank + 1) labels
        where
        (above, below) = splitAt rank state
        cur = case below of
            (_ : cur@(_:_)) : _ -> cur
            _ -> [""]
    hd [] = ""
    hd (x:_) = x
