-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{- | Functions to construct meter rulers.

    A meter ruler divides up a block analogous to a staff notation meter.  It's
    actually more general, since the meter just says how to divide up a single
    measure, and only at one level, while the ruler has arbitrary divisions.
    However, in practice, it's convenient to use a similar organization to
    staff notation's meter.  So by convention the ranks are for section,
    measure, half note, etc., and "Cmd.TimeStep" uses abbreviated mnemonics of
    these durations for the various ruler ranks it can snap to.

    However, rank 'r_2', which corresponds to TimeStep's @'h'@, doesn't
    necessarily correspond to a half note.  It actually corresponds to the
    division below the measure, which in 3+3/8 is a dotted quarter.  In the
    case of 2/4 it would be a quarter note, but to keep the mnemonic names from
    getting too far from their staff notation counterparts, the 2/4 meter
    should skip a rank so that 'r_1' and 'r_2' both correspond to the same
    amount of time.
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

-- | If a meter's labels are just number counts, they can be generated from
-- a 'Meter'.  However, if they have a more complicated structure, as with
-- "Cmd.Tala", I can't actually generate new meter without knowing the rules.
-- That means generic modification is not possible, unless it's just deleting
-- things (and even then it may be wrong if it should have renumbered).
--
-- I considered some schemes for a mid-level representation, but eventually
-- decided that they were all too complicated, and if you want to modify
-- a complicated ruler, you should just recreate it from scratch, presumably
-- using whatever mid-level representation was appropriate for the function
-- that created it in the first place.
--
-- Except that deletion can still be implemented generically, and 'clip' is
-- still very useful.  So modification works on a Meterlike, and the delete
-- functions can work directly on a LabaledMeter, while the rest work on
-- a Meter.  This means they will renumber and thus mess up fancy meters like
-- talas.
--
-- An alternate approach, perhaps suitable for colotomic patterns, would be
-- a separate Marklist, but I'd run into the same modification problems.  I'll
-- probably have to come up with a better solution once I have more experience
-- with what operations I'll want to perform.
class Meterlike a where
    modify_meterlike :: (a -> a) -> Ruler.Marklist -> Ruler.Marklist

instance Meterlike Meter where
    modify_meterlike f = meter_marklist . f . marklist_meter

instance Meterlike LabeledMeter where
    modify_meterlike f = labeled_marklist . f . marklist_labeled

type Meter = [(Ruler.Rank, Duration)]
type LabeledMeter = [(Ruler.Rank, Duration, Label)]

-- | The durations in a 'Meter' are summed together, which leads to
-- accumulation of imprecision.  This means meters with unrepresentable
-- fractions will get increasingly inaccurate.  To counteract I use
-- 'ScoreTime.round' as "Ui.Events" does to round events to a likely rational
-- location.
--
-- Previously I used a rational, but it seems to be just as good to round
-- after every addition.
type Duration = ScoreTime

time_to_duration :: ScoreTime -> Duration
time_to_duration = id

meter_durations :: LabeledMeter -> [Duration]
meter_durations = scanl (+) 0 . map (\(_, d, _) -> d)

modify_meter :: (Meterlike m) => (m -> m) -> Ruler.Ruler -> Ruler.Ruler
modify_meter f = Ruler.modify_marklist Ruler.meter (modify_meterlike f)

ruler_meter :: Ruler.Ruler -> Meter
ruler_meter = marklist_meter . Ruler.get_marklist Ruler.meter

-- | Extract the half-open range from start to end.
clip :: Duration -> Duration -> LabeledMeter -> LabeledMeter
clip start end meter =
    map snd $ takeWhile ((<=end) . fst) $ dropWhile ((<start) . fst) $
        zip (meter_durations meter) meter

-- | Ack.  The Meter \/ LabeledMeter division is not very ideal.
clipm :: Duration -> Duration -> Meter -> Meter
clipm start end meter =
    map snd $ takeWhile ((<end) . fst) $ dropWhile ((<start) . fst) $
        zip (scanl (+) 0 (map snd meter)) meter

-- | Remove the half-open range.
delete :: Duration -> Duration -> LabeledMeter -> LabeledMeter
delete start end meter = map snd pre ++ map snd post
    where
    (pre, within) = break ((>=start) . fst) (zip (meter_durations meter) meter)
    post = dropWhile ((<end) . fst) within

scale :: Duration -> Meter -> Meter
scale dur meter = map (second (*factor)) meter
    where factor = if dur == 0 then 1 else dur / time_end meter

time_end :: Meter -> Duration
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

-- | These are mnemonics for staff notation durations, though they may not
-- correspond exactly, as documented in "Cmd.Meter".
rank_names :: [(Ruler.Rank, Text)]
rank_names = zip [0..] (map (Text.toLower . showt) [Section ..])

rank_to_pixels :: [Int]
rank_to_pixels = [pixels | (_, _, pixels) <- meter_ranks]

data RankName = Section | W | H | Q | E | S | T32 | T64 | T128 | T256
    deriving (Show, Eq, Ord, Bounded, Enum)

name_to_rank :: RankName -> Ruler.Rank
name_to_rank = fromEnum

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

meter_length :: AbstractMeter -> Duration
meter_length (D ms) = sum (map meter_length ms)
meter_length T = 1


-- ** meter implementation

-- | Convert AbstractMeters into a Meter.  The AbstractMeters are concatenated,
-- and each one defines a rank 0.
make_meter :: Duration -> [AbstractMeter] -> Meter
make_meter stretch meters = group0 marks
    where
    marks = concatMap (convert 0) meters
    -- Convert returns an intermediate format where all the ranks coexist at
    -- the same time, by giving them 0 dur.
    group0 dur_rank = case span ((==0) . snd) dur_rank of
        (zeros, (rank, dur) : rest) ->
            (minimum (rank : map fst zeros), dur) : group0 rest
        (_, []) -> [(0, 0)]
    convert rank T = [(rank, stretch)]
    convert rank (D m) = (rank, 0) : concatMap (convert (rank+1)) m

-- | Like 'make_meter', but stretch the meter to fit in the given duration.
fit_meter :: Duration -> [AbstractMeter] -> Meter
fit_meter dur meters = make_meter stretch meters
    where stretch = dur / sum (map meter_length meters)

-- ** marklist conversion

-- | Convert a Meter into a Marklist using the default labels.
meter_marklist :: Meter -> Ruler.Marklist
meter_marklist = labeled_marklist . label_meter

marklist_meter :: Ruler.Marklist -> Meter
marklist_meter = map (\(rank, dur, _) -> (rank, dur)) . marklist_labeled

label_meter :: Meter -> LabeledMeter
label_meter meter = List.zip3 ranks ps labels
    where
    (ranks, ps) = unzip meter
    labels = text_labels 1 count1_labels $
        collapse_ranks unlabelled_ranks ranks

-- | Create a Marklist from a labelled Meter.
labeled_marklist :: LabeledMeter -> Ruler.Marklist
labeled_marklist meter = Ruler.marklist
    [ (ScoreTime.round pos, mark is_edge dur rank label)
    | (rank, pos, label, dur, is_edge)
        <- List.zip5 ranks (scanl ((+) . ScoreTime.round) 0 ps)
            labels durs edges
    ]
    -- Round at every step to avoid accumulating error, as per 'Duration'.
    where
    edges = True : map null (drop 2 (List.tails ranks))
    durs = rank_durs (zip ranks ps)
    (ranks, ps, labels) = List.unzip3 meter
    mark is_edge rank_dur rank name =
        let (color, width, pixels) = meter_ranks !! min rank ranks_len
            zoom = pixels_to_zoom rank_dur pixels
        in Ruler.Mark
            { Ruler.mark_rank = rank
            , Ruler.mark_width = width
            , Ruler.mark_color = color
            , Ruler.mark_name = name
            , Ruler.mark_name_zoom_level = if is_edge then 0 else zoom * 2
            , Ruler.mark_zoom_level = if is_edge then 0 else zoom
            }
    ranks_len = length meter_ranks

-- | The last mark gets a 0 duration.
marklist_labeled :: Ruler.Marklist -> LabeledMeter
marklist_labeled mlist =
    [ (Ruler.mark_rank m, maybe 0 (subtract p . fst) maybe_next,
        Ruler.mark_name m)
    | ((p, m), maybe_next) <- Seq.zip_next marks
    ]
    where marks = Ruler.ascending 0 mlist


-- *** implementation

count0 :: [Label]
count0 = map showt [0..]

count1 :: [Label]
count1 = drop 1 count0

count0_labels :: [[Label]]
count0_labels = List.repeat count0

count1_labels :: [[Label]]
count1_labels = List.repeat count1

-- | The rank duration is the duration until the next mark of equal or greater
-- (lower) rank.
rank_durs :: Meter -> [Duration]
rank_durs = map rank_dur . List.tails
    where
    rank_dur [] = 0
    rank_dur ((rank, dur) : meter) = total
        where total = dur + sum (map snd (takeWhile ((>rank) . fst) meter))

-- | Given a mark duration and the number of pixels it needs to display,
-- return the appropriate zoom factor.
pixels_to_zoom :: Duration -> Int -> Double
pixels_to_zoom dur pixels
    | dur == 0 = 0
    | otherwise = fromIntegral pixels / ScoreTime.to_double dur

-- * labels

type Label = Text

data Labelled =
    -- | The mark gets this text.
    Literal Label
    -- | The mark gets a number
    | Count deriving (Show)

type LabelledMeter = [(Ruler.Rank, ScoreTime, Labelled)]

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
    map (Text.intercalate "." . strip) . Seq.zip_prev
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
