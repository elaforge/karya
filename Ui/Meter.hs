-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
module Ui.Meter (
    Meter(..)
    , make_marklist
#ifdef TESTING
    , module Ui.Meter
#endif
) where
import qualified Prelude
import           Prelude hiding (repeat)
import qualified Data.List as List
import qualified Data.Ratio as Ratio
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified GHC.Generics as Generics

import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Util.Regex as Regex
import qualified Util.Seq as Seq
import qualified Util.Then as Then

import qualified Ui.Color as Color
import qualified Ui.Ruler as Ruler
import qualified Ui.ScoreTime as ScoreTime

import           Global
import           Types

-- This should replace Ruler.MeterConfig
data Meter =
    Marklist Ruler.Marklist
    -- | Each meter starts at the time and repeats until the next one.
    -- invariant: ends always get bigger
    | Measures !Config ![(MeasureCount, Duration, AbstractMeter)]
    deriving (Show)

type Marklist = [(TrackTime, Ruler.Mark)]
type MeasureCount = Int

-- * make

-- If I port this to c++, then I can just marshal Config instead of the vector.
make_marklist :: Meter -> Ruler.Marklist
make_marklist = \case
    Marklist v -> v
    Measures config measures -> Ruler.marklist $ make_measures config measures

-- | Repeat each measure for the given count.
-- This can't express unaligned beats.  If you want a cut-off measure
-- like 4/4 - 1/16, you would have to create a special measure for it.
-- If I want that, I can make a function on AbstractMeter.
make_measures :: Config -> [(MeasureCount, Duration, AbstractMeter)]
    -> Marklist
make_measures config =
    labeled_marklist 0 . label_ranks config (config_start_measure config)
    . to_rank_durations . map expand
    where
    expand (count, dur, meter) = (tdur, D (replicate count meter))
        where !tdur = dur / meter_length meter

-- | The AbstractMeters are concatenated, and each one defines a rank 0.
-- Each T level gets the given Duration.
to_rank_durations :: [(Duration, AbstractMeter)] -> [(Ruler.Rank, Duration)]
to_rank_durations = group0 . concatMap (uncurry (convert 0))
    where
    -- Convert returns an intermediate format where all the ranks coexist at
    -- the same time, by giving them 0 dur.
    group0 dur_rank = case span ((==0) . snd) dur_rank of
        (zeros, (rank, dur) : rest) ->
            (minimum (rank : map fst zeros), dur) : group0 rest
        (_, []) -> [(0, 0)]
    convert rank dur meter = case meter of
        T -> [(rank, dur)]
        D ms -> (rank, 0) : concatMap (convert (rank+1) dur) ms

{-
data Measure = Measure {
    -- A single measure.
    measure_meter :: !AbstractMeter
    , measure_duration :: !Duration
    } deriving (Show)

-- say I want Section RankName for the measure changes, or every 4 measures
-- I should just have one big AbstractMeter.
-- Or compress it with [(Count, AbstractMeter)]
make_measures_old :: Config -> [(TrackTime, Measure)] -> [Marklist]
make_measures_old config =
    Then.mapAccumL make (0, config_start_measure config) done
    where
    make (start, start_measure) (end, measure) =
        ( (end, start_measure + ms)
        , make_measure config start_measure start end measure
        )
        where ms = floor $ (end - start) / measure_duration measure
    -- Append a final measure mark.
    done (start, start_measure) =
        [make_measure config start_measure start (start+1) (Measure T 1)]

-- | Repeat the Measure until the end time.
make_measure :: Config -> MeasureCount -> TrackTime -> TrackTime -> Measure
    -> Marklist
make_measure config start_measure start end (Measure meter measure_dur) =
    labeled_marklist start $ label_ranks config start_measure $ take marks $
        to_rank_duration stretch (D (Prelude.repeat meter))
    where
    marks = floor $ (end - start) / stretch
    stretch = measure_dur / meter_length meter

to_rank_duration :: Duration -> AbstractMeter -> [(Ruler.Rank, Duration)]
to_rank_duration stretch = group0 . convert 0
    where
    -- Convert returns an intermediate format where all the ranks coexist at
    -- the same time, by giving them 0 dur.
    group0 dur_rank = case span ((==0) . snd) dur_rank of
        (zeros, (rank, dur) : rest) ->
            (minimum (rank : map fst zeros), dur) : group0 rest
        (_, []) -> [(0, 0)]
    convert rank = \case
        T -> [(rank, stretch)]
        D ms -> (rank, 0) : concatMap (convert (rank+1)) ms
-}

data LabeledMark = LabeledMark {
    m_rank :: !Ruler.Rank
    , m_duration :: !Duration
    , m_label :: !Label
    } deriving (Eq, Show)

instance Pretty LabeledMark where
    pretty (LabeledMark rank dur label) = pretty (rank, dur, label)

-- | Add Labels to the given ruler ranks.
label_ranks :: Config -> MeasureCount -> [(Ruler.Rank, Duration)]
    -> [LabeledMark]
label_ranks config start_measure rank_durs =
    [ LabeledMark rank dur label
    | (rank, dur, label) <- List.zip3 ranks ps labels
    ]
    where
    (ranks, ps) = unzip (drop_0dur rank_durs)
    labels = map join_label $ strip_prefixes "" (config_strip_depth config) $
        convert_labels (config_min_depth config)
            (config_label_components config) start_measure
            (collapse_ranks unlabeled ranks)
    unlabeled = labeled_to_unlabeled_ranks (config_labeled_ranks config)
    -- Appending Meters can result in 0 dur marks in the middle.
    drop_0dur [] = []
    drop_0dur ((r, d) : rank_durs)
        | d == 0 && not (null rank_durs) = drop_0dur rank_durs
        | otherwise = (r, d) : drop_0dur rank_durs

labeled_to_unlabeled_ranks :: Set RankName -> [Ruler.Rank]
labeled_to_unlabeled_ranks labeled =
    [name_to_rank r | r <- all_ranks, not (r `Set.member` labeled)]

-- | Create a Marklist from a labeled Meter.
labeled_marklist :: TrackTime -> [LabeledMark] -> Marklist
labeled_marklist start marks =
    [ (realToFrac pos, mark is_edge dur rank label)
    | (rank, pos, label, dur, is_edge)
        <- List.zip5 ranks
            (scanl (+) (to_rational start)
                (map (to_rational . m_duration) marks))
            (map m_label marks) durs edges
    ]
    where
    -- Avoid accumulating error, as per 'Duration'.
    to_rational t = Ratio.approxRational t 0.0000001
    edges = True : map null (drop 2 (List.tails ranks))
    durs = rank_durs (zip ranks (map m_duration marks))
    ranks = map m_rank marks
    mark is_edge rank_dur rank name = Ruler.Mark
        { mark_rank = rank
        , mark_width = width
        , mark_color = color
        , mark_name = name
        , mark_name_zoom_level = zoom * 2
        , mark_zoom_level = zoom
        -- , mark_name_zoom_level = if is_edge then 0 else zoom * 2
        -- , mark_zoom_level = if is_edge then 0 else zoom
        }
        where
        (color, width, pixels) = meter_ranks !! min rank ranks_len
        zoom = pixels_to_zoom rank_dur pixels
    ranks_len = length meter_ranks

-- * config

data Config = Config {
    -- | Skip labels for these ranks.
    config_labeled_ranks :: !(Set RankName)
    -- | The convention is that the first two ranks, section and measure, are
    -- universal.  So this omits measure, which gets 'measure_labels', starting
    -- from 'config_start_measure'.
    , config_label_components :: !LabelComponents
    -- | The ruler should start counting at this number.  This could be measure
    -- number, or gong count, or avartanam count, whatever is the highest visual
    -- 'Label'.
    , config_start_measure :: !MeasureCount
    -- | Labels have at least this many sections.  Otherwise, trailing sections
    -- are omitted.
    , config_min_depth :: !Int
    -- | Strip leading prefixes to this depth, via 'strip_prefixes'.
    , config_strip_depth :: !Int
    -- | Key to 'Ruler.config_name'.
    , config_name :: !Text
    } deriving (Show, Generics.Generic)

instance Pretty Config where format = Pretty.formatG_

default_config :: Config
default_config = Config
    { config_labeled_ranks = default_labeled_ranks
    , config_label_components = big_number_components 1
    , config_start_measure = Ruler.config_start_measure Ruler.default_config
    , config_min_depth = 1
    , config_strip_depth = 2
    , config_name = Ruler.config_name Ruler.default_config
    }

-- ruler_config :: Config -> Ruler.MeterConfig
-- ruler_config config = Ruler.MeterConfig
--     { config_name = config_name config
--     , config_start_measure = config_start_measure config
--     }

-- TODO this type is shared with Derive, maybe it should go in its own module?
data RankName = Section | W | H | Q | E | S | T32 | T64 | T128 | T256
    deriving (Show, Eq, Ord, Bounded, Enum)
instance Pretty RankName where pretty = showt

all_ranks :: [RankName]
all_ranks = [minBound .. maxBound]

name_to_rank :: RankName -> Ruler.Rank
name_to_rank = fromEnum

-- * AbstractMeter

type Duration = TrackTime

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

-- | Map the given function over all @T@s in the given AbstractMeter.
replace_t :: AbstractMeter -> AbstractMeter -> AbstractMeter
replace_t val (D ts) = D (map (replace_t val) ts)
replace_t val T = val

meter_length :: AbstractMeter -> Duration
meter_length (D ms) = Num.sum (map meter_length ms)
meter_length T = 1

-- * labels

type Label = Text -- TODO make it ByteString so I can pass to c++ efficiently?

-- | This is the prototype for how to draw labels.  The outer list is indexed
-- by rank, while the inner is has the sequence of labels at that rank.
-- 'convert_labels' will take from a given level each time it sees that rank,
-- and reset back to the beginning when the rank becomes less than that level.
-- The inner list should be infinite to so it won't run out of labels no matter
-- how many consecutive ranks are at that level.
newtype LabelComponents = LabelComponents [[Label]]

instance Show LabelComponents where
    show (LabelComponents labels) = show $ map ((++["..."]) . take 10) labels
instance Pretty LabelComponents where pretty = showt

-- | Like 'number_components', but the first two are bigger.
big_number_components :: Int -> LabelComponents
big_number_components sub_start = LabelComponents $ take max_label_depth $
    map big_label (count_from sub_start) : List.repeat (count_from sub_start)
    where
    -- | Limit label component depth.  I'll never take more than this anyway so
    -- it probably doesn't matter, but let's avoid too many infinite lists.
    max_label_depth :: Int
    max_label_depth = 10

big_label :: Label -> Label
big_label t = "`+2/" <> t <> "`"

biggest_label :: Label -> Label
biggest_label t = "`+4/" <> t <> "`"

strip_markup :: Label -> Label
strip_markup = Regex.substitute (Regex.compileUnsafe "`(\\+\\d+/)?") ""

join_label :: [Label] -> Label
join_label = Text.intercalate "."

split_label :: Label -> [Label]
split_label = Text.split (=='.')

-- | Convert label components to label lists based on the given ranks.
convert_labels :: Int -- ^ Labels have at least this many sections.  Otherwise,
    -- trailing sections are omitted.
    -> LabelComponents -> MeasureCount -> [Ruler.Rank] -> [[Label]]
convert_labels min_depth (LabelComponents components) start_measure ranks =
    strip $ map (map replace) $
        apply_labels (drop start_measure measure_labels : components) ranks
    where
    strip = zipWith take (map (max min_depth . (+1)) ranks)
    replace t = if Text.null t then "-" else t

measure_labels :: [Label]
measure_labels = map biggest_label (count_from 0)

count_from :: Int -> [Label]
count_from n = map showt [n..]

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
strip_prefixes :: Text -> Int -> [[Label]] -> [[Label]]
strip_prefixes replacement depth
    | depth <= 0 = id
    | otherwise = map strip . Seq.zip_prev
    where
    strip (prev, cur) =
        [ if d < depth && Just c == mp then replacement else c
        | (d, (c, mp)) <- zip [0..] $ Seq.zip_padded_snd cur (fromMaybe [] prev)
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

-- ** meter constants

-- | The mark color defaults to mostly transparent so it looks nice on overlay
-- rulers.
color1, color2 :: Double -> Double -> Double -> Color.Color
color1 r g b = Color.rgba r g b 0.5
color2 r g b = Color.rgba r g b 0.3

type MarkWidth = Int

-- | Configs for marks in order of increasing rank.
-- @(color, width, zoom_pixels)@
--
-- @zoom_pixels@ is how many pixels of space a mark at this rank must have
-- between its neighbors before it appears.
meter_ranks :: [(Color.Color, MarkWidth, Int)]
meter_ranks =
    [ (a3 0.0 0.0 0.0, 3, 8)    -- section
    , (a3 0.2 0.1 0.0, 2, 8)    -- measure / whole

    , (a3 1.0 0.4 0.2, 2, 8)    -- half
    , (a2 1.0 0.4 0.2, 2, 8)    -- quarter

    , (a3 1.0 0.4 0.9, 1, 8)    -- 8th
    , (a2 1.0 0.4 0.9, 1, 8)    -- 16th

    , (a2 0.1 0.5 0.1, 1, 8)    -- 32nd
    , (a1 0.1 0.5 0.1, 1, 8)    -- 64th

    , (a2 0.0 0.0 0.0, 1, 8)    -- 128th
    , (a1 0.0 0.0 0.0, 1, 8)    -- 256th
    ]
    where
    a1 = alpha 0.2
    a2 = alpha 0.4
    a3 = alpha 0.55
    alpha a r g b = Color.rgba r g b a

-- | These are the conventional meanings for the ranks.
r_section, r_1, r_2, r_4, r_8, r_16, r_32, r_64, r_128, r_256 :: Ruler.Rank
r_section : r_1 : r_2 : r_4 : r_8 : r_16 : r_32 : r_64 : r_128 : r_256 : _ =
  [0..]

-- | By convention, ranks divide up the ruler by dividing it by two for each
-- rank.  This is convenient because that's how staff notation works.  But then
-- the labels wind up being all 0s and 1s, which is not that useful.  The ranks
-- in this list don't receive their own label.
default_labeled_ranks :: Set RankName
default_labeled_ranks = Set.fromList [W, Q, S, T128]

-- | These are mnemonics for staff notation durations, though they may not
-- correspond exactly, as documented in "Cmd.Meter".
rank_names :: [(Ruler.Rank, Text)]
rank_names = zip [0..] (map (Text.toLower . showt) [Section ..])

rank_to_pixels :: [Int]
rank_to_pixels = [pixels | (_, _, pixels) <- meter_ranks]

-- | The rank duration is the duration until the next mark of equal or greater
-- (lower) rank.
rank_durs :: [(Ruler.Rank, Duration)] -> [Duration]
rank_durs = map rank_dur . List.tails
    where
    rank_dur [] = 0
    rank_dur ((rank, dur) : meter) = total
        where total = dur + Num.sum (map snd (takeWhile ((>rank) . fst) meter))

-- | Given a mark duration and the number of pixels it needs to display,
-- return the appropriate zoom factor.
pixels_to_zoom :: Duration -> Int -> Double
pixels_to_zoom dur pixels
    | dur == 0 = 0
    | otherwise = fromIntegral pixels / ScoreTime.to_double dur

-- takeWhileS :: state -> (state -> a -> (state, Bool)) -> [a] -> [a]
-- takeWhileS state f = go state
--     where
--     go _ [] = []
--     go state0 (x : xs)
--         | wanted = x : go state1 xs
--         | otherwise = []
--         where (state1, wanted) = f state0 x
