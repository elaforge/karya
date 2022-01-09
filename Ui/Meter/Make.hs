-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
-- | Convert Meter.Meter into a low level marklist.
module Ui.Meter.Make (
    make_marklist
    , to_rank_durations -- used by Perform.Lilypond.Meter
    , big_label
    , count_from
#ifdef TESTING
    , module Ui.Meter.Make
#endif
) where
import qualified Data.List as List
import qualified Data.Ratio as Ratio
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Util.Num as Num
import qualified Util.Regex as Regex
import qualified Ui.Color as Color
import qualified Ui.Meter.Mark as Mark
import qualified Ui.Meter.Meter as Meter
import           Ui.Meter.Meter (AbstractMeter(..), Duration, Label, Measures)
import qualified Ui.ScoreTime as ScoreTime

import           Global
import           Types


-- TODO If I port this to c++, then I can just marshal Config instead of the
-- vector.  Then, I could only materialize marks that are visible and stop
-- paying for the micro divisions I hardly ever see.
--
-- On the other hand, paying for them once on ruler creation sounds better
-- than paying for them on every zoom change.
make_marklist :: Meter.Meter -> Mark.Marklist
make_marklist = Mark.marklist . make_measures

-- | Repeat each measure for the given count.
-- This can't express unaligned beats.  If you want a cut-off measure
-- like 4/4 - 1/16, you would have to create a special measure for it.
-- If I want that, I can make a function on AbstractMeter.
make_measures :: Meter.Meter -> [(TrackTime, Mark.Mark)]
make_measures meter =
    labeled_marklist 0 $
    label_ranks config (Meter.config_start_measure config) $
    to_rank_durations $
    map expand (Meter.meter_sections meter)
    where
    config = Meter.meter_config meter
    expand (Meter.MSection count dur measure) =
        (tdur, D (replicate count measure))
        where tdur = dur / fromIntegral (Meter.meter_length measure)

-- | The AbstractMeters are concatenated, and each one defines a rank 0.
-- Each T level gets the given Duration.
to_rank_durations :: [(Duration, AbstractMeter)] -> [(Mark.Rank, Duration)]
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

data LabeledMark = LabeledMark {
    m_rank :: !Mark.Rank
    , m_duration :: !Duration
    , m_label :: !Label
    } deriving (Eq, Show)

instance Pretty LabeledMark where
    pretty (LabeledMark rank dur label) = pretty (rank, dur, label)

-- | Add Labels to the given ruler ranks.
label_ranks :: Meter.Config -> Measures -> [(Mark.Rank, Duration)]
    -> [LabeledMark]
label_ranks config start_measure rank_durs =
    [ LabeledMark rank dur label
    | (rank, dur, label) <- List.zip3 ranks ps labels
    ]
    where
    -- (ranks, ps) = unzip (drop_0dur rank_durs)
    (ranks, ps) = unzip rank_durs
    labels = map join_label $
        strip_prefixes "" (Meter.config_strip_depth config) $
        convert_labels (Meter.config_min_depth config)
            (label_components (Meter.config_label config))
            start_measure (collapse_ranks unlabeled ranks)
    unlabeled = labeled_to_unlabeled_ranks (Meter.config_labeled_ranks config)
    -- -- Appending Meters can result in 0 dur marks in the middle.
    -- drop_0dur [] = []
    -- drop_0dur ((r, d) : rank_durs)
    --     | d == 0 && not (null rank_durs) = drop_0dur rank_durs
    --     | otherwise = (r, d) : drop_0dur rank_durs

labeled_to_unlabeled_ranks :: Set Meter.RankName -> [Mark.Rank]
labeled_to_unlabeled_ranks labeled =
    [Meter.name_to_rank r | r <- Meter.all_ranks, not (r `Set.member` labeled)]

-- | Create marks from a labeled meter.
labeled_marklist :: TrackTime -> [LabeledMark] -> [(TrackTime, Mark.Mark)]
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
    mark is_edge rank_dur rank name = Mark.Mark
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


-- * labels

-- TODO make LabelConfig into a general description, eliminate LabelComponents
label_components :: Meter.LabelConfig -> LabelComponents
label_components = \case
    Meter.BigNumber sub_start -> big_number_components sub_start
    Meter.Cycle labels -> LabelComponents
        [ cycle labels
        , numbers -- nadai / gati
        , numbers, numbers, numbers, numbers
        ]
        where numbers = count_from 1

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
    -> LabelComponents -> Measures -> [Mark.Rank] -> [[Label]]
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
collapse_ranks :: [Mark.Rank] -> [Mark.Rank] -> [Mark.Rank]
collapse_ranks omit = map (\r -> r - sub r)
    where sub r = length (takeWhile (<r) omit)

-- | When labels are created, many of them have the same components as the
-- previous label, e.g. @1.1.1@, @1.1.2@.  Replace leading components up to
-- a certain depth with a placeholder to make the difference more apparent:
-- @1.1.1@, @-.-.2@.
--
-- This doesn't actually look that nice on the UI because it does it for all
-- labels, not just the visible ones.
strip_prefixes :: Text -> Int -> [[Label]] -> [[Label]]
strip_prefixes replacement depth = map strip
    where
    strip labels = replicate prefix replacement ++ post
        where (prefix, post) = split1 depth labels
    -- Like splitAt, but don't take the final one, because then I'd replace
    -- everything in shorter labels.
    split1 n xs | n <= 0 = (0, xs)
    split1 _ [] = (0, [])
    split1 _ [x] = (0, [x])
    split1 n (_:xs) = first (+1) (split1 (n-1) xs)

-- | Apply the labels according to the ranks.  Each Rank input has
-- a corresponding @[Label]@ output.  Each rank advances the label at the rank's
-- index in the labels, and resets all the labels beneath it.  If a rank runs
-- out of labels, @\"\"@ is emitted.
--
-- The first rank doesn't matter since it always emits the initial state of the
-- labels.
apply_labels :: [[Label]] -> [Mark.Rank] -> [[Label]]
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

-- | The rank duration is the duration until the next mark of equal or greater
-- (lower) rank.
rank_durs :: [(Mark.Rank, Duration)] -> [Duration]
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
