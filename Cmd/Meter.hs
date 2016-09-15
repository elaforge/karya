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
import qualified Data.Map as Map
import qualified Data.Ratio as Ratio
import qualified Data.Text as Text
import qualified Data.Text.Read as Text.Read

import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Ui.Color as Color
import qualified Ui.Ruler as Ruler
import qualified Ui.ScoreTime as ScoreTime

import Global
import Types


type ModifyRuler = Ruler.Ruler -> Either Text Ruler.Ruler

-- * meter marklist

-- | Meter is for simple numeric meters, as in "Cmd.Meters".  The labels can
-- be generated entirely from the 'Ruler.Rank's.
type Meter = [(Ruler.Rank, Duration)]

-- | LabeledMeter is for meters that have some structure in their labels, and
-- can't be generated from the Ranks only, such as "Cmd.Tala".  After
-- modification, they need a separate pass to renumber the labels, looked up in
-- 'meter_types'.
type LabeledMeter = [LabeledMark]
data LabeledMark = LabeledMark {
    m_rank :: !Ruler.Rank
    , m_duration :: !Duration
    , m_label :: !Label
    } deriving (Show)

instance Pretty.Pretty LabeledMark where
    pretty (LabeledMark rank dur label) = pretty (rank, dur, label)

-- | Duration between ruler marks.  Since these are added together, there is
-- a risk of accumulating innaccuracy.  I could use rationals if I changed
-- 'Ruler.PosMark' to rational, but for the moment it's more convenient to
-- stay as TrackTime, and convert to rationals before adding, assuming that
-- TrackTime has enough resolution to figure out what the rational should be.
--
-- TODO If I get more inaccuracy problems I should probably just switch to
-- rational, but it's a bit of a pain because Ruler.Marklist and its callers
-- have to change.  Also, I'm not even sure if it's a good idea, because
-- TrackTime is still floating point, so there will still be rounding in there
-- somewhere, and this would just put it in more places.
type Duration = TrackTime

time_to_duration :: ScoreTime -> Duration
time_to_duration = id

meter_durations :: LabeledMeter -> [Duration]
meter_durations = scanl (+) 0 . map m_duration

modify_meter :: (LabeledMeter -> LabeledMeter) -> ModifyRuler
modify_meter modify ruler = case flip Map.lookup meter_types =<< mtype of
    Nothing -> Left $ "unknown meter type: " <> pretty mtype
    Just renumber -> Right $ Ruler.set_marklist Ruler.meter mtype new ruler
        where
        new = labeled_marklist $ renumber $ modify $ marklist_labeled mlist
    where (mtype, mlist) = Ruler.get_marklist Ruler.meter ruler

ruler_meter :: Ruler.Ruler -> LabeledMeter
ruler_meter = marklist_labeled . snd . Ruler.get_marklist Ruler.meter

-- | Extract the inclusive range from start to end.
clip :: Duration -> Duration -> LabeledMeter -> LabeledMeter
clip start end =
    transform $ takeWhile ((<=end) . fst) . dropWhile ((<start) . fst)

take_before :: Duration -> LabeledMeter -> LabeledMeter
take_before p = transform $ takeWhile ((<p) . fst)

drop_until :: Duration -> LabeledMeter -> LabeledMeter
drop_until p = transform $ dropWhile ((<p) . fst)

transform :: ([(Duration, LabeledMark)] -> [(Duration, LabeledMark)])
    -> LabeledMeter -> LabeledMeter
transform modify meter = map snd $ modify $ zip (meter_durations meter) meter

-- | Remove the half-open range.
delete :: Duration -> Duration -> LabeledMeter -> LabeledMeter
delete start end meter = map snd pre ++ map snd post
    where
    (pre, within) = break ((>=start) . fst) (zip (meter_durations meter) meter)
    post = dropWhile ((<end) . fst) within

strip_ranks :: Ruler.Rank -> LabeledMeter -> LabeledMeter
strip_ranks max_rank = strip
    where
    strip [] = []
    strip (LabeledMark rank dur label : rest) =
        LabeledMark rank (dur + sum (map m_duration pre)) label : strip post
        where (pre, post) = span ((>max_rank) . m_rank) rest

scale :: Duration -> LabeledMeter -> LabeledMeter
scale dur meter =
    [mark { m_duration = m_duration mark * factor } | mark <- meter]
    where factor = if dur == 0 then 1 else dur / time_end meter

time_end :: LabeledMeter -> Duration
time_end = sum . map m_duration


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
unlabeled_ranks :: [Ruler.Rank]
unlabeled_ranks = [r_section, r_2, r_8, r_32, r_64, r_256]

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

data MeterConfig = MeterConfig {
    -- | The first section number.
    meter_start :: !Int
    -- | Whether label groups start from 0, or 1.
    , meter_from0 :: !Bool
    -- | Strip leading prefixes to this depth, via 'strip_prefixes'.
    , meter_strip_depth :: !Int
    } deriving (Show)

default_config :: MeterConfig
default_config = MeterConfig 1 False 0

-- | Convert a Meter into a Marklist using the default labels.
meter_marklist :: MeterConfig -> Meter -> Ruler.Marklist
meter_marklist config = labeled_marklist . label_meter config

marklist_meter :: Ruler.Marklist -> Meter
marklist_meter =
    map (\(LabeledMark rank dur _) -> (rank, dur)) . marklist_labeled

label_meter :: MeterConfig -> Meter -> LabeledMeter
label_meter (MeterConfig start from0 strip_depth) meter =
    strip_mark_prefixes "_" strip_depth
        [ LabeledMark rank dur (join_label label)
        | (rank, dur, label) <- List.zip3 ranks ps labels
        ]
    where
    (ranks, ps) = unzip meter
    labels = text_labels 1 (make_labels start (if from0 then 0 else 1))
        (collapse_ranks unlabeled_ranks ranks)

unlabel_meter :: LabeledMeter -> Meter
unlabel_meter = map (\m -> (m_rank m, m_duration m))

-- | Create a Marklist from a labeled Meter.
labeled_marklist :: LabeledMeter -> Ruler.Marklist
labeled_marklist meter = Ruler.marklist
    [ (realToFrac pos, mark is_edge dur rank label)
    | (rank, pos, label, dur, is_edge)
        <- List.zip5 ranks
            (scanl (+) 0 (map (to_rational . m_duration) meter))
            (map m_label meter) durs edges
    ]
    where
    -- Avoid accumulating error, as per 'Duration'.
    to_rational t = Ratio.approxRational t 0.0000001
    edges = True : map null (drop 2 (List.tails ranks))
    durs = rank_durs (zip ranks (map m_duration meter))
    ranks = map m_rank meter
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
    [ LabeledMark (Ruler.mark_rank m) (maybe 0 (subtract p . fst) maybe_next)
        (Ruler.mark_name m)
    | ((p, m), maybe_next) <- Seq.zip_next marks
    ]
    where marks = Ruler.ascending 0 mlist


-- *** implementation

count :: Int -> [Label]
count n = map showt [n..]

make_labels :: Int -> Int -> [[Label]]
make_labels section_start label_start =
    count section_start : List.repeat (count label_start)

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

big_label :: Label -> Label
big_label t = "`+2/" <> t <> "`"

biggest_label :: Label -> Label
biggest_label t = "`+4/" <> t <> "`"

-- | Standard numbered meter, starting from 1.
mtype_meter :: Ruler.MeterType
mtype_meter = "meter"

-- | Balinese \"meters\" are just standard numbered meters, but each section
-- starts from 0, instead of 1.
mtype_meter0 :: Ruler.MeterType
mtype_meter0 = "meter0"

-- | Carnatic talas, as generated by "Cmd.Tala".
mtype_tala :: Ruler.MeterType
mtype_tala = "tala"

-- | In order to perform generic operations on meters, such as doubling the
-- length, I need a way to renumber them.  So rulers keep track of their
-- created type and use that to look up the 'Renumber' function.
meter_types :: Map.Map Ruler.MeterType Renumber
meter_types = Map.fromList
    [ (mtype_meter, renumber_meter False)
    , (mtype_meter0, renumber_meter True)
    , (mtype_tala, renumber_topmost)
    ]

type Renumber = LabeledMeter -> LabeledMeter

-- | Strip all labels and renumber.
renumber_meter :: Bool -> Renumber
renumber_meter from0 = label_meter (default_config { meter_from0 = from0 })
    . map (\(LabeledMark rank dur _) -> (rank, dur))

-- | Renumber only the topmost count.  The number is increased at ranks 0 and
-- 1, based on 'Tala.unlabeled_ranks'.
renumber_topmost :: Renumber
renumber_topmost meter = fromMaybe meter $ do
    mark <- Seq.head meter
    label1 : _ <- return $ split_label (m_label mark)
    start <- case Text.Read.decimal label1 of
        Right (d, rest) | Text.null rest -> Just (d - 1)
        _ -> Nothing
    return $ snd $ List.mapAccumL renumber start meter
    where
    renumber n mark = (next_n, mark { m_label = replace next_n (m_label mark) })
        where
        next_n = if m_rank mark `elem` [0, 1] then n + 1 else n
    replace n label = case split_label label of
        _ : rest -> join_label $ showt n : rest
        [] -> ""

join_label :: [Label] -> Label
join_label = Text.intercalate "."

split_label :: Label -> [Label]
split_label = Text.split (=='.')

type Label = Text

text_labels :: Int -- ^ Labels have at least this many sections.  Otherwise,
    -- trailing sections are omitted.
    -> [[Label]] -> [Ruler.Rank] -> [[Label]]
text_labels min_depth labels ranks =
    strip $ map (map replace) $ apply_labels labels ranks
    where
    strip = zipWith take (map (max min_depth . (+1)) ranks)
    replace t = if Text.null t then "-" else t

-- | The ruler gets cluttered if I label every single rank, so combine the ones
-- in the given list with the following rank.
collapse_ranks :: [Ruler.Rank] -> [Ruler.Rank] -> [Ruler.Rank]
collapse_ranks omit = map (\r -> r - sub r)
    where sub r = length (takeWhile (<r) omit)

strip_mark_prefixes :: Text -> Int -> [LabeledMark] -> [LabeledMark]
strip_mark_prefixes replacement depth marks
    | depth <= 0 = marks
    | otherwise =
        [m { m_label = s } | (m, s) <- zip marks labels]
        where labels = strip_prefixes replacement depth $ map m_label marks

-- | When labels are created, many of them have the same components as the
-- previous label, e.g. @1.1.1@, @1.1.2@.  Replace the identical components
-- with a placeholder to make the difference more apparent: @1.1.1@, @-.-.2@.
--
-- This doesn't actually look that nice on the UI because it does it for all
-- labels, not just the visible ones.
strip_prefixes :: Text -> Int -> [Label] -> [Label]
strip_prefixes replacement depth =
    map (join_label . strip) . Seq.zip_prev . map split_label
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
