-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
-- | Convert Meter.Meter into a low level marklist.
module Ui.Meter.Make (
    makeMarklist
    , toRankDurations -- used by Perform.Lilypond.Meter
    , bigLabel
    , countFrom
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
makeMarklist :: Meter.Meter -> Mark.Marklist
makeMarklist = Mark.marklist . makeMeasures

-- | Repeat each measure for the given count.
-- This can't express unaligned beats.  If you want a cut-off measure
-- like 4/4 - 1/16, you would have to create a special measure for it.
-- If I want that, I can make a function on AbstractMeter.
makeMeasures :: Meter.Meter -> [(TrackTime, Mark.Mark)]
makeMeasures meter =
    labeledMarklist 0 $
    labelRanks config (Meter.configStartMeasure config) $
    toRankDurations $
    map expand (Meter.meterSections meter)
    where
    config = Meter.meterConfig meter
    expand (Meter.MSection count dur measure) =
        (tdur, D (replicate count measure))
        where tdur = dur / fromIntegral (Meter.meterLength measure)

-- | The AbstractMeters are concatenated, and each one defines a rank 0.
-- Each T level gets the given Duration.
toRankDurations :: [(Duration, AbstractMeter)] -> [(Meter.Rank, Duration)]
toRankDurations = group0 . concatMap (uncurry (convert minBound))
    where
    -- Convert returns an intermediate format where all the ranks coexist at
    -- the same time, by giving them 0 dur.
    group0 rankDur = case span ((==0) . snd) rankDur of
        (zeros, (rank, dur) : rest) ->
            (minimum (rank : map fst zeros), dur) : group0 rest
        (_, []) -> [(minBound, 0)]
    convert rank dur meter = case meter of
        T -> [(rank, dur)]
        D ms -> (rank, 0) : concatMap (convert (nextRank rank) dur) ms

nextRank :: Meter.Rank -> Meter.Rank
nextRank r
    | r == maxBound = r
    | otherwise = succ r

data LabeledMark = LabeledMark
    { mRank :: !Meter.Rank
    , mDuration :: !Duration
    , mLabel :: !Label
    } deriving (Eq, Show)

type RankNum = Int

rankNum :: Meter.Rank -> RankNum
rankNum = fromEnum

instance Pretty LabeledMark where
    pretty (LabeledMark rank dur label) = pretty (rank, dur, label)

-- | Add Labels to the given ruler ranks.
labelRanks :: Meter.Config -> Measures -> [(Meter.Rank, Duration)]
    -> [LabeledMark]
labelRanks config startMeasure rankDurs =
    [ LabeledMark rank dur label
    | (rank, dur, label) <- List.zip3 ranks ps labels
    ]
    where
    (ranks, ps) = unzip rankDurs
    labels = map joinLabel $
        stripPrefixes "" (Meter.configStripDepth config) $
        convertLabels (Meter.configMinDepth config)
            (labelComponents (Meter.configLabel config))
            startMeasure
            (collapseRanks unlabeled ranks)
    unlabeled = labeledToUnlabeledRanks (Meter.configLabeledRanks config)

labeledToUnlabeledRanks :: Set Meter.Rank -> [Meter.Rank]
labeledToUnlabeledRanks labeled =
    [r | r <- Meter.allRanks, not (r `Set.member` labeled)]

-- | Create marks from a labeled meter.
labeledMarklist :: TrackTime -> [LabeledMark] -> [(TrackTime, Mark.Mark)]
labeledMarklist start marks =
    [ (realToFrac pos, mark dur rank label)
    | (rank, pos, label, dur) <- List.zip4 ranks starts (map mLabel marks) durs
    ]
    where
    starts = scanl (+) (toRational start)
        (map (toRational . mDuration) marks)
    -- Avoid accumulating error, as per 'Duration'.
    toRational t = Ratio.approxRational t 0.0000001
    durs = rankDurs (zip ranks (map mDuration marks))
    ranks = map mRank marks
    mark rankDur rank name = Mark.Mark
        { markRank = rank
        , markWidth = width
        , markColor = color
        , markName = name
        , markNameZoomLevel = zoom * 2
        , markZoomLevel = zoom
        }
        where
        (color, width, pixels) = meterRanks !! min (rankNum rank) ranksLen
        zoom = pixelsToZoom rankDur pixels
    ranksLen = length meterRanks


-- * labels

-- TODO make LabelConfig into a general description, eliminate LabelComponents
labelComponents :: Meter.LabelConfig -> LabelComponents
labelComponents = \case
    Meter.BigNumber subStart -> bigNumberComponents subStart
    Meter.Cycle labels -> LabelComponents
        [ cycle labels
        , numbers -- nadai / gati
        , numbers, numbers, numbers, numbers
        ]
        where numbers = countFrom 1

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
bigNumberComponents :: Int -> LabelComponents
bigNumberComponents subStart = LabelComponents $ take maxLabelDepth $
    map bigLabel (countFrom subStart) : List.repeat (countFrom subStart)
    where
    -- | Limit label component depth.  I'll never take more than this anyway so
    -- it probably doesn't matter, but let's avoid too many infinite lists.
    maxLabelDepth :: Int
    maxLabelDepth = 10

bigLabel :: Label -> Label
bigLabel t = "`+2/" <> t <> "`"

biggestLabel :: Label -> Label
biggestLabel t = "`+4/" <> t <> "`"

stripMarkup :: Label -> Label
stripMarkup = Regex.substitute (Regex.compileUnsafe "`(\\+\\d+/)?") ""

joinLabel :: [Label] -> Label
joinLabel = Text.intercalate "."

splitLabel :: Label -> [Label]
splitLabel = Text.split (=='.')

-- | Convert label components to label lists based on the given ranks.
convertLabels :: Int -- ^ Labels have at least this many sections.  Otherwise,
    -- trailing sections are omitted.
    -> LabelComponents -> Measures -> [RankNum] -> [[Label]]
convertLabels minDepth (LabelComponents components) startMeasure ranks =
    strip $ map (map replace) $
        applyLabels (drop startMeasure measureLabels : components) ranks
    where
    strip = zipWith take (map (max minDepth . (+1)) ranks)
    replace t = if Text.null t then "-" else t

measureLabels :: [Label]
measureLabels = map biggestLabel (countFrom 0)

countFrom :: Int -> [Label]
countFrom n = map showt [n..]

-- | The ruler gets cluttered if I label every single rank, so combine the ones
-- in the given list with the following rank.
collapseRanks :: [Meter.Rank] -> [Meter.Rank] -> [RankNum]
collapseRanks omit = map (\r -> rankNum r - below r)
    where below r = length (takeWhile (<r) omit)

-- | When labels are created, many of them have the same components as the
-- previous label, e.g. @1.1.1@, @1.1.2@.  Replace leading components up to
-- a certain depth with a placeholder to make the difference more apparent:
-- @1.1.1@, @-.-.2@.
--
-- This doesn't actually look that nice on the UI because it does it for all
-- labels, not just the visible ones.
stripPrefixes :: Text -> Int -> [[Label]] -> [[Label]]
stripPrefixes replacement depth = map strip
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
applyLabels :: [[Label]] -> [RankNum] -> [[Label]]
applyLabels labels =
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
meterRanks :: [(Color.Color, MarkWidth, Int)]
meterRanks =
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
rankDurs :: [(Meter.Rank, Duration)] -> [Duration]
rankDurs = map rankDur . List.tails
    where
    rankDur [] = 0
    rankDur ((rank, dur) : meter) = total
        where total = dur + Num.sum (map snd (takeWhile ((>rank) . fst) meter))

-- | Given a mark duration and the number of pixels it needs to display,
-- return the appropriate zoom factor.
pixelsToZoom :: Duration -> Int -> Double
pixelsToZoom dur pixels
    | dur == 0 = 0
    | otherwise = fromIntegral pixels / ScoreTime.toDouble dur
