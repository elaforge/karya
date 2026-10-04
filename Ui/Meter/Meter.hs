-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{-# LANGUAGE StrictData #-}
{- | Types to describe meters.

    A 'Meter' gets turned into a lower level Marklist, and goes in the
    'Ruler.meter_name' section of a Ruler.  'Ruler.meter_ruler' creates one.

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
module Ui.Meter.Meter (
    Meter(..), MSection(..), Measures
    , meter
    , meterEnd
    , emptyMeter
    , modifyConfig
    , setSections, modifySections
    , sectionsSplit, sectionsDrop, sectionsTake
    , sectionStarts
    , Duration, timeToDuration
    , Config(..)
    , defaultConfig
    , Rank(..), allRanks
    , rankNames, rankName
    , LabelConfig(..)
    , Label
    -- * AbstractMeter
    , AbstractMeter(..)
    , subdivide, subdivides
    , repeat, repeats
    , regularSubdivision
    , meterLength
#ifdef TESTING
    , module Ui.Meter.Meter
#endif
) where
import           Prelude hiding (repeat)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified GHC.Generics as Generics

import qualified Util.Lists as Lists
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import           Util.Pretty ((<+>))

import           Global
import           Types


-- | Duration is of one AbstractMeter, so total duration will be count*dur.
data Meter = Meter
    { meterConfig :: Config
    , meterSections :: [MSection] -- I frequently modify the end, Seq, Vector?
    } deriving (Eq, Show)

instance Pretty Meter where
    format (Meter config sections) = Pretty.record "Meter"
        [ ("config", Pretty.format config)
        , ("sections", Pretty.format sections)
        ]

instance Semigroup Meter where
    meter1 <> meter2 = meter (meterConfig meter1)
        (meterSections meter1 <> meterSections meter2)
instance Monoid Meter where
    mempty = emptyMeter
    mappend = (<>)

emptyMeter :: Meter
emptyMeter = Meter defaultConfig []

meterEnd :: Meter -> TrackTime
meterEnd = Num.sum . map sectionDuration . meterSections

meter :: Config -> [MSection] -> Meter
meter config sections =
    setSections (filter ((>0) . sectionMeasures) sections) $
    modifyConfig (const config) emptyMeter

-- Called MSection due to annoying name clash with Rank Section.
-- If I change that one, then the change to rank_names affects parsing.
data MSection = MSection
    -- | The section contains this many measures.
    { sectionMeasures :: Measures
    -- | Each measure has this duration.
    , sectionMeasureDuration :: Duration
    -- | Describe a measure.
    , sectionMeasure :: AbstractMeter
    } deriving (Eq, Show)

instance Pretty MSection where
    format (MSection measures dur measure) =
        "MSection" <+> Pretty.format measures <+> Pretty.format dur
            <+> Pretty.format measure

type Measures = Int

sectionDuration :: MSection -> Duration
sectionDuration (MSection measures dur _) = fromIntegral measures * dur

modifyConfig :: (Config -> Config) -> Meter -> Meter
modifyConfig modify meter =
    meter { meterConfig = modify (meterConfig meter) }

setSections :: [MSection] -> Meter -> Meter
setSections = modifySections . const

modifySections :: ([MSection] -> [MSection]) -> Meter -> Meter
modifySections modify meter =
    meter { meterSections = modify (meterSections meter) }

-- | Trimming the AbstractMeter from the start will change the labels, but
-- that's probably desired everywhere except a pickup.
sectionsDrop :: TrackTime -> [MSection] -> [MSection]
sectionsDrop start = snd . sectionsSplit start

sectionsTake :: TrackTime -> [MSection] -> [MSection]
sectionsTake end = fst . sectionsSplit end

sectionsSplit :: TrackTime -> [MSection] -> ([MSection], [MSection])
sectionsSplit at = if at <= 0 then ([],) else go 0
    where
    go _ [] = ([], [])
    go t (s : ss)
        | at >= t2 = first (s:) rest
        | at <= t = second (s:) rest
        | otherwise = bimap (pre++) (post++) rest
        where
        rest = go t2 ss
        t2 = t + sectionDuration s
        (pre, post) = sectionSplit (at-t) s

sectionSplit :: TrackTime -> MSection -> ([MSection], [MSection])
sectionSplit at (MSection count dur meter)
    | ts <= 0 = (makeSection measures, makeSection (count - measures))
    | otherwise =
        ( makeSection measures ++ maybe [] (:[]) preSection
        , maybe [] (:[]) postSection ++ makeSection (count - measures-1)
        )
    where
    makeSection m
        | m <= 0 = []
        | otherwise = [MSection m dur meter]
    (preSection, postSection) =
        ( MSection 1 (fromIntegral ts * tlen) <$> pre
        , MSection 1 (dur - fromIntegral ts * tlen) <$> post
        )
        where (pre, post) = meterSplit ts meter
    (measures_, frac) = properFraction (at / dur)
    -- Since Duration is not Rational, I can easily get off and floor or
    -- ceiling will be surprising half the time.
    ts_ = round $ (frac * dur) / tlen
    -- Because I use round, 'ts' could overflow and be ==mlen.
    (measures, ts)
        | ts_ >= mlen = (measures_ + 1, ts_ - mlen)
        | otherwise = (measures_, ts_)
    mlen = meterLength meter
    tlen = dur / fromIntegral mlen

sectionStarts :: [MSection] -> [(Duration, MSection)]
sectionStarts sections =
    zip (scanl (+) 0 (map sectionDuration sections)) sections

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

-- | TODO it's id for now, but maybe I'll want to make it Rational at some
-- point?
timeToDuration :: TrackTime -> Duration
timeToDuration = id

data Config = Config
    -- | Only write labels on these ranks.
    { configLabeledRanks :: Set Rank
    -- | How to generate labels.
    , configLabel :: LabelConfig
    -- | The ruler should start counting at this number.  This could be measure
    -- number, or gong count, or avartanam count, whatever is the highest visual
    -- 'Label'.
    , configStartMeasure :: Measures
    -- | Labels have at least this many sections.  Otherwise, trailing sections
    -- are omitted.
    , configMinDepth :: Int
    -- | Strip leading prefixes to this depth, via 'strip_prefixes'.
    , configStripDepth :: Int
    } deriving (Eq, Show, Generics.Generic)

instance Pretty Config where format = Pretty.formatGCamel

defaultConfig :: Config
defaultConfig = Config
    { configLabeledRanks = defaultLabeledRanks
    , configLabel = BigNumber 1
    , configStartMeasure = 1
    , configMinDepth = 1
    , configStripDepth = 2
    }

-- | By convention, ranks divide up the ruler by dividing it by two for each
-- rank.  This is convenient because that's how staff notation works.  But then
-- the labels wind up being all 0s and 1s, which is not that useful.  The ranks
-- in this list don't receive their own label.
defaultLabeledRanks :: Set Rank
defaultLabeledRanks = Set.fromList [W, Q, S, T128]

-- * Rank

-- Also used by Derive.Typecheck
data Rank = Section | W | H | Q | E | S | T32 | T64 | T128 | T256
    deriving (Show, Eq, Ord, Bounded, Enum)
instance Pretty Rank where pretty = showt

allRanks :: [Rank]
allRanks = [minBound .. maxBound]

-- | These are mnemonics for staff notation durations, though they may not
-- correspond exactly, as documented in "Cmd.Meter".
rankNames :: [(Rank, Text)]
rankNames = zip [minBound ..] (map rankName allRanks)

rankName :: Rank -> Text
rankName = Text.toLower . showt

data LabelConfig = BigNumber Int | Cycle [Label]
    deriving (Eq, Show)
type Label = Text -- TODO make it ByteString so I can pass to c++ efficiently?

instance Pretty LabelConfig where pretty = showt

-- * AbstractMeter

{- | An AbstractMeter is a structured description of how a unit of time is
    broken up into hiererchical sections.  A 'T' represents a mark with the
    unit duration, and a 'D' is a group of Meters.  The rank of each mark is
    determined by its nesting depth.

    Previously a 'T' could take a duration, but I didn't wind up using that
    feature, so I removed it.  So meters have to be built of multiples of a
    unit duration multiplied by some stretch factor.

    An AbstractMeter can be created either by declaring it outright, or by
    declaring a simpler AbstractMeter and subdividing or repeating it.
-}
data AbstractMeter = T | D [AbstractMeter]
    deriving (Eq, Show)

instance Pretty AbstractMeter where pretty = showt

-- | Subdivide each mark into the given number @D@s.  This has the effect of
-- putting one layer of subdivision under the current structure.
subdivide :: Int -> AbstractMeter -> AbstractMeter
subdivide n = replaceT (D (replicate n T))

subdivides :: [Int] -> AbstractMeter -> AbstractMeter
subdivides divs meter = foldr subdivide meter (reverse divs)

-- | Create a layer that repeats the given meter a certain number of times.
repeat :: Int -> AbstractMeter -> AbstractMeter
repeat n meter = D $ replicate n meter

repeats :: [Int] -> AbstractMeter -> AbstractMeter
repeats ns meter = foldr repeat meter ns

-- | Form a meter based on regular subdivision.  E.g. [4, 4] is 4 groups of 4,
-- [3, 3] is like 9\/8, and [4, 3] is 4 groups of 3 (12\/8).
regularSubdivision :: [Int] -> AbstractMeter
    -- It's most natural to think of the list as big divisions on the left to
    -- small divisions on the right, so reverse the list.
regularSubdivision ns = foldr subdivide T (reverse ns)

-- ** AbstractMeter utils

-- | Map the given function over all @T@s in the given AbstractMeter.
replaceT :: AbstractMeter -> AbstractMeter -> AbstractMeter
replaceT val (D ts) = D (map (replaceT val) ts)
replaceT val T = val

meterLength :: AbstractMeter -> Int
meterLength (D ms) = Num.sum (map meterLength ms)
meterLength T = 1

meterDropEnd :: Int -> AbstractMeter -> Maybe AbstractMeter
meterDropEnd n
    | n <= 0 = Just
    | otherwise = fmap meterReverse . meterDrop n . meterReverse

meterReverse :: AbstractMeter -> AbstractMeter
meterReverse = \case
    T -> T
    D ts -> D (reverse (map meterReverse ts))

-- | Drop the number of Ts.  This has to be in Maybe because there's no empty
-- AbstractMeter.
meterDrop :: Int -> AbstractMeter -> Maybe AbstractMeter
meterDrop n = snd . meterSplit n

meterTake :: Int -> AbstractMeter -> Maybe AbstractMeter
meterTake n = fst . meterSplit n

meterSplit :: Int -> AbstractMeter
    -> (Maybe AbstractMeter, Maybe AbstractMeter)
meterSplit n m
    | n <= 0 = (Nothing, Just m)
    | otherwise = case snd (go n [m]) of
        (pre, post) -> (Lists.head pre, Lists.head post)
    where
    go n = \case
        ts | n <= 0 -> (0, ([], ts))
        [] -> (n, ([], []))
        D subs : ts
            | n2 <= 0 ->
                ( 0
                -- I don't have a use for D [] so those get stripped out.
                -- TODO should it be D (NonEmpty AbstractMeter) then?
                , ( if null pre then [] else [D pre]
                  , if null post then ts else D post : ts
                  )
                )
            | otherwise -> (n3, (D subs : pre2, post2))
            -- | otherwise -> second (first (D subs :)) (go n2 ts)
            where
            (n2, (pre, post)) = go n subs
            (n3, (pre2, post2)) = go n2 ts
        T : ts -> second (first (T:)) (go (n-1) ts)
