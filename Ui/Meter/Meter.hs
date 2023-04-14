-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{- | Types to describe meters.

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
    , meter_end
    , empty_meter
    , modify_config
    , set_sections, modify_sections
    , sections_split, sections_drop, sections_take
    , section_starts
    , Duration, time_to_duration
    , Config(..)
    , default_config
    , Rank(..), all_ranks
    , rank_names, rank_name
    , LabelConfig(..)
    , Label
    -- * AbstractMeter
    , AbstractMeter(..)
    , subdivide, subdivides
    , repeat, repeats
    , regular_subdivision
    , meter_length
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
data Meter = Meter {
    meter_config :: !Config
    , meter_sections :: ![MSection] -- I frequently modify the end, Seq, Vector?
    } deriving (Eq, Show)

instance Pretty Meter where
    format (Meter config sections) = Pretty.record "Meter"
        [ ("config", Pretty.format config)
        , ("sections", Pretty.format sections)
        ]

instance Semigroup Meter where
    meter1 <> meter2 = meter (meter_config meter1)
        (meter_sections meter1 <> meter_sections meter2)
instance Monoid Meter where
    mempty = empty_meter
    mappend = (<>)

empty_meter :: Meter
empty_meter = Meter default_config []

meter_end :: Meter -> TrackTime
meter_end = Num.sum . map section_duration . meter_sections

meter :: Config -> [MSection] -> Meter
meter config sections =
    set_sections (filter ((>0) . section_measures) sections) $
    modify_config (const config) empty_meter

-- Called MSection due to annoying name clash with Rank Section.
-- If I change that one, then the change to rank_names affects parsing.
data MSection = MSection {
    -- | The section contains this many measures.
    section_measures :: !Measures
    -- | Each measure has this duration.
    , section_measure_duration :: !Duration
    -- | Describe a measure.
    , section_measure :: !AbstractMeter
    } deriving (Eq, Show)

instance Pretty MSection where
    format (MSection measures dur measure) =
        "MSection" <+> Pretty.format measures <+> Pretty.format dur
            <+> Pretty.format measure

type Measures = Int

section_duration :: MSection -> Duration
section_duration (MSection measures dur _) = fromIntegral measures * dur

modify_config :: (Config -> Config) -> Meter -> Meter
modify_config modify meter =
    meter { meter_config = modify (meter_config meter) }

set_sections :: [MSection] -> Meter -> Meter
set_sections = modify_sections . const

modify_sections :: ([MSection] -> [MSection]) -> Meter -> Meter
modify_sections modify meter =
    meter { meter_sections = modify (meter_sections meter) }

-- | Trimming the AbstractMeter from the start will change the labels, but
-- that's probably desired everywhere except a pickup.
sections_drop :: TrackTime -> [MSection] -> [MSection]
sections_drop start = snd . sections_split start

sections_take :: TrackTime -> [MSection] -> [MSection]
sections_take end = fst . sections_split end

sections_split :: TrackTime -> [MSection] -> ([MSection], [MSection])
sections_split at = if at <= 0 then ([],) else go 0
    where
    go _ [] = ([], [])
    go t (s : ss)
        | at >= t2 = first (s:) rest
        | at <= t = second (s:) rest
        | otherwise = bimap (pre++) (post++) rest
        where
        rest = go t2 ss
        t2 = t + section_duration s
        (pre, post) = section_split (at-t) s

section_split :: TrackTime -> MSection -> ([MSection], [MSection])
section_split at (MSection count dur meter)
    | ts <= 0 = (make_section measures, make_section (count - measures))
    | otherwise =
        ( make_section measures ++ maybe [] (:[]) pre_section
        , maybe [] (:[]) post_section ++ make_section (count - measures-1)
        )
    where
    make_section m
        | m <= 0 = []
        | otherwise = [MSection m dur meter]
    (pre_section, post_section) =
        ( MSection 1 (fromIntegral ts * tlen) <$> pre
        , MSection 1 (dur - fromIntegral ts * tlen) <$> post
        )
        where (pre, post) = meter_split ts meter
    (measures_, frac) = properFraction (at / dur)
    -- Since Duration is not Rational, I can easily get off and floor or
    -- ceiling will be surprising half the time.
    ts_ = round $ (frac * dur) / tlen
    -- Because I use round, 'ts' could overflow and be ==mlen.
    (measures, ts)
        | ts_ >= mlen = (measures_ + 1, ts_ - mlen)
        | otherwise = (measures_, ts_)
    mlen = meter_length meter
    tlen = dur / fromIntegral mlen

section_starts :: [MSection] -> [(Duration, MSection)]
section_starts sections =
    zip (scanl (+) 0 (map section_duration sections)) sections

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
time_to_duration :: TrackTime -> Duration
time_to_duration = id

data Config = Config {
    -- | Only write labels on these ranks.
    config_labeled_ranks :: !(Set Rank)
    -- | How to generate labels.
    , config_label :: !LabelConfig
    -- | The ruler should start counting at this number.  This could be measure
    -- number, or gong count, or avartanam count, whatever is the highest visual
    -- 'Label'.
    , config_start_measure :: !Measures
    -- | Labels have at least this many sections.  Otherwise, trailing sections
    -- are omitted.
    , config_min_depth :: !Int
    -- | Strip leading prefixes to this depth, via 'strip_prefixes'.
    , config_strip_depth :: !Int
    } deriving (Eq, Show, Generics.Generic)

instance Pretty Config where format = Pretty.formatG_

default_config :: Config
default_config = Config
    { config_labeled_ranks = default_labeled_ranks
    , config_label = BigNumber 1
    , config_start_measure = 1
    , config_min_depth = 1
    , config_strip_depth = 2
    }

-- | By convention, ranks divide up the ruler by dividing it by two for each
-- rank.  This is convenient because that's how staff notation works.  But then
-- the labels wind up being all 0s and 1s, which is not that useful.  The ranks
-- in this list don't receive their own label.
default_labeled_ranks :: Set Rank
default_labeled_ranks = Set.fromList [W, Q, S, T128]

-- * Rank

-- Also used by Derive.Typecheck
data Rank = Section | W | H | Q | E | S | T32 | T64 | T128 | T256
    deriving (Show, Eq, Ord, Bounded, Enum)
instance Pretty Rank where pretty = showt

all_ranks :: [Rank]
all_ranks = [minBound .. maxBound]

-- | These are mnemonics for staff notation durations, though they may not
-- correspond exactly, as documented in "Cmd.Meter".
rank_names :: [(Rank, Text)]
rank_names = zip [minBound ..] (map rank_name all_ranks)

rank_name :: Rank -> Text
rank_name = Text.toLower . showt

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

-- ** AbstractMeter utils

-- | Map the given function over all @T@s in the given AbstractMeter.
replace_t :: AbstractMeter -> AbstractMeter -> AbstractMeter
replace_t val (D ts) = D (map (replace_t val) ts)
replace_t val T = val

meter_length :: AbstractMeter -> Int
meter_length (D ms) = Num.sum (map meter_length ms)
meter_length T = 1

meter_drop_end :: Int -> AbstractMeter -> Maybe AbstractMeter
meter_drop_end n
    | n <= 0 = Just
    | otherwise = fmap meter_reverse . meter_drop n . meter_reverse

meter_reverse :: AbstractMeter -> AbstractMeter
meter_reverse = \case
    T -> T
    D ts -> D (reverse (map meter_reverse ts))

-- | Drop the number of Ts.  This has to be in Maybe because there's no empty
-- AbstractMeter.
meter_drop :: Int -> AbstractMeter -> Maybe AbstractMeter
meter_drop n = snd . meter_split n

meter_take :: Int -> AbstractMeter -> Maybe AbstractMeter
meter_take n = fst . meter_split n

meter_split :: Int -> AbstractMeter
    -> (Maybe AbstractMeter, Maybe AbstractMeter)
meter_split n m
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
