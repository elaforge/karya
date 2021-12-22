-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Types to describe meters.
module Ui.Meter.Meter (
    Meter(..)
    , MeasureCount
    , Duration
    , Config(..)
    , default_config
    , RankName(..), all_ranks, name_to_rank
    , LabelConfig(..)
    -- * AbstractMeter
    , AbstractMeter(..)
    , subdivide, subdivides
    , repeat, repeats
    , regular_subdivision
    , meter_length
) where
import           Prelude hiding (repeat)
import qualified Data.Set as Set
import qualified GHC.Generics as Generics

import qualified Util.Num as Num
import qualified Util.Pretty as Pretty

import           Global
import           Types


-- | Duration is of one AbstractMeter, so total duration will be count*dur.
data Meter = Meter !Config ![(MeasureCount, Duration, AbstractMeter)]
    deriving (Show)

type MeasureCount = Int

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

data Config = Config {
    -- | Skip labels for these ranks.
    config_labeled_ranks :: !(Set RankName)
    -- | The convention is that the first two ranks, section and measure, are
    -- universal.  So this omits measure, which gets 'measure_labels', starting
    -- from 'config_start_measure'.
    , config_label :: !LabelConfig
    -- | The ruler should start counting at this number.  This could be measure
    -- number, or gong count, or avartanam count, whatever is the highest visual
    -- 'Label'.
    , config_start_measure :: !MeasureCount
    -- | Labels have at least this many sections.  Otherwise, trailing sections
    -- are omitted.
    , config_min_depth :: !Int
    -- | Strip leading prefixes to this depth, via 'strip_prefixes'.
    , config_strip_depth :: !Int
    } deriving (Show, Generics.Generic)

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
default_labeled_ranks :: Set RankName
default_labeled_ranks = Set.fromList [W, Q, S, T128]

-- TODO this type is shared with Derive, maybe it should go in its own module?
data RankName = Section | W | H | Q | E | S | T32 | T64 | T128 | T256
    deriving (Show, Eq, Ord, Bounded, Enum)
instance Pretty RankName where pretty = showt

type Rank = Int -- same as Ruler.Rank

all_ranks :: [RankName]
all_ranks = [minBound .. maxBound]

name_to_rank :: RankName -> Rank
name_to_rank = fromEnum

newtype LabelConfig = BigNumber Int
    deriving (Eq, Show, Pretty)

-- * AbstractMeter

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
