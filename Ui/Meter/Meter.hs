-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

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
    Meter, Section(..), Measures
    , meter
    , meter_config, meter_sections, meter_end
    , empty_meter
    , modify_config
    , modify_sections, clip_start, clip_end
    , Duration, time_to_duration
    , Config(..)
    , default_config
    , RankName(..), all_ranks, name_to_rank
    , r_section, r_1, r_2, r_4, r_8, r_16, r_32, r_64, r_128, r_256
    , rank_names
    , LabelConfig(..)
    , Label
    -- * AbstractMeter
    , AbstractMeter(..)
    , subdivide, subdivides
    , repeat, repeats
    , regular_subdivision
    , meter_length
    , drop_meter
) where
import           Prelude hiding (repeat)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified GHC.Generics as Generics

import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import           Util.Pretty ((<+>))
import qualified Util.Seq as Seq
import qualified Util.Serialize as Serialize
import           Util.Serialize (get, put)

import           Global
import           Types


-- | Duration is of one AbstractMeter, so total duration will be count*dur.
data Meter = Meter {
    meter_config :: !Config
    -- These are private because 'modify_sections' and 'clip_end' keep
    -- _meter_end up to date.
    , _meter_sections :: ![Section] -- I frequently modify the end, Seq, Vector?
    -- | Trim from the last Section by this amount.
    , _meter_end :: !TrackTime
    } deriving (Eq, Show)

-- | Instances are in here instead of Cmd.Serialize to avoid exporting the
-- record fields.  It's probably not a big deal if I do though.
instance Serialize.Serialize Meter where
    put (Meter a b c) = Serialize.put_version 0 >> put a >> put b >> put c
    get = Serialize.get_version >>= \case
        0 -> Meter <$> get <*> get <*> get
        v -> Serialize.bad_version "Meter" v

instance Pretty Meter where
    format (Meter config sections end) =
        "Meter" <+> Pretty.format config
            <+> Pretty.format sections
            <+> Pretty.format end

instance Semigroup Meter where
    meter1 <> meter2 = meter (meter_config meter1)
        (meter_sections meter1 <> meter_sections meter2)
instance Monoid Meter where
    mempty = empty_meter
    mappend = (<>)

empty_meter :: Meter
empty_meter = Meter default_config [] 0

meter_sections :: Meter -> [Section]
meter_sections = _meter_sections

meter_end :: Meter -> TrackTime
meter_end = _meter_end

meter :: Config -> [Section] -> Meter
meter config sections = set_sections sections $
    modify_config (const config) empty_meter

data Section = Section {
    -- | The section contains this many measures.
    section_count :: !Measures
    -- | Each measure has this duration.
    , section_measure_duration :: !Duration
    -- | Describe a measure.
    , section_measure :: !AbstractMeter
    } deriving (Eq, Show)

instance Serialize.Serialize Section where
    put (Section a b c) = Serialize.put_version 0 >> put a >> put b >> put c
    get = Serialize.get_version >>= \case
        0 -> Section <$> get <*> get <*> get
        v -> Serialize.bad_version "Section" v

instance Pretty Section where
    format (Section measures dur measure) =
        "Section" <+> Pretty.format measures <+> Pretty.format dur
            <+> Pretty.format measure

type Measures = Int

section_duration :: Section -> Duration
section_duration (Section count dur _) = fromIntegral count * dur

modify_config :: (Config -> Config) -> Meter -> Meter
modify_config modify meter =
    meter { meter_config = modify (meter_config meter) }

set_sections :: [Section] -> Meter -> Meter
set_sections = modify_sections . const

modify_sections :: ([Section] -> [Section]) -> Meter -> Meter
modify_sections modify meter = meter
    { _meter_sections = sections
    , _meter_end = Num.sum (map section_duration sections)
    }
    where sections = modify (_meter_sections meter)

-- TODO: no good because trimming the AbstractMeter will throw off the labels!
clip_start :: TrackTime -> Meter -> Meter
clip_start start | start <= 0 = id
clip_start start = modify_sections clip
    where
    clip sections = case Seq.drop_before fst start (section_starts sections) of
        [] -> []
        (s, Section count dur meter) : sections
            -- Dropped an exact number of Sections.
            | s >= start -> Section count dur meter : map snd sections
            -- Dropped >= the final Section.
            | count2 <= 0 -> map snd sections
            -- Dropped an exact number of measures.
            | ts == 0 -> Section count2 dur meter : map snd sections
            -- Otherwise I dropped a fractional measure and must split a
            -- Section.
            | otherwise -> concat
                [ maybe [] (:[]) $ -- Debug.trace "dropped" $
                    Section 1 (dur - fromIntegral ts * tlen) <$>
                        drop_meter ts meter
                , if count2 <= 1 then [] else [Section (count2 - 1) dur meter]
                , map snd sections
                ]
            where
            (measures, frac) = -- Debug.trace_ret "mfrac" (start, s, dur) $
                properFraction $ (start - s) / dur
            count2 = count - measures
            tlen = dur / fromIntegral (meter_length meter)
            ts = floor $ (frac * dur) / tlen

clip_end :: TrackTime -> Meter -> Meter
clip_end end meter
    | end >= _meter_end meter = meter
    | otherwise = meter
        { _meter_sections = sections
        , _meter_end = min end (Num.sum (map section_duration sections))
        }
    where
    sections = Seq.takeWhileS 0 wanted (meter_sections meter)
    wanted total_dur (Section count dur _)
        | total_dur > end = Nothing
        | otherwise = Just $ total_dur + fromIntegral count * dur

section_starts :: [Section] -> [(Duration, Section)]
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
    config_labeled_ranks :: !(Set RankName)
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

instance Serialize.Serialize Config where
    put (Config a b c d e) = Serialize.put_version 0
        >> put a >> put b >> put c >> put d >> put e
    get = Serialize.get_version >>= \case
        0 -> Config <$> get <*> get <*> get <*> get <*> get
        v -> Serialize.bad_version "Config" v

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

-- * Rank

-- Also used by Derive.Typecheck
data RankName = SectionR | W | H | Q | E | S | T32 | T64 | T128 | T256
    deriving (Show, Eq, Ord, Bounded, Enum)
instance Pretty RankName where pretty = showt

instance Serialize.Serialize RankName where
    put a = Serialize.put_version 0 >> Serialize.put_enum a
    get = Serialize.get_version >>= \case
        0 -> Serialize.get_enum
        v -> Serialize.bad_version "RankName" v

type Rank = Int -- same as Mark.Rank

all_ranks :: [RankName]
all_ranks = [minBound .. maxBound]

name_to_rank :: RankName -> Rank
name_to_rank = fromEnum

data LabelConfig = BigNumber Int | Cycle [Label]
    deriving (Eq, Show)
type Label = Text -- TODO make it ByteString so I can pass to c++ efficiently?

instance Pretty LabelConfig where pretty = showt

instance Serialize.Serialize LabelConfig where
    put a = Serialize.put_version 0 >> case a of
        BigNumber a -> Serialize.put_tag 0 >> put a
        Cycle a -> Serialize.put_tag 1 >> put a
    get = Serialize.get_version >>= \case
        0 -> Serialize.get_tag >>= \case
            0 -> BigNumber <$> get
            1 -> Cycle <$> get
            t -> Serialize.bad_tag "LabelConfig" t
        v -> Serialize.bad_version "LabelConfig" v

-- | These are the conventional meanings for the ranks.
r_section, r_1, r_2, r_4, r_8, r_16, r_32, r_64, r_128, r_256 :: Rank
r_section : r_1 : r_2 : r_4 : r_8 : r_16 : r_32 : r_64 : r_128 : r_256 : _ =
  [0..]

-- | These are mnemonics for staff notation durations, though they may not
-- correspond exactly, as documented in "Cmd.Meter".
rank_names :: [(Rank, Text)]
rank_names = zip [0..] (map (Text.toLower . showt) all_ranks)

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

instance Pretty AbstractMeter where pretty = showt

instance Serialize.Serialize AbstractMeter where
    put = \case
        T -> Serialize.put_tag 0
        D ts -> Serialize.put_tag 1 >> mapM_ put ts
    get = Serialize.get_tag >>= \case
        0 -> pure T
        1 -> D <$> get
        tag -> Serialize.bad_tag "AbstractMeter" tag

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

-- | Drop the number of Ts.
drop_meter :: Int -> AbstractMeter -> Maybe AbstractMeter
drop_meter n m = case snd $ go n [m] of
    [] -> Nothing
    [m] -> Just m
    _ -> error "unreached"
    where
    go :: Int -> [AbstractMeter] -> (Int, [AbstractMeter])
    go n = \case
        ts | n <= 0 -> (0, ts)
        [] -> (n, [])
        D subs : ts
            | n2 <= 0 && not (null subs2) -> (0, D subs2 : ts)
            | otherwise -> go n2 ts
            where (n2, subs2) = go n subs
        T : ts -> go (n-1) ts
