-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Meters for Carnatic music.

    The main data type is 'Meter.LabeledMeter', produced by 'make_meter', which
    can be easily turned into a 'Ruler.Ruler' via 'ruler' if needed.

    E.g., 3 avartanams of adi talam chatusra nadai followed by 4 avartanams of
    tisra nadai: @make_meter [Ruler adi 1 3 4 1, Ruler adi 2 4 3 1]@
-}
module Cmd.Ruler.Tala (
    Sections, Avartanams, Nadai
    -- * standard talams
    , simple
    , make, make_until
    , tala_to_meter
    , adi4, adi3, adi6
    , config
    , adi, dhruva, matya, rupaka, jhampa, triputa, ata, eka
    , misra_chapu, kanda_chapu, rupaka_fast
) where
import qualified Data.Set as Set

import qualified Cmd.Ruler.RulerUtil as RulerUtil
import qualified Solkattu.Tala as Tala
import           Solkattu.Tala
    (Tala(..), adi, ata, dhruva, eka, jhampa, kanda_chapu, matya, misra_chapu,
     rupaka, rupaka_fast, triputa)
import qualified Ui.Meter.Make as Make
import qualified Ui.Meter.Meter as Meter
import           Ui.Meter.Meter (AbstractMeter(..))

import           Global
import           Types


type Sections = Int
type Avartanams = Int
type Nadai = Int

-- * standard talams

-- | Simplified version of 'make', avartanam dur and sections hardcoded to 1.
simple :: Tala -> Nadai -> Avartanams -> Meter.Meter
simple tala nadai avartanams = make tala nadai 1 avartanams 1

-- TODO too many parameters.. use a record?
make :: Tala -> Nadai -> Meter.Duration -> Avartanams -> Sections
    -> Meter.Meter
make tala nadai avartanam_dur avartanams sections =
    Meter.meter (config tala) (replicate sections section)
    where
    section = Meter.MSection avartanams avartanam_dur (tala_to_meter tala nadai)

make_until :: Tala -> Nadai -> Meter.Duration -> TrackTime -> Meter.Meter
make_until tala nadai avartanam_dur end =
    RulerUtil.meter_take end $ make tala nadai avartanam_dur avartanams 1
    where avartanams = ceiling (end / avartanam_dur)

-- | 4 avartanams of everyone's favorite talam.
adi4 :: Avartanams -> Meter.Meter
adi4 = simple Tala.adi 4

-- | 'adi' but in tisram.
adi3 :: Avartanams -> Meter.Meter
adi3 = simple Tala.adi 3

adi6 :: Avartanams -> Meter.Meter
adi6 = simple Tala.adi 6

-- * implementation

config :: Tala -> Meter.Config
config tala = Meter.Config
    { config_labeled_ranks = labeled_ranks
    , config_label = Meter.Cycle $ tala_labels tala
    , config_start_measure = 1
    , config_min_depth = 1
    , config_strip_depth = 2
    }

-- Ranks (* marks labeled ranks):
-- * r_section is several avartanams -- sections
-- * r_1 avartanam -- avartanams
-- * r_2 anga -- based on tala
-- * r_4 akshara -- basad on akshara type and jati
-- * r_8 nadai / gati -- variable
-- . r_16 -- 2
-- * r_32 -- 2
-- . r_64 -- 2
-- * r_128 -- 2
-- . r_256 -- 2
labeled_ranks :: Set Meter.Rank
labeled_ranks = Set.fromList [Meter.W, Meter.Q, Meter.E, Meter.T32, Meter.T128]

tala_to_meter :: Tala -> Nadai -> AbstractMeter
tala_to_meter tala nadai =
    -- r_8, r_16, r_32, r_64, ...
    Meter.subdivides [nadai, 2, 2, 2, 2, 2] $ D
        -- r_2, r_4
        [ D (replicate (Tala.anga_aksharas (Tala._jati tala) anga) T)
        | anga <- Tala._angas tala
        ]

tala_labels :: Tala -> [Meter.Label]
tala_labels tala = map Make.big_label $ concatMap mk (Tala._angas tala)
    where
    mk anga = case anga of
        Tala.Clap n -> "X" : replicate (n-1) "-"
        Tala.Wave n -> "O" : replicate (n-1) "-"
        Tala.I -> take (Tala._jati tala) (Make.count_from 0)
        Tala.O -> ["X", "O"]
        Tala.U -> ["X"]
