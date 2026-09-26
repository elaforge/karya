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
    , make, makeUntil
    , talaToMeter
    , adi4, adi3, adi6
    , config
    , adi, dhruva, matya, rupaka, jhampa, triputa, ata, eka
    , misraChapu, kandaChapu, rupakaFast
) where
import qualified Data.Set as Set

import qualified Cmd.Ruler.RulerUtil as RulerUtil
import qualified Solkattu.Tala as Tala
import           Solkattu.Tala
    (Tala(..), adi, ata, dhruva, eka, jhampa, kandaChapu, matya, misraChapu,
     rupaka, rupakaFast, triputa)
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

-- TODO too many parameters, use a record?
make :: Tala -> Nadai -> Meter.Duration -> Avartanams -> Sections
    -> Meter.Meter
make tala nadai avartanamDur avartanams sections =
    Meter.meter (config tala) (replicate sections section)
    where
    section = Meter.MSection avartanams avartanamDur (talaToMeter tala nadai)

makeUntil :: Tala -> Nadai -> Meter.Duration -> TrackTime -> Meter.Meter
makeUntil tala nadai avartanamDur end =
    RulerUtil.meterTake end $ make tala nadai avartanamDur avartanams 1
    where avartanams = ceiling (end / avartanamDur)

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
    { configLabeledRanks = labeledRanks
    , configLabel = Meter.Cycle $ talaLabels tala
    , configStartMeasure = 1
    , configMinDepth = 1
    , configStripDepth = 2
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
labeledRanks :: Set Meter.Rank
labeledRanks = Set.fromList [Meter.W, Meter.Q, Meter.E, Meter.T32, Meter.T128]

talaToMeter :: Tala -> Nadai -> AbstractMeter
talaToMeter tala nadai =
    -- r_8, r_16, r_32, r_64, ...
    Meter.subdivides [nadai, 2, 2, 2, 2, 2] $ D
        -- r_2, r_4
        [ D (replicate (Tala.angaAksharas (Tala._jati tala) anga) T)
        | anga <- Tala._angas tala
        ]

talaLabels :: Tala -> [Meter.Label]
talaLabels tala = map Make.bigLabel $ concatMap mk (Tala._angas tala)
    where
    mk anga = case anga of
        Tala.Clap n -> "X" : replicate (n-1) "-"
        Tala.Wave n -> "O" : replicate (n-1) "-"
        Tala.I -> take (Tala._jati tala) (Make.countFrom 0)
        Tala.O -> ["X", "O"]
        Tala.U -> ["X"]
