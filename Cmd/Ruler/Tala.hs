-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Meters for Carnatic music.

    The main data type is 'Meter.LabeledMeter', produced by 'make_meter', which
    can be easily turned into a 'Ruler.Ruler' via 'ruler' if needed.

    E.g., 3 avartanams of adi talam chatusra nadai followed by 4 avartanams of
    tisra nadai: @make_meter [Ruler adi_tala 1 3 4 1, Ruler adi_tala 2 4 3 1]@
-}
module Cmd.Ruler.Tala (
    ruler
    -- * standard talams
    , simple, simple_meter
    , adi, adi3, adi6
    , adi_tala, dhruva_tala, matya_tala, rupaka_tala, jhampa_tala, triputa_tala
    , ata_tala, eka_tala
    , misra_chapu, khanda_chapu, rupaka_fast
    -- * define talams
    , Ruler(..), Sections, Avartanams, Nadai
    , make_meter
    , make_config, dummy_config
) where
import qualified Data.Set as Set

import qualified Ui.Ruler as Ruler
import qualified Cmd.Ruler.Meter as Meter
import Cmd.Ruler.Meter (AbstractMeter(..))
import qualified Solkattu.Tala as Tala
import Solkattu.Tala
       (Tala(..), adi_tala, dhruva_tala, matya_tala, rupaka_tala, jhampa_tala,
        triputa_tala, ata_tala, eka_tala, misra_chapu, khanda_chapu,
        rupaka_fast)

import Global


ruler :: Meter.LabeledMeter -> Ruler.Ruler
ruler = Ruler.meter_ruler (Meter.ruler_config dummy_config)
    . Meter.labeled_marklist

-- * standard talams

-- | Create a ruler from just one Tala.  Section is hardcoded to 1 since
-- usually there isn't a section structure.
simple :: Tala -> Nadai -> Avartanams -> Ruler.Ruler
simple tala nadai avartanams = ruler $ simple_meter tala nadai 1 avartanams

-- | 4 avartanams of the given tala.
simple_meter :: Tala -> Nadai -> Sections -> Avartanams -> Meter.LabeledMeter
simple_meter tala nadai sections avartanams = make_meter $ (:[]) $ Ruler
    { ruler_tala = tala
    , ruler_sections = sections
    , ruler_avartanams = avartanams
    , ruler_nadai = nadai
    , ruler_dur = 1
    }

-- | n avartanams of everyone's favorite talam.
adi :: Avartanams -> Ruler.Ruler
adi = simple adi_tala 4

-- | 'adi' but in tisram.
adi3 :: Avartanams -> Ruler.Ruler
adi3 = simple adi_tala 3

adi6 :: Avartanams -> Ruler.Ruler
adi6 = simple adi_tala 6

-- * define talams

data Ruler = Ruler {
    ruler_tala :: !Tala
    , ruler_sections :: !Sections
    , ruler_avartanams :: !Avartanams
    , ruler_nadai :: !Nadai
    , ruler_dur :: !Meter.Duration -- ^ Duration of each akshara.
    } deriving (Show)

type Sections = Int
type Avartanams = Int
type Nadai = Int

-- | Concatenate the rulers and make a meter.
make_meter :: [Ruler] -> Meter.LabeledMeter
make_meter rulers =
    Meter.label_meter (make_config labels) meter
    where
    meter = concatMap ruler_meter rulers
    labels = concatMap (tala_labels . ruler_tala) rulers

-- * implementation

make_config :: [Meter.Label] -> Meter.Config
make_config labels = Meter.Config
    { config_labeled_ranks = labeled_ranks
    , config_label_components = make_components labels
    , config_start_measure = 1
    , config_min_depth = 1
    , config_strip_depth = 2
    , config_name = "tala"
    }

-- | A config for when I don't need the 'config_label_components'.
-- TODO this is awkward.
dummy_config :: Meter.Config
dummy_config = make_config []

-- Ranks (* marks labeled ranks):
-- * r_section is several avartanam -- sections
-- * r_1 avartanam -- avartanams
-- * r_2 anga -- based on tala
-- * r_4 akshara -- basad on akshara type and jati
-- * r_8 nadai / gati -- variable
-- . r_16 -- 2
-- * r_32 -- 2
-- . r_64 -- 2
-- * r_128 -- 2
-- . r_256 -- 2
labeled_ranks :: Set Meter.RankName
labeled_ranks = Set.fromList [Meter.W, Meter.Q, Meter.E, Meter.T32, Meter.T128]

ruler_meter :: Ruler -> Meter.Meter
ruler_meter (Ruler tala sections avartanams nadai dur) =
    Meter.fit_meter total_dur $
        replicate sections $ Meter.repeat avartanams $ -- r_section, r_1
        Meter.subdivides [nadai, 2, 2, 2, 2, 2] $ -- r_8, r_16, r_32, r_64, ...
        tala_to_meter tala -- r_2, r_4
    where
    total_dur = dur * fromIntegral (anga_claps tala * avartanams * sections)

tala_to_meter :: Tala -> AbstractMeter
tala_to_meter tala = D
    [ D (replicate (Tala.anga_aksharas (Tala._jati tala) anga) T)
    | anga <- Tala._angas tala
    ]

anga_claps :: Tala -> Int
anga_claps tala =
    sum (map (Tala.anga_aksharas (Tala._jati tala)) (Tala._angas tala))

tala_labels :: Tala -> [Meter.Label]
tala_labels tala = map Meter.big_label $ concatMap mk (Tala._angas tala)
    where
    mk anga = case anga of
        Tala.Clap n -> "X" : replicate (n-1) "-"
        Tala.Wave n -> "O" : replicate (n-1) "-"
        Tala.I -> take (Tala._jati tala) (Meter.count_from 0)
        Tala.O -> ["X", "O"]
        Tala.U -> ["X"]

make_components :: [Meter.Label] -> Meter.LabelComponents
make_components aksharas = Meter.LabelComponents
    -- avartanam is the measure number
    [ aksharas -- akshara
    , numbers -- nadai / gati
    , numbers, numbers, numbers, numbers
    ]
    where
    numbers = Meter.count_from 1
