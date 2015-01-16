-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Meters for Carnatic music.

    The main data type is 'Meter.LabeledMeter', produced by 'make_meter', which
    can be easily turned into a 'Ruler.Ruler' via 'ruler' if needed.

    E.g., 3 avartanams of adi talam chatusra nadai followed by 4 avartanams of
    tisra nadai: @make_meter [Ruler adi_tala 1 3 4 1, Ruler adi_tala 2 4 3 1]@
-}
module Cmd.Tala (
    ruler
    -- * standard talams
    , simple_ruler, simple
    , adi, adi3
    , dhruva, matya, rupaka, jhampa, triputa, ata, eka
    , adi_tala, dhruva_tala, matya_tala, rupaka_tala, jhampa_tala, triputa_tala
    , ata_tala, eka_tala
    , misra_chapu, khanda_chapu, rupaka_fast
    -- * define talams
    , Ruler(..), Sections, Avartanams, Nadai
    , make_meter
) where
import qualified Data.List as List

import qualified Ui.Ruler as Ruler
import qualified Cmd.Meter as Meter
import Cmd.Meter (AbstractMeter(..))


ruler :: Meter.LabeledMeter -> Ruler.Ruler
ruler = Ruler.meter_ruler (Just Meter.mtype_tala) . Meter.labeled_marklist

-- * standard talams

simple_ruler :: Tala -> Nadai -> Sections -> Ruler.Ruler
simple_ruler tala nadai sections = ruler $ simple tala nadai sections

-- | 4 avartanams of the given tala.
simple :: Tala -> Nadai -> Sections -> Meter.LabeledMeter
simple tala nadai sections = make_meter [Ruler tala sections 4 nadai 1]

-- | n sections of 4 avartanams of everyone's favorite talam.
adi :: Sections -> Ruler.Ruler
adi = simple_ruler adi_tala 4

-- | 'adi' but in tisram.
adi3 :: Sections -> Ruler.Ruler
adi3 = simple_ruler adi_tala 3

-- * implementation

data Tala = Tala ![Anga] !Jati deriving (Show, Eq)
data Anga = Clap !Int | Wave !Int
    -- | laghu, drutam, anudrutam
    | I | O | U
    deriving (Show, Eq)
type Jati = Int

dhruva, matya, rupaka, jhampa, triputa, ata, eka :: [Anga]
dhruva = [I, O, I, I]
matya = [I, O, I]
rupaka = [O, I]
jhampa = [I, U, O]
triputa = [I, O, O]
ata = [I, I, O, O]
eka = [I]

-- | Talas with default jati.
dhruva_tala, matya_tala, rupaka_tala, jhampa_tala, triputa_tala, ata_tala,
    eka_tala :: Tala
dhruva_tala = Tala dhruva 4
matya_tala = Tala matya 4
rupaka_tala = Tala rupaka 4
jhampa_tala = Tala jhampa 7
triputa_tala = Tala triputa 3
ata_tala = Tala ata 5
eka_tala = Tala eka 4

adi_tala :: Tala
adi_tala = Tala triputa 4 -- chatusra jati triputa tala

misra_chapu :: Tala
misra_chapu = Tala [Wave 1, Wave 2, Clap 2, Clap 2] 0

khanda_chapu :: Tala
khanda_chapu = Tala [Clap 2, Clap 1, Clap 2] 0

rupaka_fast :: Tala
rupaka_fast = Tala [Clap 1, Clap 1, Wave 1] 0

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
make_meter rulers = label_meter (make_labels labels) meter
    where
    meter = concatMap ruler_meter rulers
    labels = concatMap (tala_labels . ruler_tala) rulers

-- * implementation

label_meter :: [[Meter.Label]] -> Meter.Meter -> Meter.LabeledMeter
label_meter labels meter =
    [Meter.LabeledMark rank dur (Meter.join_label label)
        | (rank, dur, label) <- List.zip3 ranks ps all_labels]
    where
    (ranks, ps) = unzip (clean meter)
    all_labels = Meter.text_labels 2 labels $
        Meter.collapse_ranks unlabeled_ranks ranks
    -- Appending Meters can result in 0 dur marks in the middle.
    clean [] = []
    clean ((r, d) : meter)
        | d == 0 && not (null meter) = clean meter
        | otherwise = (r, d) : clean meter

-- Ranks (* marks labeled ranks):
-- r_section is several avarta -- sections -- *
-- r_1 avarta -- avartanams
-- r_2 anga -- based on tala -- *
-- r_4 akshara -- basad on akshara type and jati
-- r_8 nadai / gati -- variable -- *
-- r_16 -- 2 -- *
-- r_32 -- 2
-- r_64 -- 2 -- *
-- r_128 -- 2
-- r_256 -- 2

unlabeled_ranks :: [Ruler.Rank]
unlabeled_ranks =
    [Meter.r_section, Meter.r_2, Meter.r_16, Meter.r_64, Meter.r_128]

ruler_meter :: Ruler -> Meter.Meter
ruler_meter (Ruler tala sections avartanams nadai dur) =
    Meter.fit_meter total_dur $
        replicate sections $ Meter.repeat avartanams $ -- r_section, r_1
        Meter.subdivides [nadai, 2, 2, 2, 2, 2] $ -- r_8, r_16, r_32, r_64, ...
        tala_to_meter tala -- r_2, r_4
    where
    total_dur = dur * fromIntegral (anga_claps tala * avartanams * sections)

tala_to_meter :: Tala -> AbstractMeter
tala_to_meter (Tala angas jati) =
    D [D (replicate (anga_aksharas jati anga) T) | anga <- angas]

anga_claps :: Tala -> Int
anga_claps (Tala angas jati) = sum (map (anga_aksharas jati) angas)

anga_aksharas :: Int -> Anga -> Int
anga_aksharas jati anga = case anga of
    Clap n -> n
    Wave n -> n
    I -> jati
    O -> 2
    U -> 1

tala_labels :: Tala -> [Meter.Label]
tala_labels (Tala angas jati) = concatMap mk angas
    where
    mk anga = case anga of
        Clap n -> "X" : replicate (n-1) "-"
        Wave n -> "O" : replicate (n-1) "-"
        I -> take jati (Meter.count 0)
        O -> ["X", "O"]
        U -> ["X"]

make_labels :: [Meter.Label] -> [[Meter.Label]]
make_labels aksharas =
    [ numbers -- avarta
    , aksharas -- akshara
    , numbers -- nadai / gati
    , numbers, numbers, numbers, numbers
    ]
    where numbers = Meter.count 1
