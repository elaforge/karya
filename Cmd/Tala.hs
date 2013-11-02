-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Meters for Carnatic music.
module Cmd.Tala where
import qualified Data.List as List

import qualified Ui.Ruler as Ruler
import qualified Cmd.Meter as Meter
import Cmd.Meter (AbstractMeter(..))


-- * presets

-- | 2 sections of 4 avartanams of everyone's favorite talam.
adi_ruler :: Ruler.Ruler
adi_ruler = make_ruler adi_tala 2 4 4 1

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

rupaka_c :: Tala
rupaka_c = Tala [Clap 1, Clap 1, Wave 1] 0

-- Ranks (* marks labelled ranks):
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

unlabelled_ranks :: [Ruler.Rank]
unlabelled_ranks =
    [Meter.r_section, Meter.r_2, Meter.r_16, Meter.r_64, Meter.r_128]

data Ruler = Ruler {
    ruler_tala :: !Tala
    , ruler_sections :: !Int
    , ruler_avartanams :: !Int
    , ruler_nadai :: !Int
    , ruler_dur :: !Meter.Duration -- ^ Duration of each akshara.
    } deriving (Show)

make_ruler :: Tala -> Int -> Int -> Int
    -> Meter.Duration -- ^ Duration of each akshara.
    -> Ruler.Ruler
make_ruler tala sections avartanams nadai dur =
    make_rulers [Ruler tala sections avartanams nadai dur]

make_rulers :: [Ruler] -> Ruler.Ruler
make_rulers rulers = Ruler.ruler [(Meter.meter, rulers_marklist rulers)]

-- | Create a marklist from multiple rulers concatenated.
rulers_marklist :: [Ruler] -> Ruler.Marklist
rulers_marklist rulers = meter_marklist (make_labels labels) meters
    where
    meters = concatMap ruler_meter rulers
    labels = concatMap (tala_labels . ruler_tala) rulers

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
        I -> take jati Meter.count0
        O -> ["X", "O"]
        U -> ["X"]

angas_to_labels :: Jati -> [Anga] -> [Meter.Label]
angas_to_labels jati = concatMap $ \anga -> case anga of
    Clap n -> "X" : replicate (n-1) "-"
    Wave n -> "O" : replicate (n-1) "-"
    I -> take jati Meter.count0
    O -> ["X", "O"]
    U -> ["X"]

meter_marklist :: [[Meter.Label]] -> Meter.Meter -> Ruler.Marklist
meter_marklist labels meter =
    Meter.make_marklist $ List.zip3 ranks ps all_labels
    where
    (ranks, ps) = unzip (clean meter)
    all_labels = Meter.text_labels 2 labels $
        Meter.collapse_ranks unlabelled_ranks ranks
    -- Appending Meters can result in 0 dur marks in the middle.
    clean [] = []
    clean ((r, d) : meter)
        | d == 0 && not (null meter) = clean meter
        | otherwise = (r, d) : clean meter

make_labels :: [Meter.Label] -> [[Meter.Label]]
make_labels aksharas =
    [ numbers -- avarta
    , aksharas -- akshara
    , numbers -- nadai / gati
    , numbers, numbers, numbers, numbers
    ]
    where numbers = Meter.count1
