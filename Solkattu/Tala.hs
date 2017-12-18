-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | The 'Tala' type, which describes a Carnatic tala.
module Solkattu.Tala where
import Global


-- | An akshara is one count of the talam.
type Akshara = Int

anga_aksharas :: Jati -> Anga -> Akshara
anga_aksharas jati anga = case anga of
    Clap n -> n
    Wave n -> n
    I -> jati
    O -> 2
    U -> 1

anga_labels :: Jati -> Anga -> [Text]
anga_labels jati anga = case anga of
    Clap n -> "X" : replicate (n-1) "-"
    Wave n -> "O" : replicate (n-1) "-"
    I -> take jati (map showt [0..])
    O -> ["X", "O"]
    U -> ["X"]

tala_aksharas :: Tala -> [Akshara]
tala_aksharas tala = map (anga_aksharas (_jati tala)) (_angas tala)

tala_labels :: Tala -> [Text]
tala_labels tala = concatMap (anga_labels (_jati tala)) (_angas tala)

data Tala = Tala {
    _name :: !Text
    , _angas :: ![Anga]
    , _jati :: !Jati
    } deriving (Show, Eq)

instance Pretty Tala where pretty = showt

data Anga = Clap !Akshara | Wave !Akshara
    -- | laghu, drutam, anudrutam
    | I | O | U
    deriving (Show, Eq)

instance Pretty Anga where pretty = showt

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
dhruva_tala = Tala "dhruva" dhruva 4
matya_tala = Tala "matya" matya 4
rupaka_tala = Tala "rupaka" rupaka 4
jhampa_tala = Tala "jhampa" jhampa 7
triputa_tala = Tala "triputa" triputa 3
ata_tala = Tala "ata" ata 5
eka_tala = Tala "eka" eka 4

adi_tala :: Tala
adi_tala = Tala "adi" triputa 4 -- chatusra jati triputa tala

misra_chapu :: Tala
misra_chapu = Tala "misra chapu" [Wave 1, Wave 2, Clap 2, Clap 2] 0
    -- These have no laghu, so jati doesn't matter.

khanda_chapu :: Tala
khanda_chapu = Tala "khanda chapu" [Clap 2, Clap 1, Clap 2] 0

rupaka_fast :: Tala
rupaka_fast = Tala "rupaka" [Clap 1, Clap 1, Wave 1] 0
