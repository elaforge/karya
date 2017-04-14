-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | The 'Tala' type, which describes a Carnatic tala.
module Derive.Solkattu.Tala where
import qualified Util.Pretty as Pretty
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

tala_aksharas :: Tala -> [Akshara]
tala_aksharas (Tala angas jati) = map (anga_aksharas jati) angas

data Tala = Tala ![Anga] !Jati deriving (Show, Eq)

instance Pretty.Pretty Tala where pretty = showt

data Anga = Clap !Akshara | Wave !Akshara
    -- | laghu, drutam, anudrutam
    | I | O | U
    deriving (Show, Eq)

instance Pretty.Pretty Anga where pretty = showt

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
    -- These have no laghu, so jati doesn't matter.

khanda_chapu :: Tala
khanda_chapu = Tala [Clap 2, Clap 1, Clap 2] 0

rupaka_fast :: Tala
rupaka_fast = Tala [Clap 1, Clap 1, Wave 1] 0
