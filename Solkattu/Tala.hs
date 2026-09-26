-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | The 'Tala' type, which describes a Carnatic tala.
module Solkattu.Tala (
    Akshara
    , angaAksharas
    , talaAngas
    , talaAksharas
    , talaLabels
    , Tala(..)
    , talaName
    , Anga(..)
    -- * talas
    , dhruvaAnga, matyaAnga, rupakaAnga, jhampaAnga, triputaAnga
    , ataAnga, ekaAnga
    , dhruva, matya, rupaka, jhampa, triputa, ata, eka, adi
    , misraChapu
    , kandaChapu
    , rupakaFast
    , anyBeats
    , beats
) where
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Num as Num

import           Global


-- | An akshara is one count of the talam.
type Akshara = Int

angaAksharas :: Jati -> Anga -> Akshara
angaAksharas jati anga = case anga of
    Clap n -> n
    Wave n -> n
    I -> jati
    O -> 2
    U -> 1

angaLabels :: Jati -> Anga -> [Text]
angaLabels jati = \case
    Clap n -> "X" : replicate (n-1) "-"
    Wave n -> "O" : replicate (n-1) "-"
    I -> take jati (map showt [0..])
    O -> ["X", "O"]
    U -> ["X"]

talaAngas :: Tala -> [Akshara]
talaAngas tala = map (angaAksharas (_jati tala)) (_angas tala)

talaAksharas :: Tala -> Akshara
talaAksharas = Num.sum . talaAngas

talaLabels :: Tala -> [Text]
talaLabels tala = concatMap (angaLabels (_jati tala)) (_angas tala)

data Tala = Tala
    { _name :: !Text
    , _angas :: ![Anga]
    , _jati :: !Jati
    } deriving (Show, Eq)

instance Pretty Tala where pretty = showt

talaName :: Tala -> Text
talaName tala
    | tala == adi = "adi"
    | _angas tala == [I] = showt (_jati tala) <> " beats"
    | _jati tala == 0 = _name tala -- chapu talams don't have jati
    | otherwise = Text.unwords
        [ _name tala
        , Map.findWithDefault (showt (_jati tala)) (_jati tala) jatis
        , "jati"
        ]
    where
    jatis = Map.fromList
        [ (3, "tisra")
        , (4, "chatusra")
        , (5, "kanda")
        , (9, "sankirna")
        ]

data Anga = Clap !Akshara | Wave !Akshara
    -- | laghu, drutam, anudrutam
    | I | O | U
    deriving (Show, Eq)

instance Pretty Anga where pretty = showt

type Jati = Int

dhruvaAnga, matyaAnga, rupakaAnga, jhampaAnga, triputaAnga, ataAnga,
    ekaAnga :: [Anga]
dhruvaAnga = [I, O, I, I]
matyaAnga = [I, O, I]
rupakaAnga = [O, I]
jhampaAnga = [I, U, O]
triputaAnga = [I, O, O]
ataAnga = [I, I, O, O]
ekaAnga = [I]

-- | Talas with default jati.
dhruva, matya, rupaka, jhampa, triputa, ata, eka :: Tala
dhruva = Tala "dhruva" dhruvaAnga 4
matya = Tala "matya" matyaAnga 4
rupaka = Tala "rupaka" rupakaAnga 4
jhampa = Tala "jhampa" jhampaAnga 7
triputa = Tala "triputa" triputaAnga 3
ata = Tala "ata" ataAnga 5
eka = Tala "eka" ekaAnga 4

adi :: Tala
adi = Tala "adi" triputaAnga 4 -- chatusra jati triputa tala

misraChapu :: Tala
misraChapu = Tala "misra chapu" [Wave 1, Wave 2, Clap 2, Clap 2] 0
    -- These have no laghu, so jati doesn't matter.

kandaChapu :: Tala
kandaChapu = Tala "kanda chapu" [Clap 2, Clap 1, Clap 2] 0

rupakaFast :: Tala
rupakaFast = Tala "rupaka" [Clap 1, Clap 1, Wave 1] 0

-- | A special "tala" that just counts beats and turns off alignment checking.
-- I use 4 beats just to get a nice wrapping, but I should do wrapping
-- automatically.
anyBeats :: Tala
anyBeats = Tala "any beats" [I] 4

-- | For a fragment which fits a certain number of beats.
beats :: Akshara -> Tala
beats aksharas = Tala "beats" [I] aksharas
