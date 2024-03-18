-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Solkattu.Talas where
import qualified Data.Set as Set
import qualified GHC.Generics as Generics

import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Solkattu.Tala as Tala

import           Global


data Tala = Carnatic Tala.Tala | Hindustani Tal
    deriving (Eq, Show)

instance Pretty Tala where
    format = \case
        Carnatic tala -> Pretty.format tala
        Hindustani tala -> Pretty.format tala

aksharas :: Tala -> Tala.Akshara
aksharas = \case
    Carnatic tala -> Tala.tala_aksharas tala
    Hindustani tala -> talAksharas tala

angas :: Tala -> [Tala.Akshara]
angas = \case
    Carnatic tala -> Tala.tala_angas tala
    Hindustani tala -> map snd (talVibhags tala)

name :: Tala -> Text
name = \case
    Carnatic tala -> Tala.tala_name tala
    Hindustani tala -> talName tala

angaSet :: Tala -> Set Tala.Akshara
angaSet = Set.fromList .  scanl (+) 0 . angas

labels :: Tala -> [Text]
labels = \case
    Carnatic tala -> Tala.tala_labels tala
    Hindustani tala -> talLabels tala

-- * Tal

-- TODO I wound up using Carnatic Tala because I wound up using Korvai, but
-- I'll need some way to reconcile.
data Tal = Tal {
    talName :: Text
    , talVibhags :: [(Tali, Tala.Akshara)]
    } deriving (Eq, Show, Generics.Generic)

instance Pretty Tal where
    format = Pretty.formatGCamel

talAksharas :: Tal -> Tala.Akshara
talAksharas = Num.sum . map snd . talVibhags

talLabels :: Tal -> [Text]
talLabels = taliLabels . talVibhags

-- 1x 2 3 4 5x 6 7 8 9o 10 11 12 13x 14 15 16
taliLabels :: [(Tali, Tala.Akshara)] -> [Text]
taliLabels = map fmt . zip [1 ..] . concatMap expand
    where
    fmt (n, mb_tali) = showt n <> case mb_tali of
        Nothing -> ""
        Just Tali -> "x"
        Just Kali -> "o"
    expand (tali, aksharas) = Just tali : replicate (aksharas - 1) Nothing

data Tali = Tali | Kali
    deriving (Eq, Show)
instance Pretty Tali where pretty = showt

tintal :: Tal
tintal = Tal "tintal" [(Tali, 4), (Tali, 4), (Kali, 4), (Tali, 4)]

kehrwa :: Tal
kehrwa = Tal "kehrwa" [(Tali, 4), (Kali, 4)]

jhaptal :: Tal
jhaptal = Tal "jhaptal" [(Tali, 2), (Tali, 3), (Kali, 2), (Tali, 3)]
    -- dhin na dhin dhin na tin na dhin dhin na

rupak :: Tal
rupak = Tal "rupak" [(Kali, 3), (Tali, 2), (Tali, 2)]
    -- tin tin na dhin na dhin na

-- | Hindustani adi tal, for pakhawaj.
adi :: Tal
adi = Tal "adi" [(Tali, 2), (Tali, 2), (Kali, 2), (Tali, 2)]
