-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Just the sollus, so "Solkattu.MridagamDsl" doesn't have to import
-- them along with "Solkattu.Dsl".
module Solkattu.DslSollu (
    dheem, dhom, di, din, dit, ga, gin, gu, ka, ki, ku, mi, na, nam, nang, ri
    , ta, tam, tat, tha, thom, ti, kum
    , tang, lang
) where
import qualified Solkattu.Sequence as Sequence
import qualified Solkattu.Solkattu as Solkattu
import Solkattu.Solkattu (Sollu(..))


type Sequence = [Sequence.Note Solkattu.Group (Solkattu.Note Solkattu.Sollu)]
    -- This is the same as in Korvai.

sollu :: Solkattu.Sollu -> Sequence
sollu s = makeNote (Solkattu.Note (Solkattu.note s))

makeNote :: a -> [Sequence.Note g a]
makeNote a = [Sequence.Note a]

dheem = sollu Dheem
dhom = sollu Dhom
di = sollu Di
din = sollu Din
dit = sollu Dit
ga = sollu Ga
gin = sollu Gin
gu = sollu Gu
ka = sollu Ka
ki = sollu Ki
ku = sollu Ku
kum = sollu Kum
mi = sollu Mi
na = sollu Na
nam = sollu Nam
nang = sollu Nang
ri = sollu Ri
ta = sollu Ta
tam = sollu Tam
tat = sollu Tat
tha = sollu Tha
thom = sollu Thom
ti = sollu Ti

tang = sollu Tang
lang = sollu Lang

__ :: Sequence
__ = makeNote (Solkattu.Space Solkattu.Rest)
