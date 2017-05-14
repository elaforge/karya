-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Just the sollus, so "Derive.Solkattu.MridagamDsl" doesn't have to import
-- them along with "Derive.Solkattu.Dsl".
module Derive.Solkattu.DslSollu (
    dheem, dhom, di, din, dit, ga, gin, ka, ki, ku, mi, na, nam, nang, ri, ta,
    tam, tat, tha, thom, ti
    , tang, lang
    , dinga
) where
import qualified Derive.Solkattu.Sequence as Sequence
import qualified Derive.Solkattu.Solkattu as Solkattu
import Derive.Solkattu.Solkattu (Sollu(..))
import Global


type Sequence stroke = [Sequence.Note (Solkattu.Note stroke)]

sollu :: Solkattu.Sollu -> Sequence stroke
sollu s = make_note (Solkattu.Note (Solkattu.note s Nothing))

make_note :: a -> [Sequence.Note a]
make_note a = [Sequence.Note a]

dheem = sollu Dheem
dhom = sollu Dhom
di = sollu Di
din = sollu Din
dit = sollu Dit
ga = sollu Ga
gin = sollu Gin
ka = sollu Ka
ki = sollu Ki
ku = sollu Ku
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

dinga = din <> __ <> ga

__ :: Sequence a
__ = make_note Solkattu.Rest
