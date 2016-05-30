-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ImplicitParams #-}
-- | Provide short names and operators for writing korvais in haskell.
-- This module is meant to be imported unqualified.
module Derive.Call.India.SolkattuDsl (
    -- * solkattu
    Sequence, Korvai, Matras, Stroke, MNote
    -- ** sollus
    , (.)
    , __, __2, __3, __4, __n
    , ta, tha, tat, di, ki, thom
    , na, ka, ti, ku, ri
    , din, gin
    , tam, tang, lang
    , dit, dheem
    -- ** directives
    , nadai
    , speed, s2
    , (!)
    , at0, atX, (^)
    -- ** patterns
    , pat, p5, p6, p7, p666, p567, p765
    -- ** combinators
    , tri, tri_, trin
    , duration_of, repeat, join
    -- * transform
    , dropM, takeM
    -- * mridangam
    , k, t, n, d, u, i, o, p
    , od, pk
    , default_patterns
    -- * misc
    , check, pprint
) where
import Prelude hiding ((.), (^), repeat)
import qualified Data.List as List
import qualified Data.Monoid as Monoid

import Util.Pretty (pprint)
import qualified Derive.Call.India.Solkattu as Solkattu
import Derive.Call.India.Solkattu
       (Sequence, Korvai, Matras, Note(..), Sollu(..), Stroke(..),
        MNote(..), check, duration_of, dropM, takeM)
import Global


-- | Combine 'Sequence's.  This is just another name for (<>).
(.) :: Monoid a => a -> a -> a
(.) = (Monoid.<>)
infixr 6 . -- same as <>

sq :: Note -> Sequence
sq = (:[])

sollu :: Sollu -> Sequence
sollu s = [Sollu s Nothing]

-- ** sollus

__ :: Sequence
__ = sq Rest
__2, __3 :: Sequence
__2 = __n 2
__3 = __n 3
__4 = __n 4

__n :: Int -> Sequence
__n n = repeat n __

ta, tha, di, ki, thom :: Sequence
ta = sollu Ta
tha = sollu Tha
tat = sollu Tat
di = sollu Di
ki = sollu Ki
thom = sollu Thom

na, ka, ti, ku, ri :: Sequence
na = sollu Na
ka = sollu Ka
ti = sollu Ta
ku = sollu Ka
ri = sollu Ri

din, gin :: Sequence
din = sollu Din
gin = sollu Gin

tam, tang, lang :: Sequence
tam = sollu Tam
tang = sollu Tang <> __
lang = sollu Lang <> __

dit, dheem :: Sequence
dit = sollu Dit
dheem = sollu Dheem

-- ** directives

nadai :: Matras -> Sequence
nadai n = [Solkattu.TimeChange (Solkattu.Nadai n)]

speed :: Solkattu.Speed -> Sequence
speed s = [Solkattu.TimeChange (Solkattu.Speed s)]

s2 :: Sequence -> Sequence
s2 seq = speed Solkattu.S2 <> seq <> speed Solkattu.S1

-- | Add a specific stroke instruction to a sollu.
stroke :: MNote -> Sequence -> Sequence
stroke _ [] = errorStack $ "stroke: empty sequence"
stroke (MNote stroke) (n:ns) = case n of
    Sollu s _ -> Sollu s (Just stroke) : ns
    _ -> errorStack $ "stroke: can't add stroke to " <> prettys n
stroke s _ = errorStack $ "st: require a sollu: " <> prettys s

(!) :: Sequence -> MNote -> Sequence
(!) = flip stroke

-- | Align at sam or the arudi.
at0, atX :: Sequence
at0 = sq $ Alignment (Solkattu.Akshara 0)
atX = sq $ Alignment Solkattu.Arudi

-- | Align at the given akshara.
(^) :: Sequence -> Solkattu.Aksharas -> Sequence
seq ^ n = sq (Alignment (Solkattu.Akshara n)) <> seq
infix 9 ^

pat :: Matras -> Sequence
pat d = sq $ Pattern d

-- | Repeat thrice, with no karvai.
tri :: Sequence -> Sequence
tri = tri_ mempty

-- | Repeat thrice, with the given separator.
tri_ :: Sequence -> Sequence -> Sequence
tri_ sep seq = join sep [seq, seq, seq]

-- | Three different patterns with the same separator.
trin :: Sequence -> Sequence -> Sequence -> Sequence -> Sequence
trin s a b c = join s [a, b, c]

p5, p6, p7 :: Sequence
p5 = pat 5
p6 = pat 6
p7 = pat 7

p666, p567, p765 :: Sequence -> Sequence
p666 sep = trin sep (pat 6) (pat 6) (pat 6)
p567 sep = trin sep (pat 5) (pat 6) (pat 7)
p765 sep = trin sep (pat 7) (pat 6) (pat 5)

repeat :: Monoid a => Int -> a -> a
repeat n p = mconcat (replicate n p)

join :: Sequence -> [Sequence] -> Sequence
join with = List.intercalate with

-- * mridangam

k, t, n, d, u, i, o, p :: MNote
k = MNote (Solkattu.Valantalai Solkattu.MKi)
t = MNote (Solkattu.Valantalai Solkattu.MTa)
n = MNote (Solkattu.Valantalai Solkattu.MNam)
d = MNote (Solkattu.Valantalai Solkattu.MDin)
u = MNote (Solkattu.Valantalai Solkattu.MChapu)
i = MNote (Solkattu.Valantalai Solkattu.MDheem)

p = MNote (Solkattu.Thoppi Solkattu.MTha)
o = MNote (Solkattu.Thoppi Solkattu.MThom)

-- | do would match score notation, but do is a keyword.
-- Ultimately that's because score uses + for tha, and +o is an attr, while o+
-- is a bareword.  But perhaps I should change + to p in the score, and then
-- the left hand can go on the left side?
od :: MNote
od = MNote (Both Solkattu.MThom Solkattu.MDin)

pk :: MNote
pk = MNote (Both Solkattu.MTha Solkattu.MKi)

___ :: MNote
___ = MRest

default_patterns :: Solkattu.Patterns
default_patterns = check $ Solkattu.patterns
    [ (5, [k, t, k, n, o])
    , (6, [k, t, ___, k, n, o])
    , (7, [k, ___, t, ___, k, n, o])
    ]
