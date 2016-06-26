-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ImplicitParams #-}
-- | Provide short names and operators for writing korvais in haskell.
-- This module is meant to be imported unqualified.
module Derive.Solkattu.Dsl (
    -- * solkattu
    Sequence, Korvai, Matras, Stroke, MNote
    -- ** sollus
    , (.)
    , __, __3, __4, __5, __n

    , dheem, dhom, di, din, dit, ga, gin, ka, ki
    , ku, mi, na, ri, ta, tam, tat, tha, thom, ti
    , tang, lang
    , dinga

    -- ** directives
    , nadai
    , speed, s2
    , (!)
    , at0, atX, (^)
    -- ** patterns
    , pat, p5, p6, p7, p8, p9, p666, p567, p765
    -- ** combinators
    , tri, tri_, trin
    , duration_of, repeat, join
    , reduce, reduce3
    , reduceR, reduceR3
    -- * transform
    , dropM, takeM, rdropM
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
import qualified Derive.Solkattu.Solkattu as Solkattu
import Derive.Solkattu.Solkattu
       (Sequence, Korvai, Matras, Note(..), Sollu(..), Stroke(..),
        MNote(..), check, duration_of, dropM, takeM, rdropM)
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

class Rest a where __ :: a
instance Rest Sequence where __ = sq Rest
instance Rest MNote where __ = MRest

-- | These are meant to suffix a sollu.  Since the sollu is considered part of
-- the duration, the number is one higher than the number of rests.  E.g.
-- @din.__3@ is a 3 count, and equivalent to @din.__.__@.
__3, __4, __5 :: Sequence
__3 = __n 3
__4 = __n 4
__5 = __n 5

-- | MNote is not a monoid like Sequence, so this can't emit a Rest.
__n :: Int -> Sequence
__n n = repeat (n-1) __

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
ri = sollu Ri
ta = sollu Ta
tam = sollu Tam
tat = sollu Tat
tha = sollu Tha
thom = sollu Thom
ti = sollu Ti

tang, lang :: Sequence
tang = sollu Tang <> __
lang = sollu Lang <> __

dinga :: Sequence
dinga = din <> __ <> ga

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
trin sep a b c = join sep [a, b, c]

p5, p6, p7, p8, p9 :: Sequence
p5 = pat 5
p6 = pat 6
p7 = pat 7
p8 = pat 8
p9 = pat 9

p666, p567, p765 :: Sequence -> Sequence
p666 sep = trin sep (pat 6) (pat 6) (pat 6)
p567 sep = trin sep (pat 5) (pat 6) (pat 7)
p765 sep = trin sep (pat 7) (pat 6) (pat 5)

repeat :: Monoid a => Int -> a -> a
repeat n p = mconcat (replicate n p)

join :: Sequence -> [Sequence] -> Sequence
join with = List.intercalate with

reduce :: Matras -> Sequence -> [Sequence]
reduce n = iterate (dropM n)

reduce3 :: Matras -> Sequence -> Sequence -> Sequence
reduce3 n sep seq = join sep $ take 3 $ reduce n seq

reduceR :: Matras -> Sequence -> [Sequence]
reduceR n = iterate (rdropM n)

reduceR3 :: Matras -> Sequence -> Sequence -> Sequence
reduceR3 n sep seq = join sep $ take 3 $ reduceR n seq

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

-- | @do@ would match score notation, but @do@ is a keyword.
-- Ultimately that's because score uses + for tha, and +o is an attr, while o+
-- is a bareword.  But perhaps I should change + to p in the score, and then
-- the left hand can go on the left side?
od :: MNote
od = MNote (Both Solkattu.MThom Solkattu.MDin)

pk :: MNote
pk = MNote (Both Solkattu.MTha Solkattu.MKi)

default_patterns :: Solkattu.Patterns
default_patterns = check $ Solkattu.patterns
    [ (5, [k, t, k, n, o])
    , (6, [k, t, __, k, n, o])
    , (7, [k, __, t, __, k, n, o])
    , (9, [k, __, t, __, k, __, n, __, o])
    ]

patterns2 :: Solkattu.Patterns
patterns2 = check $ Solkattu.patterns
    [ (5, [k, n, k, n, o])
    , (7, [k, t, __, k, n, __, o])
    , (9, [k, t, __, __, k, n, __, __, o])
    ]
