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
    , (-)
    , __, __2, __3, __4, __n
    , ta, di, ki, thom, thom_
    , na, ka, ti, ku, ri
    , din, din_, gin
    , tam, tang, lang
    , dit, dheem
    , kar, st
    , at0, atX
    -- ** patterns
    , pat, pat_, p5, p6, p7, p666, p567, p765
    -- ** combinators
    , tri, tri_n, tri_
    , duration_of, repeat, sep
    -- * transform
    , dropM, takeM
    -- * mridangam
    , k, t, n, d, u, i, o, p
    , od, pk
    , default_patterns, default_karvai
    -- * misc
    , check, pprint
) where
import Prelude hiding ((-), repeat)
import qualified Data.List as List
import qualified Data.Monoid as Monoid

import qualified Util.Log as Log
import Util.Pretty (pprint)
import qualified Derive.Call.India.Solkattu as Solkattu
import Derive.Call.India.Solkattu
       (Sequence, Korvai, Matras, Note(..), Karvai(..), Sollu(..), Stroke(..),
        MNote(..), check, duration_of, dropM, takeM)
import Global


-- | Combine 'Sequence's.  This is just another name for (<>).
--
-- I don't really like hijacking a common operator, but it should be visually
-- simpler lower-key than the sollus, and unfortunately the other other
-- operator that seems to fit that is (.), and I have a bad experience from
-- reusing that, since it's so common when building combinators.
--
-- > ta / __ / di / __ / ki / __
-- > ta . __ . di . __ . ki . __
-- > ta. __. di. __. ki. __
-- > ta - __ - di - __ - ki - __
(-) :: Monoid a => a -> a -> a
(-) = (Monoid.<>)

sq :: Note -> Sequence
sq = (:[])

sollu :: Sollu -> Sequence
sollu s = [Sollu s NoKarvai Nothing]

__ :: Sequence
__ = sq Rest
__2, __3 :: Sequence
__2 = __n 2
__3 = __n 3
__4 = __n 4

__n :: Int -> Sequence
__n n = repeat n __

ta, di, ki, thom, thom_ :: Sequence
ta = sollu Ta
di = sollu Di
ki = sollu Ki
thom = sollu Thom
thom_ = kar thom

na, ka, ti, ku, ri :: Sequence
na = sollu Na
ka = sollu Ka
ti = sollu Ta
ku = sollu Ka
ri = sollu Ri

din, din_, gin :: Sequence
din = sollu Din
din_ = kar din
gin = sollu Gin

tam, tang, lang :: Sequence
tam = sollu Tam
tang = sollu Tang - __
lang = sollu Lang - __

dit, dheem :: Sequence
dit = sollu Dit
dheem = sollu Dheem

-- | Put karvai after the sollus.
kar :: Log.Stack => Sequence -> Sequence
kar notes = case reverse notes of
    [] -> errorStack "kar: empty sequence"
    n : ns -> reverse $ case n of
        Sollu s _ stroke -> Sollu s Karvai stroke : ns
        Pattern d _ -> Pattern d Karvai : ns
        _ -> errorStack "kar: last not can't have karvai"

-- | Add a specific stroke instruction to a sollu.
st :: MNote -> Sequence -> Sequence
st _ [] = errorStack $ "st: empty sequence"
st (MNote stroke) (n:ns) = case n of
    Sollu s karvai _ -> Sollu s karvai (Just stroke) : ns
    _ -> errorStack $ "st: can't add stroke to " <> prettys n
st s _ = errorStack $ "st: require a sollu: " <> prettys s

at0, atX :: Sequence
at0 = sq $ Alignment Solkattu.Sam
atX = sq $ Alignment Solkattu.Arudi

pat d = sq $ Pattern d NoKarvai
pat_ d = sq $ Pattern d Karvai

-- | Repeat thrice, with no karvai.
tri :: Sequence -> Sequence
tri = repeat 3

-- | Repeat thrice, with the given separator.
tri_n :: Sequence -> Sequence -> Sequence
tri_n seq n = sep n [seq, seq, seq]

-- | Repeat thrice, with karvai between the middle two.
tri_ :: Sequence -> Sequence
tri_ p = kar p - kar p - p

p5, p6, p7 :: Sequence
p5 = pat 5
p6 = pat 6
p7 = pat 7

p666, p567, p765 :: Sequence
p666  = tri_ (pat 6)
p567 = pat_ 5 - pat_ 6 - pat 7
p765 = pat_ 7 - pat_ 6 - pat 5

repeat :: Monoid a => Int -> a -> a
repeat n p = mconcat (replicate n p)

sep :: Sequence -> [Sequence] -> Sequence
sep with = List.intercalate with

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

default_karvai :: Solkattu.Patterns
default_karvai = check $ Solkattu.patterns
    [ (4, [i, ___, ___, ___])
    ]
