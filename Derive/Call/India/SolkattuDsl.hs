-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ImplicitParams #-}
-- | Provide short names and operators for writing korvais in haskell.
-- This module is meant to be imported unqualified.
module Derive.Call.India.SolkattuDsl (
    -- * solkattu
    Sequence, Korvai, Matras, Stroke
    -- ** sollus
    , (-)
    , __, __2, __3, __n
    , ta, di, ki, thom, thom_
    , na, ka, ti, ku, ri
    , din, din_, gin
    , dit, dheem
    , kar, st
    , at0, atX
    -- ** patterns
    , pat, pat_, p5, p6, p7, p666, p567, p765
    -- ** combinators
    , tri, tri_
    , duration, repeat, sep
    -- * transform
    , dropM
    -- * mridangam
    , k, t, n, d, u, i, o, p
    , od
    , simple_patterns
    -- * misc
    , check, pprint
) where
import Prelude hiding ((-), repeat)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid

import qualified Util.Log as Log
import Util.Pretty (pprint)
import qualified Derive.Call.India.Solkattu as Solkattu
import Derive.Call.India.Solkattu
       (Sequence, Korvai, Matras, Note(..), Karvai(..), Sollu(..), Stroke(..),
        check, duration, dropM)
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
__2 = __ - __
__3 = __ - __ - __

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

dit, dheem :: Sequence
dit = sollu Dit
dheem = sollu Dheem

-- | Put karvai after the sollus.
kar :: Log.Stack => Sequence -> Sequence
kar notes = case reverse notes of
    [] -> Solkattu.error_stack "kar: empty sequence"
    n : ns -> reverse $ case n of
        Sollu s _ stroke -> Sollu s Karvai stroke : ns
        Pattern d _ -> Pattern d Karvai : ns
        _ -> Solkattu.error_stack "kar: last not can't have karvai"

st :: Maybe Stroke -> Sequence -> Sequence
st _ [] = Solkattu.error_stack $ "st: empty sequence"
st Nothing _ = Solkattu.error_stack $ "st: stroke was a rest"
st (Just stroke) (n:ns) = case n of
    Sollu s karvai _ -> Sollu s karvai (Just stroke) : ns
    _ -> Solkattu.error_stack $ "st: can't add stroke to " <> pretty n

at0, atX :: Sequence
at0 = sq $ Alignment Solkattu.Sam
atX = sq $ Alignment Solkattu.Arudi

pat d = sq $ Pattern d NoKarvai
pat_ d = sq $ Pattern d Karvai

-- | Repeat three times, with karvai between the middle two.
tri :: Sequence -> Sequence
tri = repeat 3

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

k, t, n, d, u, i, o, p :: Maybe Stroke
k = Just (Solkattu.Valantalai Solkattu.MKi)
t = Just (Solkattu.Valantalai Solkattu.MTa)
n = Just (Solkattu.Valantalai Solkattu.MNam)
d = Just (Solkattu.Valantalai Solkattu.MDin)
u = Just (Solkattu.Valantalai Solkattu.MChapu)
i = Just (Solkattu.Valantalai Solkattu.MDheem)

p = Just (Solkattu.Thoppi Solkattu.MTha)
o = Just (Solkattu.Thoppi Solkattu.MThom)

-- | do would match score notation, but do is a keyword.
-- Ultimately that's because score uses + for tha, and +o is an attr, while o+
-- is a bareword.  But perhaps I should change + to p in the score, and then
-- the left hand can go on the left side?
od :: Maybe Stroke
od = Just (Both Solkattu.MThom Solkattu.MDin)

___ :: Maybe Stroke
___ = Nothing

simple_patterns :: Solkattu.Patterns
simple_patterns = Map.fromList
    [ (5, [k, t, k, n, o])
    , (6, [k, t, ___, k, n, o])
    , (7, [k, ___, t, ___, k, n, o])
    ]
