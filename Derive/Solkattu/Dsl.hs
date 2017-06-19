-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-- | Provide short names and operators for writing korvais in haskell.
-- This module is meant to be imported unqualified.
module Derive.Solkattu.Dsl (
    (.), (•)
    , __, __2, __3, __4, __5, __6, __7, __8, __9, __n
    , karvai

    -- ** directives
    , (!), (<+>)
    , hv, lt
    , akshara, sam, (^), (§)
    -- ** patterns
    , pat, p5, p6, p7, p8, p9, p666, p567, p765
    , nakatiku, tk, tknk
    -- ** combinators
    , tri, tri_, trin, tri2
    , join, repeat, inter, spread
    , cmap, for, cfor, prefixes, suffixes, circum, prefix, suffix
    , accumulate
    -- * re-exports
    , module Derive.Solkattu.Korvai
    , module Derive.Solkattu.Metadata
    , module Derive.Solkattu.Sequence
    , check, duration_of
    , module Derive.Solkattu.Notation
    , module Derive.Solkattu.Tala
    -- * mridangam
    , stroke, (&)
    -- * misc
    , pprint
    -- * realize
    , index
    , realize, realizep
    , realize_m, realize_k1, realize_r
) where
import qualified Prelude
import Prelude hiding ((.), (^), repeat)
import qualified Data.List as List
import qualified Data.Monoid as Monoid

import qualified Util.CallStack as CallStack
import Util.Pretty (pprint)
import qualified Derive.Solkattu.Korvai as Korvai
import Derive.Solkattu.Korvai (Korvai, print_konnakol)
import Derive.Solkattu.Metadata
import Derive.Solkattu.Mridangam ((&))
import Derive.Solkattu.Notation hiding (Sequence)
import qualified Derive.Solkattu.Realize as Realize
import qualified Derive.Solkattu.Sequence as S
import Derive.Solkattu.Sequence (Duration, Matra, Nadai)
import qualified Derive.Solkattu.Solkattu as Solkattu
import Derive.Solkattu.Solkattu (check, duration_of)
import Derive.Solkattu.Tala (Akshara)

import Global


type Sequence stroke = [S.Note (Solkattu.Note stroke)]

-- | Combine 'Sequence's.  This is just another name for (<>).
(.) :: Monoid a => a -> a -> a
(.) = (Monoid.<>)
infixr 6 . -- same as <>

-- | Composition is still useful though.
(•) :: (b -> c) -> (a -> b) -> a -> c
(•) = (Prelude..)

make_note :: a -> [S.Note a]
make_note a = [S.Note a]

-- ** sollus

class Rest a where __ :: a
instance Rest (Sequence stroke) where __ = make_note Solkattu.Rest
instance Rest (Realize.SNote stroke) where __ = S.Note Realize.Rest

-- | These are meant to suffix a sollu.  Since the sollu is considered part of
-- the duration, the number is one higher than the number of rests.  E.g.
-- @din.__3@ is a 3 count, and equivalent to @din.__.__@.
__2, __3, __4, __5, __6, __7, __8, __9 :: Sequence stroke
__2 = __
__3 = __n 3
__4 = __n 4
__5 = __n 5
__6 = __n 6
__7 = __n 7
__8 = __n 8
__9 = __n 9

-- | 'Realize.Note' is not a monoid like 'Sequence', so this can't emit
-- a Rest.
__n :: Matra -> Sequence stroke
__n n = repeat (n-1) __

-- | Make a single sollu 'Solkattu.Karvai'.
karvai :: (CallStack.Stack, Pretty stroke) => Sequence stroke -> Sequence stroke
karvai = modify_single_note $ Solkattu.modify_note $
    \note -> note { Solkattu._karvai = True }

-- ** directives

akshara :: Akshara -> Sequence stroke
akshara n = make_note (Solkattu.Alignment n)

-- | Align at sam.
sam :: Sequence stroke
sam = akshara 0

-- | Align at the given akshara.  I use § because I don't use it so often,
-- and it's opt-6 on OS X.
(§) :: Sequence stroke -> Akshara -> Sequence stroke
seq § n = make_note (Solkattu.Alignment n) <> seq
infix 9 §

pat :: Matra -> Sequence stroke
pat d = make_note $ Solkattu.Pattern (Solkattu.PatternM d)

nakatiku :: Sequence stroke
nakatiku = make_note $ Solkattu.Pattern Solkattu.Nakatiku

tk, tknk :: Sequence stroke
tk = make_note $ Solkattu.Pattern Solkattu.Taka
tknk = make_note $ Solkattu.Pattern Solkattu.Takanaka

-- * modify sollus

-- | Infix operator to 'Solkattu.Tag' all of the sollus it applies to.
(^) :: (CallStack.Stack, Pretty stroke) => Int -> Sequence stroke
    -> Sequence stroke
(^) = set_tag
infix 9 ^

set_tag :: (CallStack.Stack, Pretty stroke) => Int -> Sequence stroke
    -> Sequence stroke
set_tag tag = fmap $ fmap $ Solkattu.modify_note $
    \note -> note { Solkattu._tag = Just tag }

modify_single_note :: (CallStack.Stack, Pretty stroke) =>
    (Solkattu.Note stroke -> Solkattu.Note stroke)
    -> Sequence stroke -> Sequence stroke
modify_single_note modify (n:ns) = case n of
    S.Note note@(Solkattu.Note {}) -> S.Note (modify note) : ns
    S.TempoChange change sub ->
        S.TempoChange change (modify_single_note modify sub) : ns
    _ -> errorStack $ "expected a single note: " <> pretty n
modify_single_note _ [] = errorStack "expected a single note, but got []"

-- ** strokes

-- | Add a specific stroke annotation to a sollu.
stroke :: (CallStack.Stack, Pretty stroke, Korvai.ToStroke stroke) =>
    stroke -> Sequence Korvai.Stroke -> Sequence Korvai.Stroke
stroke s = modify_single_note $
    Solkattu.modify_stroke (const (Just (Korvai.to_stroke s)))

-- | Add a specific stroke annotation to a sollu.
--
-- If e.g. mridangam strokes are \"imported\" via @Strokes {..} = ...@, then
-- just @sollu ! d@ works.  For non-imported, it would have to be
-- @sollu ! d <+> K.p@.
(!) :: (CallStack.Stack, Pretty stroke, Korvai.ToStroke stroke) =>
    Sequence Korvai.Stroke -> stroke -> Sequence Korvai.Stroke
(!) = flip stroke

-- | Combine strokes from different instruments.
(<+>) :: (Korvai.ToStroke a, Korvai.ToStroke b) => a -> b -> Korvai.Stroke
a <+> b = Korvai.to_stroke a <> Korvai.to_stroke b

hv, lt :: (Pretty stroke, CallStack.Stack) =>
    S.Note (Realize.Note stroke) -> S.Note (Realize.Note stroke)
hv (S.Note (Realize.Note s)) =
    S.Note $ Realize.Note $ s { Realize._emphasis = Realize.Heavy }
hv n = errorStack $ "expected stroke: " <> pretty n

lt (S.Note (Realize.Note s)) =
    S.Note $ Realize.Note $ s { Realize._emphasis = Realize.Light }
lt n = errorStack $ "expected stroke: " <> pretty n

-- ** structures

-- | Repeat thrice, with no karvai.
tri :: Sequence stroke -> Sequence stroke
tri = tri_ mempty

-- | Repeat thrice, with the given separator.
tri_ :: Sequence stroke -> Sequence stroke -> Sequence stroke
tri_ sep seq = join sep [seq, seq, seq]

-- | Three different patterns with the same separator.
trin :: Sequence stroke -> Sequence stroke -> Sequence stroke
    -> Sequence stroke -> Sequence stroke
trin sep a b c = join sep [a, b, c]

-- | Tirmanams with a variant final repeat.
tri2 :: Sequence stroke -> Sequence stroke -> Sequence stroke -> Sequence stroke
tri2 sep ab c = join sep [ab, ab, c]

p5, p6, p7, p8, p9 :: Sequence stroke
p5 = pat 5
p6 = pat 6
p7 = pat 7
p8 = pat 8
p9 = pat 9

p666, p567, p765 :: Sequence stroke -> Sequence stroke
p666 sep = trin sep (pat 6) (pat 6) (pat 6)
p567 sep = trin sep (pat 5) (pat 6) (pat 7)
p765 sep = trin sep (pat 7) (pat 6) (pat 5)

repeat :: Monoid a => Int -> a -> a
repeat n p = mconcat (replicate n p)

-- * sequences

join :: Sequence stroke -> [Sequence stroke] -> Sequence stroke
join = List.intercalate

-- | Intersperse between each stroke.
inter :: Sequence stroke -> Sequence stroke -> Sequence stroke
inter _ [] = []
inter sep (x:xs) = x : sep ++ inter sep xs

spread :: Matra -> Sequence stroke -> Sequence stroke
spread n = inter (__n n)

cmap :: Monoid b => (a -> b) -> [a] -> b
cmap = mconcatMap

for :: [a] -> (a -> b) -> [b]
for = flip map

cfor :: Monoid b => [a] -> (a -> b) -> b
cfor xs f = mconcatMap f xs

-- | Multiple prefixes on a single suffix.
prefixes :: Monoid a => [a] -> a -> a
prefixes prefixes suffix = mconcatMap (<>suffix) prefixes

suffixes :: Monoid a => a -> [a] -> a
suffixes prefix suffixes = mconcatMap (prefix<>) suffixes

circum :: Monoid a => a -> [a] -> a -> a
circum prefix mids suffix = mconcatMap (\m -> prefix <> m <> suffix) mids

suffix :: Monoid a => [a] -> a -> a
suffix seqs suf = mconcat $ map (<>suf) seqs

prefix :: Monoid a => a -> [a] -> a
prefix pref seqs = mconcat $ map (pref<>) seqs

-- | Succesively accumulate suffixes.
accumulate :: Monoid a => [a] -> [a]
accumulate = map mconcat • drop 1 • List.inits

-- * realize util

index :: Int -> Korvai -> Korvai
index i korvai =
    korvai { Korvai.korvai_sequences = [Korvai.korvai_sequences korvai !! i] }

realize, realizep :: Korvai.Korvai -> IO ()
realize = realize_m True
realizep = realize_m False

realize_m :: Bool -> Korvai.Korvai -> IO ()
realize_m = Korvai.print_instrument Korvai.mridangam

realize_k1 :: Bool -> Korvai.Korvai -> IO ()
realize_k1 = Korvai.print_instrument Korvai.kendang_tunggal

realize_r :: Bool -> Korvai.Korvai -> IO ()
realize_r = Korvai.print_instrument Korvai.reyong
