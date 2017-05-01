-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-- | Provide short names and operators for writing korvais in haskell.
-- This module is meant to be imported unqualified.
module Derive.Solkattu.Dsl (
    -- * solkattu
    Korvai
    -- ** sollus
    , (.), (•)
    , __, __2, __3, __4, __5, __6, __7, __8, __9, __n
    , karv

    , dheem, dhom, di, din, dit, ga, gin, ka, ki
    , ku, mi, na, nam, nang, ri, ta, tam, tat, tha, thom, ti
    , tang, lang
    , dinga

    -- ** directives
    , (!), (<+>)
    , akshara, sam, (^)
    -- ** patterns
    , pat, p5, p6, p7, p8, p9, p666, p567, p765
    , nakatiku
    -- ** combinators
    , tri, tri_, trin
    , join, repeat, inter, spread
    -- * re-exports
    , module Derive.Solkattu.Sequence
    , module Derive.Solkattu.Solkattu
    , module Derive.Solkattu.Notation
    , module Derive.Solkattu.Tala
    -- * mridangam
    , stroke, (&)
    -- * misc
    , pprint
    -- * realize
    , realize_instrument, many
) where
import Prelude hiding ((.), (^), repeat)
import qualified Prelude
import qualified Data.List as List
import qualified Data.Monoid as Monoid
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import qualified Util.CallStack as CallStack
import qualified Util.Pretty as Pretty
import Util.Pretty (pprint)

import qualified Derive.Solkattu.Korvai as Korvai
import Derive.Solkattu.Korvai (Korvai)
import Derive.Solkattu.Mridangam ((&))
import Derive.Solkattu.Notation
import qualified Derive.Solkattu.Realize as Realize
import qualified Derive.Solkattu.Solkattu as S
import qualified Derive.Solkattu.Sequence as Sequence
import Derive.Solkattu.Sequence (Duration, Matra, Nadai)
import Derive.Solkattu.Tala (Akshara)
import Derive.Solkattu.Solkattu (check, duration_of)

import Global


-- | Combine 'Sequence's.  This is just another name for (<>).
(.) :: Monoid a => a -> a -> a
(.) = (Monoid.<>)
infixr 6 . -- same as <>

-- | Composition is still useful though.
(•) :: (b -> c) -> (a -> b) -> a -> c
(•) = (Prelude..)

make_note :: S.Solkattu stroke -> Sequence stroke
make_note n = [Sequence.Note n]

sollu :: S.Sollu -> Sequence stroke
sollu s = make_note (S.Sollu s S.NotKarvai Nothing)

-- ** sollus

class Rest a where __ :: a
instance Rest (Sequence stroke) where __ = make_note S.Rest
instance Rest (Realize.Note stroke) where __ = Sequence.Note Realize.Rest

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

-- | Make a single sollu 'S.Karvai'.
karv :: (CallStack.Stack, Pretty.Pretty stroke) =>
    Sequence stroke -> Sequence stroke
karv [Sequence.Note (S.Sollu s _ stroke)] =
    [Sequence.Note $ S.Sollu s S.Karvai stroke]
karv ns = errorStack $ "can only add karvai to a single stroke: " <> pretty ns

dheem = sollu S.Dheem
dhom = sollu S.Dhom
di = sollu S.Di
din = sollu S.Din
dit = sollu S.Dit
ga = sollu S.Ga
gin = sollu S.Gin
ka = sollu S.Ka
ki = sollu S.Ki
ku = sollu S.Ku
mi = sollu S.Mi
na = sollu S.Na
nam = sollu S.Nam
nang = sollu S.Nang
ri = sollu S.Ri
ta = sollu S.Ta
tam = sollu S.Tam
tat = sollu S.Tat
tha = sollu S.Tha
thom = sollu S.Thom
ti = sollu S.Ti

tang, lang :: Sequence stroke
tang = sollu S.Tang
lang = sollu S.Lang

dinga :: Sequence stroke
dinga = din <> __ <> ga

-- ** directives

akshara :: Akshara -> Sequence stroke
akshara n = make_note (S.Alignment n)

-- | Align at sam.
sam :: Sequence stroke
sam = akshara 0

-- | Align at the given akshara.
(^) :: Sequence stroke -> Akshara -> Sequence stroke
seq ^ n = make_note (S.Alignment n) <> seq
infix 9 ^

pat :: Matra -> Sequence stroke
pat d = make_note $ S.Pattern (S.PatternM d)

nakatiku :: Sequence stroke
nakatiku = make_note $ S.Pattern S.Nakatiku

-- ** strokes

-- | Add a specific stroke annotation to a sollu.
stroke :: (CallStack.Stack, Pretty.Pretty stroke, Korvai.ToStroke stroke) =>
    stroke -> Sequence Korvai.Stroke -> Sequence Korvai.Stroke
stroke _ [] = errorStack "stroke: empty sequence"
stroke stroke (n:ns) = case n of
    Sequence.Note (S.Sollu s karvai _) ->
        Sequence.Note (S.Sollu s karvai (Just (Korvai.to_stroke stroke))) : ns
    _ -> errorStack $ "stroke: can't add stroke to " <> pretty n

-- | Add a specific stroke annotation to a sollu.
--
-- If e.g. mridangam strokes are \"imported\" via @Strokes {..} = ...@, then
-- just @sollu ! d@ works.  For non-imported, it would have to be
-- @sollu ! d <+> K.p@.
(!) :: (Pretty.Pretty stroke, Korvai.ToStroke stroke) =>
    Sequence Korvai.Stroke -> stroke -> Sequence Korvai.Stroke
(!) = flip stroke

(<+>) :: (Korvai.ToStroke a, Korvai.ToStroke b) => a -> b -> Korvai.Stroke
a <+> b = Korvai.to_stroke a <> Korvai.to_stroke b

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

join :: Sequence stroke -> [Sequence stroke] -> Sequence stroke
join = List.intercalate

-- | Intersperse between each stroke.
inter :: Sequence stroke -> Sequence stroke -> Sequence stroke
inter _ [] = []
inter sep (x:xs) = x : sep ++ inter sep xs

spread :: Matra -> Sequence stroke -> Sequence stroke
spread n = inter (__n n)

-- * realize

realize_instrument :: Pretty.Pretty stroke => Korvai.GetInstrument stroke
    -> Bool -> Korvai.Korvai -> IO ()
realize_instrument instrument realize_patterns korvai = Text.IO.putStrLn $
    case Korvai.realize instrument realize_patterns korvai of
        Left err -> "ERROR:\n" <> err
        Right (notes, warning) ->
            Realize.format width (Korvai.korvai_tala korvai) notes
            <> if Text.null warning then "" else "\n" <> warning
    where
    width = 78

many :: (a -> IO ()) -> [a] -> IO ()
many f xs = sequence_ $ List.intersperse (putChar '\n') $ map put (zip [0..] xs)
    where put (i, x) = putStrLn ("---- " ++ show i) >> f x
