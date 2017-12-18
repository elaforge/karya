-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Provide short names and operators for writing korvais in haskell.
-- This module is the shared global namespace between
-- "Solkattu.SolkattuGlobal" and "Solkattu.MridangamGlobal".
module Solkattu.Dsl (
    (.), (•), ø
    , karvai

    -- ** directives
    , hv, lt
    , akshara, sam, (§)
    -- ** patterns
    , pat, p5, p6, p7, p8, p9, p666, p567, p765
    , nakatiku
    -- * re-exports
    , module Solkattu.Korvai
    , module Solkattu.Metadata
    , module Solkattu.Sequence
    , check, duration_of
    , module Solkattu.Notation
    , module Solkattu.Tala
    -- * mridangam
    , (&)
    -- * misc
    , pprint
    -- * realize
    , index
    , realize, realizep
    , realize_m, realize_k1, realize_r, realize_sargam
    -- * conveniences
    , ganesh, janahan, sriram, sudhindra
    , adi
) where
import qualified Prelude
import Prelude hiding ((.), (^), repeat)
import qualified Data.Monoid as Monoid

import qualified Util.CallStack as CallStack
import Util.Pretty (pprint)
import Solkattu.Instrument.Mridangam ((&))
import qualified Solkattu.Korvai as Korvai
import Solkattu.Korvai (Korvai, print_konnakol, write_konnakol_html)
import Solkattu.Metadata
import Solkattu.Notation
import qualified Solkattu.Realize as Realize
import qualified Solkattu.Sequence as S
import Solkattu.Sequence (Duration, Matra, Nadai)
import qualified Solkattu.Solkattu as Solkattu
import Solkattu.Solkattu (check, duration_of)
import qualified Solkattu.Tala as Tala
import Solkattu.Tala (Akshara)

import Global


-- | Combine 'Sequence's.  This is just another name for (<>).
(.) :: Monoid a => a -> a -> a
(.) = (Monoid.<>)
infixr 6 . -- same as <>

-- | Composition is still useful though.
(•) :: (b -> c) -> (a -> b) -> a -> c
(•) = (Prelude..)

-- | Synonym for mempty.  Opt-o on OS X.  It looks a little bit nicer when
-- the empty case takes less horizontal space than the non-empty case.
ø :: Monoid a => a
ø = mempty

make_note :: a -> [S.Note g a]
make_note a = [S.Note a]

-- ** sollus

-- | Make a single sollu 'Solkattu.Karvai'.
karvai :: (CallStack.Stack, Pretty sollu) => SequenceT sollu -> SequenceT sollu
karvai = modify_single_note $ Solkattu.modify_note $
    \note -> note { Solkattu._karvai = True }

-- ** directives

akshara :: Akshara -> SequenceT sollu
akshara n = make_note (Solkattu.Alignment n)

-- | Align at sam.
sam :: SequenceT sollu
sam = akshara 0

-- | Align at the given akshara.  I use § because I don't use it so often,
-- and it's opt-6 on OS X.
(§) :: SequenceT sollu -> Akshara -> SequenceT sollu
seq § n = make_note (Solkattu.Alignment n) <> seq
infix 9 §

-- * modify sollus

modify_single_note :: (CallStack.Stack, Pretty sollu) =>
    (Solkattu.Note sollu -> Solkattu.Note sollu)
    -> SequenceT sollu -> SequenceT sollu
modify_single_note modify (n:ns) = case n of
    S.Note note@(Solkattu.Note {}) -> S.Note (modify note) : ns
    S.TempoChange change sub ->
        S.TempoChange change (modify_single_note modify sub) : ns
    _ -> errorStack $ "expected a single note: " <> pretty n
modify_single_note _ [] = errorStack "expected a single note, but got []"

-- ** strokes

hv, lt :: (Pretty stroke, Pretty g, CallStack.Stack) =>
    S.Note g (Realize.Note stroke) -> S.Note g (Realize.Note stroke)
hv (S.Note (Realize.Note s)) =
    S.Note $ Realize.Note $ s { Realize._emphasis = Realize.Heavy }
hv n = errorStack $ "expected stroke: " <> pretty n

lt (S.Note (Realize.Note s)) =
    S.Note $ Realize.Note $ s { Realize._emphasis = Realize.Light }
lt n = errorStack $ "expected stroke: " <> pretty n

-- * patterns

pat :: Matra -> SequenceT sollu
pat d = make_note $ Solkattu.Pattern (Solkattu.PatternM d)

p5, p6, p7, p8, p9 :: SequenceT sollu
p5 = pat 5
p6 = pat 6
p7 = pat 7
p8 = pat 8
p9 = pat 9

p666, p567, p765 :: SequenceT sollu -> SequenceT sollu
p666 sep = trin sep (pat 6) (pat 6) (pat 6)
p567 sep = trin sep (pat 5) (pat 6) (pat 7)
p765 sep = trin sep (pat 7) (pat 6) (pat 5)

nakatiku :: SequenceT sollu
nakatiku = make_note $ Solkattu.Pattern Solkattu.Nakatiku


-- * realize util

index :: Int -> Korvai -> Korvai
index i korvai = case Korvai.korvai_sequences korvai of
    Korvai.Mridangam seqs ->
        korvai { Korvai.korvai_sequences = Korvai.Mridangam [seqs !! i] }
    Korvai.Sollu seqs ->
        korvai { Korvai.korvai_sequences = Korvai.Sollu [seqs !! i] }

realize, realizep :: Korvai.Korvai -> IO ()
realize = realize_m True
realizep = realize_m False

realize_m :: Bool -> Korvai.Korvai -> IO ()
realize_m = Korvai.print_instrument Korvai.mridangam

realize_k1 :: Bool -> Korvai.Korvai -> IO ()
realize_k1 = Korvai.print_instrument Korvai.kendang_tunggal

realize_r :: Bool -> Korvai.Korvai -> IO ()
realize_r = Korvai.print_instrument Korvai.reyong

realize_sargam :: Bool -> Korvai.Korvai -> IO ()
realize_sargam = Korvai.print_instrument Korvai.sargam


-- * conveniences

ganesh, janahan, sriram, sudhindra :: Korvai -> Korvai
ganesh = source "ganesh"
janahan = source "janahan"
sriram = source "sriram"
sudhindra = source "sudhindra"

adi :: Tala.Tala
adi = Tala.adi_tala
