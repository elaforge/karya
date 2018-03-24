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
    , module Solkattu.Html
    , module Solkattu.MetadataGlobal
    , module Solkattu.SectionGlobal
    , module Solkattu.Sequence
    , module Solkattu.Solkattu
    , module Solkattu.Notation
    , module Solkattu.Tala
    -- * mridangam
    , (&)
    -- * misc
    , pprint
    -- * realize
    , index
    , realize, realizep
    , realizeM, realizeK1, realizeR, realizeSargam
    -- * talam
    , beats
    , adi
    -- * conveniences
    , ganesh, janahan, sriram, sudhindra
) where
import qualified Prelude
import Prelude hiding ((.), (^), repeat)
import qualified Data.Monoid as Monoid

import qualified Util.CallStack as CallStack
import Util.Pretty (pprint)
import Solkattu.Instrument.Mridangam ((&))
import qualified Solkattu.Korvai as Korvai
import Solkattu.Korvai (Korvai, printKonnakol, section, smap)
import Solkattu.Html (writeHtmlKorvai)
import Solkattu.MetadataGlobal
import Solkattu.SectionGlobal
import Solkattu.Notation
import qualified Solkattu.Realize as Realize
import qualified Solkattu.Sequence as S
import Solkattu.Sequence (Duration, Matra, Nadai, defaultTempo)
import qualified Solkattu.Solkattu as Solkattu
import Solkattu.Solkattu (check, durationOf, throw)
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

makeNote :: a -> [S.Note g a]
makeNote a = [S.Note a]

-- ** sollus

-- | Make a single sollu 'Solkattu.Karvai'.
karvai :: (CallStack.Stack, Pretty sollu) => SequenceT sollu -> SequenceT sollu
karvai = modifySingleNote $ Solkattu.modifyNote $
    \note -> note { Solkattu._karvai = True }

-- ** directives

akshara :: Akshara -> SequenceT sollu
akshara n = makeNote (Solkattu.Alignment n)

-- | Align at sam.
sam :: SequenceT sollu
sam = akshara 0

-- | Align at the given akshara.  I use § because I don't use it so often,
-- and it's opt-6 on OS X.
(§) :: SequenceT sollu -> Akshara -> SequenceT sollu
seq § n = makeNote (Solkattu.Alignment n) <> seq
infix 9 §

-- * modify sollus

modifySingleNote :: (CallStack.Stack, Pretty sollu) =>
    (Solkattu.Note sollu -> Solkattu.Note sollu)
    -> SequenceT sollu -> SequenceT sollu
modifySingleNote modify (n:ns) = case n of
    S.Note note@(Solkattu.Note {}) -> S.Note (modify note) : ns
    S.TempoChange change sub ->
        S.TempoChange change (modifySingleNote modify sub) : ns
    _ -> throw $ "expected a single note: " <> pretty n
modifySingleNote _ [] = throw "expected a single note, but got []"

-- ** strokes

hv, lt :: (Pretty stroke, Pretty g, CallStack.Stack) =>
    S.Note g (Realize.Note stroke) -> S.Note g (Realize.Note stroke)
hv (S.Note (Realize.Note s)) =
    S.Note $ Realize.Note $ s { Realize._emphasis = Realize.Heavy }
hv n = throw $ "expected stroke: " <> pretty n

lt (S.Note (Realize.Note s)) =
    S.Note $ Realize.Note $ s { Realize._emphasis = Realize.Light }
lt n = throw $ "expected stroke: " <> pretty n

-- * patterns

pat :: Matra -> SequenceT sollu
pat d = makeNote $ Solkattu.Pattern (Solkattu.PatternM d)

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
nakatiku = makeNote $ Solkattu.Pattern Solkattu.Nakatiku


-- * realize util

index :: Int -> Korvai -> Korvai
index i korvai = case Korvai.korvaiSections korvai of
    Korvai.Mridangam sections ->
        korvai { Korvai.korvaiSections = Korvai.Mridangam [sections !! i] }
    Korvai.Sollu sections ->
        korvai { Korvai.korvaiSections = Korvai.Sollu [sections !! i] }

realize, realizep :: Korvai.Korvai -> IO ()
realize = realizeM True
realizep = realizeM False

realizeM :: Bool -> Korvai.Korvai -> IO ()
realizeM = Korvai.printInstrument Korvai.mridangam

realizeK1 :: Bool -> Korvai.Korvai -> IO ()
realizeK1 = Korvai.printInstrument Korvai.kendangTunggal

realizeR :: Bool -> Korvai.Korvai -> IO ()
realizeR = Korvai.printInstrument Korvai.reyong

realizeSargam :: Bool -> Korvai.Korvai -> IO ()
realizeSargam = Korvai.printInstrument Korvai.sargam


-- * talam

-- | For a fragment which fits a certain number of beats.
beats :: Akshara -> Tala.Tala
beats aksharas = Tala.Tala "beats" [Tala.I] aksharas

adi :: Tala.Tala
adi = Tala.adi_tala

-- * conveniences

ganesh, janahan, sriram, sudhindra :: Korvai -> Korvai
ganesh = source "ganesh"
janahan = source "janahan"
sriram = source "sriram"
sudhindra = source "sudhindra"
