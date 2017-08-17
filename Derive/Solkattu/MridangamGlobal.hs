-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
{- | This is analogous to "Derive.Solkattu.SolkattuGlobal", except it exports
    a mridangam-specific notation without using sollus at all.

    Its sollu type is just 'Mridangam.Stroke', so it doesn't need a StrokeMap.
-}
module Derive.Solkattu.MridangamGlobal (
    Sequence
    , (&)
    , korvai, korvai1
    , k, t, n, d, u, v, i, y, j, p, o, od
    , on, l
    , closed
    , lt, hv
    , module Derive.Solkattu.Dsl
    -- * fragments
    , kt, ktkt, pk, kp
) where
import Prelude hiding ((.))
import qualified Util.CallStack as CallStack
import qualified Util.Seq as Seq
import Derive.Solkattu.Dsl hiding ((&), lt, hv)
import qualified Derive.Solkattu.Instrument.Mridangam as Mridangam
import qualified Derive.Solkattu.Korvai as Korvai
import qualified Derive.Solkattu.Realize as Realize
import qualified Derive.Solkattu.Sequence as Sequence
import qualified Derive.Solkattu.Solkattu as Solkattu
import qualified Derive.Solkattu.Tala as Tala

import Global


type Sequence = SequenceT Stroke
type Stroke = Realize.Stroke Mridangam.Stroke

(&) :: CallStack.Stack => Sequence -> Sequence -> Sequence
(&) = merge

-- | Merge a sequence of left hand strokes with one of right hand strokes.
-- Both sequences must have the same length and structure.
merge :: CallStack.Stack => Sequence -> Sequence -> Sequence
merge as bs
    | not (null trailing) = errorStack $ "trailing strokes: " <> pretty trailing
    | otherwise = map merge1 pairs
    where
    merge1 (Sequence.TempoChange t1 n1, Sequence.TempoChange t2 n2)
        | t1 == t2 = Sequence.TempoChange t1 (merge n1 n2)
        | otherwise = errorStack $ "differing tempos: " <> pretty t1 <> " /= "
            <> pretty t2
    merge1 (a, b)
        | is_rest a = b
        | is_rest b = a
        | otherwise = make_note1 $
            Mridangam.both_rstrokes (to_stroke1 a) (to_stroke1 b)
    (pairs, trailing) = second (either id id) $ Seq.zip_remainder as bs
    is_rest (Sequence.Note (Solkattu.Space Solkattu.Rest)) = True
    is_rest _ = False

to_stroke1 :: (CallStack.Stack, Pretty a, Pretty g) =>
    Sequence.Note g (Solkattu.Note a) -> a
to_stroke1 (Sequence.Note (Solkattu.Note note)) = Solkattu._sollu note
to_stroke1 note = errorStack $ "expected sollu: " <> pretty note

korvai :: Tala.Tala -> [Sequence] -> Korvai.Korvai
korvai tala = Korvai.mridangam_korvai tala Mridangam.default_patterns

korvai1 :: Tala.Tala -> Sequence -> Korvai.Korvai
korvai1 tala sequence = korvai tala [sequence]

make_note1 :: stroke -> Sequence.Note g (Solkattu.Note stroke)
make_note1 stroke = Sequence.Note $ Solkattu.Note $ Solkattu.note stroke

make_note :: Stroke -> Sequence
make_note stroke = [make_note1 stroke]

mridangam_strokes :: Mridangam.Strokes Sequence
mridangam_strokes = make_note • Realize.stroke <$> Mridangam.strokes

Mridangam.Strokes {..} = mridangam_strokes

on :: Sequence
on = o&n

-- | Thom -> tha.
closed :: Sequence -> Sequence
closed = map_stroke $ \s -> case s of
    Mridangam.Thoppi Mridangam.Thom -> Mridangam.Thoppi Mridangam.Tha
    Mridangam.Both Mridangam.Thom a -> Mridangam.Both Mridangam.Tha a
    _ -> s

map_stroke :: (Mridangam.Stroke -> Mridangam.Stroke) -> Sequence -> Sequence
map_stroke = fmap • fmap • fmap • fmap

lt, hv :: Sequence -> Sequence
lt = modify_stroke (\stroke -> stroke { Realize._emphasis = Realize.Light })
hv = modify_stroke (\stroke -> stroke { Realize._emphasis = Realize.Heavy })

modify_stroke :: (Stroke -> Stroke) -> Sequence -> Sequence
modify_stroke modify = map (fmap (fmap modify))

-- * fragments

kt, ktkt :: Sequence
kt = k.t
ktkt = k.t.k.t

pk, kp :: Sequence
pk = p.k
kp = k.p
