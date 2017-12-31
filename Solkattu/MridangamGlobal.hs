-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
{- | This is analogous to "Solkattu.SolkattuGlobal", except it exports
    a mridangam-specific notation without using sollus at all.

    Its sollu type is just 'Mridangam.Stroke', so it doesn't need a StrokeMap.
-}
module Solkattu.MridangamGlobal (
    Sequence
    , (&)
    , korvai, korvai1
    , k, t, n, d, u, v, i, y, j, p, o, od
    , on, l
    , closed, thomLH, o1
    , lt, hv
    , module Solkattu.Dsl
    -- * fragments
    , tk, tktu, tdgnt
    , kt, ktkt, pk, kp
    , takadinna
) where
import Prelude hiding ((.))
import qualified Util.CallStack as CallStack
import qualified Util.Seq as Seq
import Solkattu.Dsl hiding ((&), lt, hv)
import qualified Solkattu.Instrument.Mridangam as Mridangam
import qualified Solkattu.Korvai as Korvai
import qualified Solkattu.Realize as Realize
import qualified Solkattu.Sequence as Sequence
import qualified Solkattu.Solkattu as Solkattu
import qualified Solkattu.Tala as Tala

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
        | isRest a = b
        | isRest b = a
        | otherwise = makeNote1 $
            Mridangam.bothRStrokes (toStroke1 a) (toStroke1 b)
    (pairs, trailing) = second (either id id) $ Seq.zip_remainder as bs
    isRest (Sequence.Note (Solkattu.Space Solkattu.Rest)) = True
    isRest _ = False

toStroke1 :: (CallStack.Stack, Pretty a, Pretty g) =>
    Sequence.Note g (Solkattu.Note a) -> a
toStroke1 (Sequence.Note (Solkattu.Note note)) = Solkattu._sollu note
toStroke1 note = errorStack $ "expected sollu: " <> pretty note

korvai :: Tala.Tala -> [Sequence] -> Korvai.Korvai
korvai tala = Korvai.mridangamKorvai tala Mridangam.defaultPatterns

korvai1 :: Tala.Tala -> Sequence -> Korvai.Korvai
korvai1 tala sequence = korvai tala [sequence]

makeNote1 :: stroke -> Sequence.Note g (Solkattu.Note stroke)
makeNote1 stroke = Sequence.Note $ Solkattu.Note $ Solkattu.note stroke

makeNote :: Stroke -> Sequence
makeNote stroke = [makeNote1 stroke]

mridangamStrokes :: Mridangam.Strokes Sequence
mridangamStrokes = makeNote • Realize.stroke <$> Mridangam.strokes

Mridangam.Strokes {..} = mridangamStrokes

on :: Sequence
on = o&n

-- | Thom -> tha.
closed :: Sequence -> Sequence
closed = mapMStroke $ \s -> case s of
    Mridangam.Thoppi Mridangam.Thom -> Mridangam.Thoppi Mridangam.Tha
    Mridangam.Both Mridangam.Thom a -> Mridangam.Both Mridangam.Tha a
    _ -> s

thomLH :: Sequence -> Sequence
thomLH = mapNote $ \note -> if note `elem` [n, d] then o else __
    where
    Mridangam.Strokes {..} = Solkattu.Note • Solkattu.note • Realize.stroke <$>
        Mridangam.strokes

-- | Add a 'o' to the first stroke.
o1 :: Sequence -> Sequence
o1 = Seq.map_head $ Sequence.map1 $ fmap $ fmap $
    Mridangam.addThoppi Mridangam.Thom

lt, hv :: Sequence -> Sequence
lt = mapStroke (\stroke -> stroke { Realize._emphasis = Realize.Light })
hv = mapStroke (\stroke -> stroke { Realize._emphasis = Realize.Heavy })

mapMStroke :: (Mridangam.Stroke -> Mridangam.Stroke) -> Sequence -> Sequence
mapMStroke = fmap • fmap • fmap • fmap

mapStroke :: (Stroke -> Stroke) -> Sequence -> Sequence
mapStroke = fmap • fmap • fmap

mapNote :: (Solkattu.Note Stroke -> Solkattu.Note Stroke)
    -> Sequence -> Sequence
mapNote = fmap • fmap

-- * fragments

-- | Taka and takatiku from solkattu.
tk, tktu :: Sequence
tk = k.p
tktu = k.p.n.p

tdgnt :: Sequence
tdgnt = k.t.k.n.o

kt, ktkt :: Sequence
kt = k.t
ktkt = k.t.k.t

pk, kp :: Sequence
pk = p.k
kp = k.p

takadinna :: Sequence
takadinna = k.o.o.k