-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
{- | This is analogous to "Solkattu.Dsl.Solkattu", except it exports
    a mridangam-specific notation without using sollus at all.

    Its sollu type is just 'Mridangam.Stroke', so it doesn't need a StrokeMap.
-}
module Solkattu.Dsl.Mridangam (
    module Solkattu.Dsl.Mridangam
    , module Solkattu.Dsl.Generic
    , module Solkattu.Dsl.Interactive
) where
import Prelude hiding ((.))

import qualified Util.CallStack as CallStack
import qualified Util.Seq as Seq
import Solkattu.Dsl.Interactive (diff, diffw)
import qualified Solkattu.Dsl.MridangamNotation as MridangamNotation
import qualified Solkattu.Dsl.Solkattu as Dsl.Solkattu
import qualified Solkattu.Format.Format as Format
import qualified Solkattu.Instrument.Mridangam as Mridangam
import qualified Solkattu.Korvai as Korvai
import qualified Solkattu.Realize as Realize
import qualified Solkattu.S as S
import qualified Solkattu.Solkattu as Solkattu
import qualified Solkattu.Tala as Tala

import Solkattu.Dsl.Generic


type Sequence = SequenceT Stroke
type Stroke = Realize.Stroke Mridangam.Stroke
type Section = Korvai.Section Stroke

-- | Merge a sequence of left hand strokes with one of right hand strokes.
-- Both sequences must have the same length and structure.
(&) :: CallStack.Stack => Sequence -> Sequence -> Sequence
(&) = MridangamNotation.merge

korvai :: Tala.Tala -> [Section] -> Korvai.Korvai
korvai tala = Korvai.mridangamKorvai tala Mridangam.defaultPatterns

korvai1 :: Tala.Tala -> Section -> Korvai.Korvai
korvai1 tala section = korvai tala [section]

-- | Infer Section types, as init is development, last is ending.
korvaiS :: Tala.Tala -> [Sequence] -> Korvai.Korvai
korvaiS tala =
    Korvai.mridangamKorvaiInferSections tala Mridangam.defaultPatterns

korvaiS1 :: Tala.Tala -> Sequence -> Korvai.Korvai
korvaiS1 tala sequence = korvaiS tala [sequence]

Mridangam.Strokes {..} = Mridangam.notes

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
o1 = Seq.map_head $ S.map1 $ fmap $ fmap $
    Mridangam.addThoppi Mridangam.Thom

mapMStroke :: (Mridangam.Stroke -> Mridangam.Stroke) -> Sequence -> Sequence
mapMStroke = fmap • fmap • fmap • fmap

mapNote :: (Solkattu.Note Stroke -> Solkattu.Note Stroke)
    -> Sequence -> Sequence
mapNote = fmap • fmap

-- * fragments

-- | Taka and takatiku from solkattu.
kp, kpnp :: Sequence
kp = k.p
kpnp = k.p.n.p

tdgnt :: Sequence
tdgnt = k.t.k.n.o

kt, tk, ktkt :: Sequence
kt = k.t
tk = t.k
ktkt = k.t.k.t

ktpk :: Sequence
ktpk = k.t.p.k

pk :: Sequence
pk = p.k

takadinna, kook :: Sequence
takadinna = k.o.o.k
kook = k.o.o.k

nakatiku :: Sequence
nakatiku = namedT Solkattu.GPattern "8n" (n.p.u.p.k.t.p.k)
    -- also t.p.u.k.t.p.k

-- * interactive utilities

realize, realizep :: Korvai.Korvai -> IO ()
realize = realizeM mempty
realizep = realizeM Format.defaultAbstraction

realizeM :: Abstraction -> Korvai.Korvai -> IO ()
realizeM = Dsl.Solkattu._printInstrument Korvai.mridangam
