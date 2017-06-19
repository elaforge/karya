-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
{- | This is analogous to "Derive.Solkattu.SolkattuGlobal", except it exports
    a mridangam-specific notation without using sollus at all.

    It uses 'Solkattu.Solkattu' with 'Solkattu.NoSollu' and explicit strokes,
    so it doesn't need a StrokeMap.

    Originally I tried to generalize Solkattu to replace Sollu with a Stroke
    directly, but then I can't do a cancel_karvai.  Then I tried to generalize
    the Sollu part, but then I'd have to rewrite the functions in Korvai, and
    the types are already kind of too complicated.  So now I use NoSollu,
    with a hack in Pretty Solkattu to omit NoSollu when there is an explicit
    stroke.  This way is simpler and almost as good.
-}
module Derive.Solkattu.MridangamGlobal (
    Sequence
    , (&)
    , korvai, korvai1
    , k, t, n, d, u, i, y, j, p, o, od
    , on, l
    , closed
    , lt, hv
    , module Derive.Solkattu.Dsl
) where
import qualified Util.CallStack as CallStack
import Derive.Solkattu.Dsl hiding ((&), lt, hv)
import qualified Derive.Solkattu.Korvai as Korvai
import qualified Derive.Solkattu.Mridangam as Mridangam
import qualified Derive.Solkattu.Realize as Realize
import qualified Derive.Solkattu.Sequence as Sequence
import qualified Derive.Solkattu.Solkattu as Solkattu
import qualified Derive.Solkattu.Tala as Tala

import Global


type Sequence = [Sequence.Note (Solkattu.Note Stroke)]
type Stroke = Realize.Stroke Mridangam.Stroke

(&) :: CallStack.Stack => Sequence -> Sequence -> Sequence
a & b = make_note $ Mridangam.both_rstrokes (to_stroke a) (to_stroke b)

to_stroke :: CallStack.Stack => Sequence -> Stroke
to_stroke [Sequence.Note
    (Solkattu.Note (Solkattu.NoteT {_stroke = Just stroke }))] = stroke
to_stroke seq = errorStack $ "expected a single sollu: " <> showt seq

korvai :: Tala.Tala -> [Sequence] -> Korvai.Korvai
korvai tala sequences = Korvai.korvai tala
    (mempty { Korvai.inst_mridangam = instrument })
    (map convert sequences)

korvai1 :: Tala.Tala -> Sequence -> Korvai.Korvai
korvai1 tala sequence = korvai tala [sequence]

convert :: Sequence -> Korvai.Sequence
convert = map (fmap (fmap Korvai.to_stroke))

instrument :: Realize.Instrument Mridangam.Stroke
instrument =
    Solkattu.check $ Mridangam.instrument strokes Mridangam.default_patterns
    where strokes = []

make_note :: Stroke -> Sequence
make_note stroke = [Sequence.Note $ Solkattu.Note $
    Solkattu.note Solkattu.NoSollu (Just stroke)]

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

lt, hv :: CallStack.Stack => Sequence -> Sequence
lt = modify_stroke (\stroke -> stroke { Realize._emphasis = Realize.Light })
hv = modify_stroke (\stroke -> stroke { Realize._emphasis = Realize.Heavy })

modify_stroke :: (Stroke -> Stroke) -> Sequence -> Sequence
modify_stroke modify
    [n@(Sequence.Note (Solkattu.Note (Solkattu.NoteT { _stroke = Just _ })))] =
        [fmap (fmap modify) n]
    -- Actually just fmap would do this, but I want to crash if it doesn't
    -- exist.
modify_stroke _ ns = errorStack $ "expected a single note: " <> pretty ns
