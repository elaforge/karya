-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
{- | This is analogous to "Derive.Solkattu.Dsl", except it exports
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
module Derive.Solkattu.MridangamDsl (
    Sequence
    , (&)
    , korvai
    , k, t, n, d, u, i, y, j, p, o, od
    , on, l
    , closed
    , module Derive.Solkattu.Dsl
) where
import qualified Util.CallStack as CallStack
import Derive.Solkattu.Dsl hiding ((&))
import qualified Derive.Solkattu.Korvai as Korvai
import qualified Derive.Solkattu.Mridangam as Mridangam
import qualified Derive.Solkattu.Realize as Realize
import qualified Derive.Solkattu.Sequence as Sequence
import qualified Derive.Solkattu.Solkattu as Solkattu
import qualified Derive.Solkattu.Tala as Tala

import Global


type Sequence = [Sequence.Note (Solkattu.Solkattu Mridangam.Stroke)]

(&) :: CallStack.Stack => Sequence -> Sequence -> Sequence
a & b = make_note $ Mridangam.both_strokes (to_stroke a) (to_stroke b)

to_stroke :: CallStack.Stack => Sequence -> Mridangam.Stroke
to_stroke [Sequence.Note (Solkattu.Sollu _ _ (Just stroke))] = stroke
to_stroke seq = errorStack $ "expected a single sollu: " <> showt seq

korvai :: Tala.Tala -> Sequence -> Korvai.Korvai
korvai tala sequence = Korvai.korvai tala
    (mempty { Korvai.inst_mridangam = instrument })
    (convert sequence)

convert :: Sequence -> Korvai.Sequence
convert = map (Solkattu.map_stroke (fmap Korvai.to_stroke))

instrument :: Realize.Instrument Mridangam.Stroke
instrument =
    Solkattu.check $ Mridangam.instrument strokes Mridangam.default_patterns
    where strokes = []

make_note :: Mridangam.Stroke -> Sequence
make_note stroke =
    [Sequence.Note (Solkattu.Sollu Solkattu.NoSollu Solkattu.NotKarvai
        (Just stroke))]

mridangam_strokes :: Mridangam.Strokes Sequence
mridangam_strokes = make_note <$> Mridangam.strokes

Mridangam.Strokes {..} = mridangam_strokes

on :: Sequence
on = o&n

-- | Thom -> tha.
closed :: Sequence -> Sequence
closed = concatMap $ \note -> if [note] == o then t else [note]
