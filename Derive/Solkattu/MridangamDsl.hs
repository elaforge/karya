-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
-- | This is similar to "Derive.Solkattu.Dsl", except it exports
-- a mridangam-specific notation without using sollus at all.
module Derive.Solkattu.MridangamDsl (
    Sequence
    , (&)
    , korvai
    , k, t, n, d, u, i, y, j, p, o, od
    , on, l
    , closed
    , module Derive.Solkattu.Dsl
) where
import qualified Data.Map as Map

import qualified Util.CallStack as CallStack
import qualified Derive.Solkattu.Dsl as Dsl
import Derive.Solkattu.Dsl hiding ((&), Sequence)
import qualified Derive.Solkattu.Korvai as Korvai
import qualified Derive.Solkattu.Mridangam as Mridangam
import qualified Derive.Solkattu.Realize as Realize
import qualified Derive.Solkattu.Solkattu as Solkattu
import qualified Derive.Solkattu.Tala as Tala

import Global


type Sequence = [Solkattu.Note Korvai.Stroke]

(&) :: CallStack.Stack => Sequence -> Sequence -> Sequence
a & b = Dsl.stroke (to_stroke a Dsl.& to_stroke b) Dsl.din

to_stroke :: CallStack.Stack => Sequence -> Mridangam.Note
to_stroke seq =
    maybe (errorStack $ "no stroke for " <> pretty seq) Mridangam.note $
        Map.lookup (cast seq) stroke_map
    where
    -- Cast away the instrument-specific stroke field.
    cast = map $ Solkattu.map_stroke (const Nothing)

korvai :: Tala.Tala -> Sequence -> Korvai.Korvai
korvai tala = Korvai.korvai tala
    (mempty { Korvai.inst_mridangam = instrument })

instrument :: Realize.Instrument Mridangam.Stroke
instrument =
    Solkattu.check $ Mridangam.instrument strokes Mridangam.default_patterns
    where
    strokes =
        [ (seq, [Mridangam.note stroke])
        | (seq, stroke) <- Map.toList stroke_map
        ]

mridangam_strokes :: Mridangam.Strokes Sequence
mridangam_strokes = Mridangam.Strokes
    { k = Dsl.ki, t = Dsl.ta, l = Dsl.ti
    , n = Dsl.nam, d = Dsl.din
    , u = Dsl.lang, i = Dsl.dheem
    , y = Dsl.ku, j = Dsl.gin
    , p = Dsl.tha, o = Dsl.thom
    , od = Dsl.tam
    }
    -- The exact sollus are arbitrary, but they should be unique.
    where
    Mridangam.Strokes {..} = Mridangam.strokes

Mridangam.Strokes {..} = mridangam_strokes

on :: Sequence
on = o&n

stroke_map :: Map [Solkattu.Note Mridangam.Stroke] Mridangam.Stroke
stroke_map = Map.fromList
    [ (Dsl.ki, k), (Dsl.ta, t), (Dsl.ti, l), (Dsl.nam, n), (Dsl.din, d)
    , (Dsl.lang, u), (Dsl.dheem, i)
    , (Dsl.ku, y), (Dsl.gin, j)
    , (Dsl.tha, p), (Dsl.thom, o)
    , (Dsl.tam, od)
    ]
    where Mridangam.Strokes {..} = Mridangam.strokes

-- | Thom -> tha.
closed :: Sequence -> Sequence
closed = concatMap $ \note -> if [note] == o then t else [note]
