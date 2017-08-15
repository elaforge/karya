-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards, DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Realize abstract solkattu Notes to concrete reyong 'Note's.
module Derive.Solkattu.Instrument.Reyong where
import qualified Derive.Expr as Expr
import qualified Derive.Solkattu.Realize as Realize
import qualified Derive.Solkattu.Sequence as Sequence
import qualified Derive.Solkattu.Solkattu as Solkattu

import Global


type SNote = Realize.SNote Stroke

note :: stroke -> Realize.SNote stroke
note = Sequence.Note . Realize.Note . Realize.stroke

-- Automatically infer two handed cek if they are isolated.
-- Maybe infer light byut if there is a note immediately afterwards?
data Stroke = N1 | N2 | N3 | N4 | N14 | Byut | Byong | CekC | CekO
    deriving (Show, Eq, Ord)

instance Pretty Stroke where
    pretty s = case s of
        N1 -> "1"
        N2 -> "2"
        N3 -> "3"
        N4 -> "4"
        N14 -> "i"
        Byut -> "b"
        Byong -> "o"
        CekC -> "k"
        CekO -> "x"

instance Expr.ToExpr Stroke where
    to_expr s = case s of
        N1 -> "n1"
        N2 -> "n2"
        N3 -> "n3"
        N4 -> "n4"
        N14 -> "n14"
        Byut -> "+"
        Byong -> "O"
        CekC -> "X"
        CekO -> "/"

instance Expr.ToExpr (Realize.Stroke Stroke) where to_expr = Realize.to_expr

data Strokes a = Strokes {
    r1 :: a, r2 :: a, r3 :: a, r4 :: a, i :: a
    , b :: a, o :: a, k :: a, x :: a
    } deriving (Functor, Show)

strokes :: Strokes Stroke
strokes = Strokes
    { r1 = N1
    , r2 = N2
    , r3 = N3
    , r4 = N4
    , i = N14
    , b = Byut
    , o = Byong
    , k = CekC
    , x = CekO
    }

notes :: Strokes SNote
notes = note <$> strokes

-- * instrument

instrument :: [([Sequence.Note g (Solkattu.Note Solkattu.Sollu)], [SNote])]
    -> Patterns -> Either Text (Realize.Instrument Stroke)
instrument = Realize.instrument standard_stroke_map

standard_stroke_map :: Realize.StrokeMap Stroke
standard_stroke_map = Realize.simple_stroke_map
    [ ([Solkattu.Thom], [Just Byong])
    , ([Solkattu.Tam], [Just Byong])
    , ([Solkattu.Tang], [Just Byong])
    , ([Solkattu.Lang], [Just Byong])
    , ([Solkattu.Dheem], [Just Byong])
    ]

-- * patterns

type Patterns = Realize.Patterns Stroke

__ :: SNote
__ = Realize.rest

melodic_nakatiku :: (Solkattu.Pattern, [SNote])
melodic_nakatiku = (Solkattu.Nakatiku, [i, r3, i, r2, r3, i, r3, r2])
    where Strokes {..} = notes

melodic_patterns :: Patterns
melodic_patterns = Solkattu.check $ patterns
    [ (5, [r3, r2, r3, i, r2])
    , (6, [r3, r2, __, r3, i, r2])
    , (7, [r3, __, r2, __, r3, i, r2])
    , (8, [r3, r2, __, r3, __, i, __, r2])
    , (9, [r3, __, r2, __, r3, __, i, __, r2])
    ] where Strokes {..} = notes

rhythmic_patterns :: Patterns
rhythmic_patterns = Solkattu.check $ patterns
    [ (5, [b, __, o, __, __])
    , (6, [o, __, b, o, __, __])
    , (7, [x, __, x, __, o, __, __])
    , (8, [x, k, __, x, __, o, __, __])
    , (9, [x, k, __, x, __, b, o, __, __])
    ] where Strokes {..} = notes

patterns :: [(Sequence.Matra, [Realize.SNote Stroke])]
    -> Either Text (Realize.Patterns Stroke)
patterns = Realize.patterns . (melodic_nakatiku:)
    . map (first Solkattu.PatternM)
