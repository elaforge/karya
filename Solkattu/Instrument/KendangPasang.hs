-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
{-# LANGUAGE RecordWildCards, DeriveFunctor #-}
module Solkattu.Instrument.KendangPasang where

import qualified Derive.Expr as Expr
import qualified Derive.Symbols as Symbols
import qualified Solkattu.Instrument.KendangTunggal as T
import qualified Solkattu.Realize as Realize
import qualified Solkattu.S as S
import qualified Solkattu.Solkattu as Solkattu

import           Global


data Stroke = Plak | Ka | Pak | Kam | Pang | Kum | Pung | PungL | De | Tut
    deriving (Eq, Ord, Show)

toTunggal :: Stroke -> (Maybe T.Stroke, Maybe T.Stroke)
toTunggal = \case
    Plak -> (Nothing, Just T.Plak)
    Ka   -> (Just T.Pak, Nothing)
    Pak  -> (Nothing, Just T.Pak)
    Kam  -> (Just T.Pang, Nothing)
    Pang -> (Nothing, Just T.Pang)
    Kum  -> (Just T.Tut, Nothing)
    Pung -> (Nothing, Just T.Tut)
    PungL -> (Nothing, Just T.TutL)
    De   -> (Just T.De, Just T.Pang)
    Tut  -> (Nothing, Just T.De)

toWadon :: Realize.Stroke Stroke -> Realize.Stroke T.Stroke
toWadon stroke = maybe filler set $ fst $ toTunggal $ Realize._stroke stroke
    where set s = stroke { Realize._stroke = s }

toLanang :: Realize.Stroke Stroke -> Realize.Stroke T.Stroke
toLanang stroke = maybe filler set $ snd $ toTunggal $ Realize._stroke stroke
    where set s = stroke { Realize._stroke = s }

filler :: Realize.Stroke T.Stroke
filler = Realize.Stroke Realize.Light T.Ka

-- * strokes

instance Solkattu.Notation Stroke where
    notation = \case
        Plak -> "PL"
        Ka -> "k"
        Pak -> "P"
        Kam -> "t"
        Pang -> "T"
        Kum -> "u"
        Pung -> "U"
        PungL -> "Ø"
        De -> "a"
        Tut -> "o"

instance Pretty Stroke where pretty = Solkattu.notation

-- | These have to match with "Cmd.Instrument.KendangBali".
instance Expr.ToExpr Stroke where
    to_expr = \case
        Plak -> "PL"
        Ka -> "k"
        Pak -> "P"
        Kam -> "t"
        Pang -> "T"
        Kum -> "u"
        Pung -> "U"
        PungL -> "Ø"
        De -> "+"
        Tut -> "o"

instance Expr.ToExpr (Realize.Stroke Stroke) where
    to_expr (Realize.Stroke emphasis stroke) = case emphasis of
        Realize.Normal -> Expr.to_expr stroke
        Realize.Light -> case stroke of
            Pak -> "^"
            Ka -> "."
            De -> "-"
            _ -> Expr.with Symbols.weak stroke
        Realize.Heavy -> Expr.with Symbols.accent stroke

data Strokes a = Strokes {
    pk :: a
    , k :: a, p :: a -- ka pak
    , t :: a, l :: a -- kam pang
    , u :: a, y :: a -- kum pung
    , yy :: a -- PungL
    , a :: a, o :: a -- de tut
    } deriving (Show, Functor)

strokes :: Strokes Stroke
strokes = Strokes
    { pk = Plak
    , k = Ka
    , p = Pak
    , t = Kam
    , l = Pang -- can't write T, _T too much like rest, don't want 2 letters
    , u = Kum
    , y = Pung
    , yy = PungL
    , a = De
    , o = Tut
    }

notes :: Strokes [S.Note g (Solkattu.Note (Realize.Stroke Stroke))]
notes = Realize.strokeToSequence <$> strokes

type SequenceR = [S.Note () (Realize.Note Stroke)]

rnotes :: Strokes SequenceR
rnotes = (:[]) . S.Note . Realize.Note . Realize.stroke <$> strokes

-- * Patterns

__ :: SequenceR
__ = [Realize.rest]

defaultPatterns :: Realize.PatternMap Stroke
defaultPatterns = Solkattu.check $ patterns
    [ (5, o.k.p.l.a)
    , (6, o.k.__.p.l.a)
    , (7, o.__.k.__.p.l.a)
    , (8, o.k.__.p.__.l.__.a)
    , (9, o.__.k.__.p.__.l.__.a)
    ]
    where
    Strokes {..} = rnotes
    (.) = (<>)

patterns :: [(S.Matra, SequenceR)]
    -> Either Realize.Error (Realize.PatternMap Stroke)
patterns = Realize.patternMap . map (first Solkattu.pattern)
