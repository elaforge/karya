-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards, DeriveFunctor #-}
-- | Realize an abstract solkattu sequence to concrete kendang 'Note's.
module Solkattu.Instrument.KendangTunggal where
import qualified Derive.Expr as Expr
import qualified Derive.Symbols as Symbols
import qualified Solkattu.Realize as Realize
import qualified Solkattu.S as S
import qualified Solkattu.Solkattu as Solkattu

import Global


data Stroke =
    Plak -- both
    | Pak | Pang | TutL | DeL -- left
    | Ka | Tut | De -- right
    deriving (Eq, Ord, Show)

-- * strokes

instance Solkattu.Notation Stroke where
    notation = Solkattu.textNotation . \case
        Plak -> "P"
        Pak -> "p"
        Pang -> "t"
        TutL -> "y"
        DeL -> "a"
        Ka -> "k"
        Tut -> "i"
        De -> "o"

instance Pretty Stroke where pretty = Solkattu.notationText

-- | TODO should I make these consistent with 'Strokes'?
instance Expr.ToExpr Stroke where
    to_expr = \case
        Plak -> "PL"
        Pak -> "P"
        Pang -> "T"
        TutL -> "Ø"
        DeL -> "`O+`"
        Ka -> ".."
        Tut -> "o"
        De -> "+"

-- TODO unify with User.Elaforge.Instrument.Kontakt.KendangBali.Stroke
instance Expr.ToExpr (Realize.Stroke Stroke) where
    to_expr (Realize.Stroke emphasis stroke) = case emphasis of
        Realize.Normal -> Expr.to_expr stroke
        Realize.Light -> case stroke of
            Pak -> "^"
            TutL -> "ø"
            Ka -> "."
            De -> "-"
            _ -> Expr.with Symbols.weak stroke
        Realize.Heavy -> Expr.with Symbols.accent stroke

instance Solkattu.Abbreviations Stroke where abbreviations = const Nothing

data Strokes a = Strokes {
    pk :: a, p :: a, t :: a, u :: a, a :: a, k :: a, i :: a , o :: a
    } deriving (Show, Functor)

strokes :: Strokes Stroke
strokes = Strokes
    { pk = Plak
    , p = Pak
    , t = Pang
    , u = TutL
    , a = DeL
    , k = Ka
    , i = Tut
    , o = De
    }

notes :: Strokes (S.Sequence g (Solkattu.Note (Realize.Stroke Stroke)))
notes = Realize.strokeToSequence <$> strokes

type SequenceR = S.Sequence () (Realize.Note Stroke)

rnotes :: Strokes SequenceR
rnotes = S.singleton . S.Note . Realize.Note . Realize.stroke <$> strokes

-- * Patterns

__ :: SequenceR
__ = S.singleton Realize.rest

defaultPatterns :: Realize.PatternMap Stroke
defaultPatterns = Solkattu.check $ patterns
    [ (5, i.p.k.t.o)
    , (6, i.p.__.k.t.o)
    , (7, i.__.p.__.k.t.o)
    , (8, i.p.__.k.__.t.__.o)
    , (9, i.__.p.__.k.__.t.__.o)
    ]
    where
    Strokes {..} = rnotes
    (.) = (<>)

-- defaultPatternsEmphasis :: [(Solkattu.Pattern, SequenceM g)]
-- defaultPatternsEmphasis =
--     map (second (map $ \s -> if s == p then o else s)) defaultPatterns
--     where Strokes {..} = rnotes

patterns :: [(S.Matra, SequenceR)]
    -> Either Realize.Error (Realize.PatternMap Stroke)
patterns = Realize.patternMap . map (first Solkattu.pattern)

nakatiku :: S.Sequence g (Solkattu.Note (Realize.Stroke Stroke))
nakatiku = t.i.u.k.p.o.i.k
    where
    Strokes {..} = notes
    (.) = (<>)
