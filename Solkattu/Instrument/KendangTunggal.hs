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
        TutL -> "u"
        DeL -> "å"
        Ka -> "k"
        Tut -> "o"
        De -> "a"

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

data Strokes a = Strokes {
    pk :: a, p :: a, t :: a, u :: a, å :: a, k :: a, o :: a , a :: a
    } deriving (Show, Functor)

strokes :: Strokes Stroke
strokes = Strokes
    { pk = Plak
    , p = Pak
    , t = Pang
    , u = TutL
    , å = DeL
    , k = Ka
    , o = Tut
    , a = De
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
    [ (5, o.p.k.t.a)
    , (6, o.p.__.k.t.a)
    , (7, o.__.p.__.k.t.a)
    , (8, o.p.__.k.__.t.__.a)
    , (9, o.__.p.__.k.__.t.__.a)
    ]
    where
    Strokes {..} = rnotes
    (.) = (<>)

-- defaultPatternsEmphasis :: [(Solkattu.Pattern, SequenceM g)]
-- defaultPatternsEmphasis =
--     map (second (map $ \s -> if s == p then a else s)) defaultPatterns
--     where Strokes {..} = rnotes

patterns :: [(S.Matra, SequenceR)]
    -> Either Realize.Error (Realize.PatternMap Stroke)
patterns = Realize.patternMap . map (first Solkattu.pattern)

nakatiku :: S.Sequence g (Solkattu.Note (Realize.Stroke Stroke))
nakatiku = t.o.u.k.p.a.o.k
    where
    Strokes {..} = notes
    (.) = (<>)
