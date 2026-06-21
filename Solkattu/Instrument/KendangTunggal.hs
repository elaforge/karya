-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards, DeriveFunctor #-}
-- | Realize an abstract solkattu sequence to concrete kendang 'Note's.
module Solkattu.Instrument.KendangTunggal where
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Lists as Lists
import qualified Derive.Expr as Expr
import qualified Derive.Symbols as Symbols
import qualified Solkattu.Realize as Realize
import qualified Solkattu.S as S
import qualified Solkattu.Solkattu as Solkattu

import           Global


-- TODO some can be done together:
-- Plak, Dag + Pang, Tut + Pang, Ka + Pang
data Stroke =
    Plak -- both
    -- left
    | Pak | Pang | TutL | DeL
    -- right
    | Ka | Tut
    | De -- ^ soft dag
    | Dag -- ^ strong dag + tong on lanang
    deriving (Eq, Enum, Bounded, Ord, Show)

-- * strokes

instance Solkattu.Notation Stroke where
    notation = Solkattu.textNotation . \case
        Plak -> "P"
        Pak -> "p"
        Pang -> "t"
        TutL -> "y"
        DeL -> "u"
        Ka -> "k"
        Tut -> "i"
        De -> "o"
        Dag -> "d"

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
        De -> "-"
        Dag -> "+"

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
    pk :: a
    , p :: a, t :: a, y :: a, u :: a
    , k :: a, i :: a , o :: a, d :: a
    } deriving (Show, Functor)

strokes :: Strokes Stroke
strokes = Strokes
    { pk = Plak
    , p = Pak
    , t = Pang
    , y = TutL
    , u = DeL
    , k = Ka
    , i = Tut
    , o = De
    , d = Dag
    }

-- TODO much copy pasted with Mridangam.fromString, factor it out
fromString :: String -> Either Text [Maybe (Realize.Stroke Stroke)]
fromString = mapMaybeM parse
    where
    parse = \case
        ' ' -> Right Nothing
        '_' -> Right $ Just Nothing
        c -> case Map.lookup c notations of
            Nothing -> Left $ "unknown kendang stroke: '"
                <> Text.singleton c <> "'"
            Just s -> Right $ Just $ Just s

notations :: Map Char (Realize.Stroke Stroke)
notations = Map.fromList $ (extras++) $ Lists.mapMaybeFst isChar $
    Lists.keyOn Solkattu.notationText (map Realize.stroke [minBound ..])
    where
    extras = map (fmap Realize.stroke)
        [ ('.', De)
        ]
    isChar t = case untxt t of
        [c] -> Just c
        _ -> Nothing

_printLegend :: IO ()
_printLegend = Solkattu.printTables legend

legend :: [Solkattu.Table]
legend =
    [ ( "both" : "LH" : map describe lhs ++ "RH" : map describe rhs
      , [notation Plak : "" : map notation lhs ++ "" : map notation rhs]
      )
    ]
    where
    notation = Solkattu.notationText
    lhs = [Pak, Pang, TutL, DeL]
    rhs = [Ka, Tut, De, Dag]
    describe = showt

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
-- nakatiku = t.i.u.k.p.o.i.k -- tiykpoik
nakatiku = p.k.t.o.i.p.k.p -- or pktoipkt
    where
    Strokes {..} = notes
    (.) = (<>)
