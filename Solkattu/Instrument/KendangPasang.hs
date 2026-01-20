-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
{-# LANGUAGE RecordWildCards, DeriveFunctor #-}
module Solkattu.Instrument.KendangPasang (
    Stroke(..)
    , toWadon, toLanang
    , toWadonM, toLanangM
    , Strokes(..)
    , fromString
    , notes
    , defaultPatterns
    , nakatiku
) where
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Lists as Lists
import qualified Derive.Expr as Expr
import qualified Derive.Symbols as Symbols
import qualified Solkattu.Instrument.KendangTunggal as T
import qualified Solkattu.Instrument.Mridangam as Mridangam
import qualified Solkattu.Realize as Realize
import qualified Solkattu.S as S
import qualified Solkattu.Solkattu as Solkattu

import           Global


data Stroke = Plak | Ka | Pak | Kam | Pang | Kum | Pung | PungL | De | Tut
    deriving (Show, Eq, Ord, Enum, Bounded)

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

toMridangam :: Stroke -> (Maybe Mridangam.Stroke, Maybe Mridangam.Stroke)
toMridangam = \case
    Plak -> (Nothing, Just pk)
    Ka   -> (Just k, Nothing)
    Pak  -> (Nothing, Just k)
    Kam  -> (Just n, Nothing)
    Pang -> (Nothing, Just n)
    Kum  -> (Just d, Nothing) -- ?
    Pung -> (Nothing, Just d)
    PungL -> (Nothing, Just i)
    De   -> (Just o, Just n)
    Tut  -> (Nothing, Just o)
    where
    pk = Mridangam.Both (Mridangam.Tha Mridangam.Palm) Mridangam.Ki
    Mridangam.Strokes {..} = Mridangam.strokes

toWadonM :: Realize.Stroke Stroke -> Realize.Stroke Mridangam.Stroke
toWadonM stroke = maybe fillerM set $ fst $ toMridangam $ Realize._stroke stroke
    where set s = stroke { Realize._stroke = s }

toLanangM :: Realize.Stroke Stroke -> Realize.Stroke Mridangam.Stroke
toLanangM stroke =
    maybe fillerM set $ snd $ toMridangam $ Realize._stroke stroke
    where set s = stroke { Realize._stroke = s }

fillerM :: Realize.Stroke Mridangam.Stroke
fillerM = Realize.Stroke Realize.Light (Mridangam.p Mridangam.strokes)

-- * strokes

instance Solkattu.Notation Stroke where
    notation = Solkattu.textNotation . \case
        -- Plak -> "PL"
        -- Ka -> "k"
        -- Pak -> "P"
        -- Kam -> "t"
        -- Pang -> "T"
        -- Kum -> "u"
        -- Pung -> "U"
        -- PungL -> "Y"
        -- De -> "a"
        -- Tut -> "o"
        Plak -> "P"
        Ka -> "k"
        Pak -> "p"
        Kam -> "t"
        Pang -> "l"
        Kum -> "u"
        Pung -> "y"
        -- I like Ø from ToExpr, but it's hard to type, and tut no longer o
        PungL -> "Y"
        De -> "o"
        Tut -> "i" -- o is too similar looking to a

instance Pretty Stroke where pretty = Solkattu.notationText

-- | These have to match with "Cmd.Instrument.KendangBali".
-- TODO harmonize the two notations?
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

instance Solkattu.Abbreviations Stroke where abbreviations = const Nothing

data Strokes a = Strokes {
    pk :: a
    , k :: a, p :: a -- ka pak
    , t :: a, l :: a -- kam pang
    , u :: a, y :: a -- kum pung
    , yy :: a -- PungL
    , o :: a, i :: a -- de tut
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
    , o = De
    , i = Tut
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
    extras = [('.', lt De)]
    lt = Realize.Stroke Realize.Light
    isChar t = case untxt t of
        [c] -> Just c
        _ -> Nothing

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
    [ (5, i.k.p.l.o)
    , (6, i.k.__.p.l.o)
    , (7, i.__.k.__.p.l.o)
    , (8, i.k.__.p.__.l.__.o)
    , (9, i.__.k.__.p.__.l.__.o)
    ]
    where
    Strokes {..} = rnotes
    (.) = (<>)

patterns :: [(S.Matra, SequenceR)]
    -> Either Realize.Error (Realize.PatternMap Stroke)
patterns = Realize.patternMap . map (first Solkattu.pattern)

nakatiku :: S.Sequence g (Solkattu.Note (Realize.Stroke Stroke))
nakatiku = t.y.yy.k.p.o.i.k
    where
    Strokes {..} = notes
    (.) = (<>)
