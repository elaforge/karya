-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards, DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Realize an abstract solkattu Notes to concrete mridangam 'Note's.
module Derive.Solkattu.Instrument.Mridangam where
import qualified Data.Text as Text

import qualified Util.CallStack as CallStack
import qualified Derive.Expr as Expr
import qualified Derive.Solkattu.Realize as Realize
import qualified Derive.Solkattu.Sequence as Sequence
import qualified Derive.Solkattu.Solkattu as Solkattu
import qualified Derive.Symbols as Symbols

import Global


type SNote = Sequence.Note (Realize.Note Stroke)

note :: stroke -> Realize.SNote stroke
note = Sequence.Note . Realize.Note . Realize.stroke

data Stroke = Thoppi !Thoppi | Valantalai !Valantalai | Both !Thoppi !Valantalai
    deriving (Eq, Ord, Show)
data Thoppi = Tha | Thom
    deriving (Eq, Ord, Show)
data Valantalai = Ki | Ta
    | Mi -- ^ light Ki, played with middle finger
    | Nam | Din
    | AraiChapu -- ^ whole-hand chapu
    | MuruChapu -- ^ pinky chapu
    | Dheem
    | Kin -- ^ ki on meetu
    | Tan -- ^ ta on meetu
    deriving (Eq, Ord, Show)

instrument :: [([Sequence.Note (Solkattu.Note Stroke)], [Realize.SNote Stroke])]
    -> Patterns -> Either Text (Realize.Instrument Stroke)
instrument = Realize.instrument standard_stroke_map

standard_stroke_map :: Realize.StrokeMap Stroke
standard_stroke_map = Realize.simple_stroke_map $
    [ ([Solkattu.Thom], [Just $ Thoppi Thom])
    , ([Solkattu.Tam], [Just $ Valantalai AraiChapu])
    , ([Solkattu.Tang], [Just $ Valantalai AraiChapu])
    , ([Solkattu.Lang], [Just $ Valantalai AraiChapu])
    , ([Solkattu.Dheem], [Just $ Valantalai Dheem])
    ]

-- * strokes

instance Pretty Stroke where
    pretty (Thoppi t) = pretty t
    pretty (Valantalai v) = pretty v
    pretty (Both t v) = case t of
        Tha -> case v of
            Ki -> "P"
            Ta -> "X"
            Mi -> "pl"
            Nam -> "A"
            Din -> "O"
            AraiChapu -> "pu"
            MuruChapu -> "pv"
            Dheem -> "pi" -- These are pretty rare.
            Kin -> "p,"
            Tan -> "p^"
        Thom -> Text.toUpper (pretty v)

instance Pretty Thoppi where
    pretty n = case n of
        Thom -> "o"
        Tha -> "p"

instance Pretty Valantalai where
    pretty n = case n of
        Ki -> "k"
        Ta -> "t"
        Mi -> "l"
        Nam -> "n"
        Din -> "d"
        AraiChapu -> "u"
        MuruChapu -> "v"
        Dheem -> "i"
        Kin -> ","
        Tan -> "^"

-- | Pretty reproduces the "Derive.Solkattu.Dsl" syntax, which has to be
-- haskell syntax, so it can't use +, and I have to put thoppi first to avoid
-- the keyword @do@.  It would be nice if I could make the tracklang syntax
-- consistent, but maybe not a huge deal at the moment.
instance Expr.ToExpr Stroke where
    to_expr s = Expr.generator0 $ Expr.Symbol $ case s of
        Thoppi t -> thoppi t
        Valantalai v -> pretty v
        Both t v -> pretty v <> thoppi t
        where
        thoppi t = case t of
            Thom -> "o"
            Tha -> "+"

instance Expr.ToExpr (Realize.Stroke Stroke) where
    to_expr (Realize.Stroke emphasis stroke) = case (emphasis, stroke) of
        (Realize.Normal, _) -> Expr.to_expr stroke
        (Realize.Light, Thoppi Thom) -> "."
        (Realize.Light, Thoppi Tha) -> "-"
        (Realize.Light, _) -> Expr.with Symbols.weak stroke
        (Realize.Heavy, _) -> Expr.with Symbols.accent stroke

data Strokes a = Strokes {
    k :: a, t :: a, l :: a, n :: a, d :: a, u :: a, v :: a, i :: a
    , y :: a, j :: a
    , p :: a, o :: a
    -- | @do@ would match score notation, but @do@ is a keyword.  Ultimately
    -- that's because score uses + for tha, and +o is an attr, while o+ is
    -- a bareword.  But perhaps I should change + to p in the score, and then
    -- the left hand can go on the left side?
    , od :: a
    -- Less common combinations can use (&).
    } deriving (Functor, Show)

strokes :: Strokes Stroke
strokes = Strokes
    { k = Valantalai Ki
    , t = Valantalai Ta
    , l = Valantalai Mi
    , n = Valantalai Nam
    , d = Valantalai Din
    , u = Valantalai AraiChapu
    , v = Valantalai MuruChapu
    , i = Valantalai Dheem
    , y = Valantalai Kin
    , j = Valantalai Tan
    , p = Thoppi Tha
    , o = Thoppi Thom
    , od = Both Thom Din
    }

notes :: Strokes SNote
notes = note <$> strokes

both :: Thoppi -> Valantalai -> SNote
both a b = note (Both a b)

(&) :: CallStack.Stack => SNote -> SNote -> SNote
Sequence.Note (Realize.Note s1) & Sequence.Note (Realize.Note s2) =
    Sequence.Note $ Realize.Note $ both_rstrokes s1 s2
a & b = errorStack $ "requires notes: " <> showt (a, b)

both_rstrokes :: CallStack.Stack => Realize.Stroke Stroke
    -> Realize.Stroke Stroke -> Realize.Stroke Stroke
both_rstrokes (Realize.Stroke em1 s1) (Realize.Stroke em2 s2) =
    Realize.Stroke (em1 <> em2) (both_strokes s1 s2)

both_strokes :: CallStack.Stack => Stroke -> Stroke -> Stroke
both_strokes (Thoppi a) (Valantalai b) = Both a b
both_strokes (Valantalai b) (Thoppi a) = Both a b
both_strokes a b = errorStack $ "requires thoppi & valantalai: " <> showt (a, b)


-- * patterns

type Patterns = Realize.Patterns Stroke

__ :: SNote
__ = Realize.rest

default_nakatiku :: [(Solkattu.Pattern, [SNote])]
default_nakatiku =
    [ (Solkattu.Nakatiku, [n, p, u, p, k, t, p, k])
    , (Solkattu.Taka, [k, p])
    , (Solkattu.Takanaka, [k, p, n, p])
    ]
    where Strokes {..} = notes

alternate_nakatiku :: [SNote]
alternate_nakatiku = [t, p, u, p, k, t, p, k]
    where Strokes {..} = notes

default_patterns :: Patterns
default_patterns = Solkattu.check $ patterns
    [ (5, [k, t, k, n, o])
    , (6, [k, t, __, k, n, o])
    , (7, [k, __, t, __, k, n, o])
    , (8, [k, t, __, k, __, n, __, o])
    , (9, [k, __, t, __, k, __, n, __, o])
    ]
    where Strokes {..} = notes

default_patterns_emphasis :: Patterns
default_patterns_emphasis =
    Realize.map_patterns (map $ \s -> if s == t then i else s) default_patterns
    where Strokes {..} = notes

-- | Misc patterns I should figure out how to integrate some day.
misc :: [(Sequence.Matra, [SNote])]
misc =
    [ (7, su [k, __, __, t, __, __, k, __, __, n, __, __, o, __])
    ]
    where Strokes {..} = notes

kt_kn_o :: Patterns
kt_kn_o = Solkattu.check $ patterns
    [ (5, [k, t, k, n, o])
    , (7, [k, t, __, k, n, __, o])
    , (9, [k, t, __, __, k, n, __, __, o])
    ]
    where Strokes {..} = notes

families567 :: [Patterns]
families567 = map (Solkattu.check . patterns . zip [5..]) $
    [ [k, t, k, n, o]
    , [k, t, __, k, n, o]
    , [k, __, t, __, k, n, o]
    ] : map (map su)
    [ [ [k, __, t, __, k, __, k, t, o, __]
      , [k, __, t, __, __, __, k, __, k, t, o, __]
      , [k, __, __, __, t, __, __, __, k, __, k, t, o, __]
      ]
    , [ [k, __, t, __, k, __, k, n, o, __]
      , [k, __, t, __, __, __, k, __, k, n, o, __]
      , [k, __, __, __, t, __, __, __, k, __, k, n, o, __]
      ]
    , [ [k, t, p, k, p, k, t, k, n, o]
      , kp <> [k, t, p, k, p, k, t, k, n, o]
      , kpnp <> [k, t, p, k, p, k, t, k, n, o]
      ]
    , [ [k, t, k, t, p, k, p, t, o, __]
      , [p, __, k, t, k, t, p, k, p, t, o, __]
      , [k, __, p, __, k, t, k, t, p, k, p, t, o, __]
      ]
    , [ [n, __, k, t, p, k, p, t, o, __]
      , [p, __, n, __, k, t, p, k, p, t, o, __]
      , [k, __, p, __, n, __, k, t, p, k, p, t, o, __]
      ]
    , [ [u, __, k, t, p, k, p, t, o, __]
      , [p, __, u, __, k, t, p, k, p, t, o, __]
      , [k, __, p, __, u, __, k, t, p, k, p, t, o, __]
      ]
    , [ [k, __, t, __, k, t, __, k, n, o]
      , kp <> [k, __, t, __, k, t, __, k, n, o]
      , kpnp <> [k, __, t, __, k, t, __, k, n, o]
      ]
    , [ [k, p, k, od, __, k, t, k, n, o]
      , [k, p, __, k, od, __, k, t, __, k, n, o]
      , [k, p, __, __, k, od, __, k, __, t, __, k, n, o]
      ]
    ]
    where
    Strokes {..} = notes
    kp = [k, p]
    kpnp = [k, p, n, p]

patterns :: [(Sequence.Matra, [Realize.SNote Stroke])]
    -> Either Text (Realize.Patterns Stroke)
patterns = Realize.patterns . (default_nakatiku++)
    . map (first Solkattu.PatternM)

su :: [Sequence.Note a] -> [Sequence.Note a]
su = (:[]) . Sequence.change_speed 1
