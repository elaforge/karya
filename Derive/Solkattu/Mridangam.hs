-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards, DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Realize an abstract solkattu Notes to concrete mridangam 'Note's.
module Derive.Solkattu.Mridangam where
import qualified Data.Text as Text

import qualified Util.CallStack as CallStack
import qualified Util.Pretty as Pretty
import qualified Derive.Solkattu.Realize as Realize
import qualified Derive.Solkattu.Sequence as Sequence
import qualified Derive.Solkattu.Solkattu as Solkattu
import qualified Derive.Expr as Expr

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
    | Nam | Din | Chapu | Dheem
    | Kin -- ^ ki on meetu
    | Tan -- ^ ta on meetu
    deriving (Eq, Ord, Show)

instrument :: [([Sequence.Note (Solkattu.Note Stroke)], [Realize.SNote Stroke])]
    -> Patterns -> Either Text (Realize.Instrument Stroke)
instrument = Realize.instrument standard_stroke_map

standard_stroke_map :: Realize.StrokeMap Stroke
standard_stroke_map = Realize.simple_stroke_map $
    [ ([Solkattu.Thom], [Just $ Thoppi Thom])
    , ([Solkattu.Tam], [Just $ Valantalai Chapu])
    , ([Solkattu.Tang], [Just $ Valantalai Chapu])
    , ([Solkattu.Lang], [Just $ Valantalai Chapu])
    , ([Solkattu.Dheem], [Just $ Valantalai Dheem])
    ]

-- * strokes

instance Pretty.Pretty Stroke where
    pretty (Thoppi t) = pretty t
    pretty (Valantalai v) = pretty v
    pretty (Both t v) = case t of
        Tha -> case v of
            Ki -> "P"
            Ta -> "X"
            Mi -> "pl"
            Nam -> "A"
            Din -> "O"
            Chapu -> "V"
            Dheem -> "pi" -- These are pretty rare.
            Kin -> "p,"
            Tan -> "p^"
        Thom -> Text.toUpper (pretty v)

instance Pretty.Pretty Thoppi where
    pretty n = case n of
        Thom -> "o"
        Tha -> "p"

instance Pretty.Pretty Valantalai where
    pretty n = case n of
        Ki -> "k"
        Ta -> "t"
        Mi -> "l"
        Nam -> "n"
        Din -> "d"
        Chapu -> "u"
        Dheem -> "i"
        Kin -> ","
        Tan -> "^"

-- | Pretty reproduces the "Derive.Solkattu.Dsl" syntax, which has to be
-- haskell syntax, so it can't use +, and I have to put thoppi first to avoid
-- the keyword @do@.  It would be nice if I could make the tracklang syntax
-- consistent, but maybe not a huge deal at the moment.
instance Expr.ToCall Stroke where
    to_call s = Expr.CallId $ case s of
        Thoppi t -> thoppi t
        Valantalai v -> pretty v
        Both t v -> pretty v <> thoppi t
        where
        thoppi t = case t of
            Thom -> "o"
            Tha -> "+"

instance Expr.ToCall (Realize.Stroke Stroke) where
    to_call (Realize.Stroke emphasis stroke) = case (emphasis, stroke) of
        (Realize.Normal, _) -> Expr.to_call stroke
        (Realize.Light, Thoppi Thom) -> "."
        (Realize.Light, Thoppi Tha) -> "-"
        -- TODO this is broken because a CallId is not an Expr, but will work
        -- anyway for LSol because it just puts the text in the event.  To
        -- make it work in general, I need to either abandon pretense of making
        -- a CallId and make a Text expr, or make a ToExpr class.
        (Realize.Light, _) ->
            Expr.CallId $ "^ |" <> Expr.uncall (Expr.to_call stroke)
        (Realize.Heavy, _) ->
            Expr.CallId $ "v | " <> Expr.uncall (Expr.to_call stroke)

data Strokes a = Strokes {
    k :: a, t :: a, l :: a, n :: a, d :: a, u :: a, i :: a
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
    , u = Valantalai Chapu
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

both_rstrokes :: Realize.Stroke Stroke -> Realize.Stroke Stroke
    -> Realize.Stroke Stroke
both_rstrokes (Realize.Stroke em1 s1) (Realize.Stroke em2 s2) =
    Realize.Stroke (em1 <> em2) (both_strokes s1 s2)

both_strokes :: CallStack.Stack => Stroke -> Stroke -> Stroke
both_strokes (Thoppi a) (Valantalai b) = Both a b
both_strokes (Valantalai b) (Thoppi a) = Both a b
both_strokes a b = errorStack $ "requires thoppi & valantalai: " <> showt (a, b)


-- * patterns

type Patterns = Realize.Patterns Stroke

__ :: SNote
__ = Sequence.Note Realize.Rest

default_nakatiku :: (Solkattu.Pattern, [SNote])
default_nakatiku = (Solkattu.Nakatiku, [n, p, u, p, k, t, p, k])
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
    ] : map (map s2)
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
patterns = Realize.patterns . (default_nakatiku:)
    . map (first Solkattu.PatternM)

s2 :: [Sequence.Note a] -> [Sequence.Note a]
s2 = (:[]) . Sequence.faster
