-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
-- | Realize an abstract solkattu Notes to concrete mridangam 'Note's.
module Solkattu.Instrument.Mridangam where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.CallStack as CallStack
import qualified Util.Lists as Lists

import qualified Derive.Expr as Expr
import qualified Derive.Symbols as Symbols
import qualified Solkattu.Realize as Realize
import qualified Solkattu.S as S
import qualified Solkattu.Solkattu as Solkattu
import qualified Solkattu.Technique as Technique

import           Global


data Stroke = Thoppi !Thoppi | Valantalai !Valantalai | Both !Thoppi !Valantalai
    deriving (Eq, Ord, Show)
data Thoppi =
    Tha !Tha | Thom !Thom
    -- | Just the gumiki movement, no strike.  Or possibly a light strike to
    -- make it speak if it doesn't sustain.
    | Gum
    deriving (Eq, Ord, Show)
data Valantalai = Ki | Ta
    | Mi -- ^ light Ki, played with middle finger
    | Nam
    | Din
    | AraiChapu -- ^ "half chapu", played covering half the valantalai
    | MuruChapu -- ^ "full chapu", played with just the pinky touching saddam
    | Dheem
    | Kin -- ^ ki on meetu
    | Tan -- ^ ta on meetu
    deriving (Eq, Ord, Show, Enum, Bounded)

data Tha = Palm -- ^ standard tha
    | Fingertips -- ^ touch with fingertips
    | Fingers -- ^ flat of the fingers
    deriving (Eq, Ord, Show)

data Thom =
    Low -- ^ This could be either normal open stroke, or gumiki low stroke.
    | Up -- ^ gumiki up
    deriving (Eq, Ord, Show)

-- * strokes

instance Solkattu.Notation Stroke where
    notation (Thoppi t) = Solkattu.notation t
    notation (Valantalai v) = Solkattu.notation v
    notation (Both t v) = Solkattu.textNotation $ case t of
        -- The convention is that thom & x is written as X.  That leaves
        -- tha & x.  I can't think of any systematic ascii transformation for x
        -- so I use a unicode overline thing.  However, p&k and p&t are pretty
        -- common, so I have irregular ad-hoc P and X for them.
        Tha _ -> case v of
            Ki -> "P"
            Ta -> "X"
            -- Hopefully this is big enough to not look like screen gunk, but
            -- small enough to not be too distracting or make the original
            -- character unreadable.
            _ -> Solkattu.notationText v <> overline
        Thom Low -> case v of
            -- These are symbols, so they have no uppercase.
            Kin -> "o" <> cedillaBelow
            Mi -> "o" <> dotBelow
            Tan -> "ô"
            _ -> Text.toUpper (Solkattu.notationText v)
        Thom Up -> Solkattu.notationText (Thom Up)
        Gum -> "/"

instance Pretty Stroke where pretty = Solkattu.notationText

-- COMBINING CEDILLA
cedillaBelow :: Text
cedillaBelow = "\x0327"

-- COMBINING DOT BELOW
dotBelow :: Text
dotBelow = "\x0323"

-- COMBINING OVERLINE
overline :: Text
overline = "\x0305"

instance Solkattu.Notation Thoppi where
    notation = Solkattu.textNotation . \case
        Thom Low -> "o"
        Thom Up -> "ó"
        Tha _ -> "p"
        Gum -> "/"

instance Solkattu.Notation Valantalai where
    notation = Solkattu.textNotation . \case
        Ki -> "k"
        Ta -> "t"
        Mi -> "."
        Nam -> "n"
        Din -> "d"
        AraiChapu -> "u"
        MuruChapu -> "v"
        Dheem -> "i"
        Kin -> ","
        Tan -> "^"

ganeshNotationThoppi :: Thoppi -> Text
ganeshNotationThoppi = \case
    Thom Low -> "d"
    Thom Up -> "d"
    Tha _ -> "h"
    Gum -> "?"

ganeshNotationValantalai :: Valantalai -> Text
ganeshNotationValantalai = \case
    Ki -> "k"
    Ta -> "t"
    Mi -> "?"
    Nam -> "n"
    Din -> "i"
    AraiChapu -> "l"
    MuruChapu -> "l"
    Dheem -> "?"
    Kin -> ","
    Tan -> "^"

instance Pretty Thoppi where pretty = Solkattu.notationText
instance Pretty Valantalai where pretty = Solkattu.notationText

-- | Pretty reproduces the "Derive.Solkattu.Dsl" syntax, which has to be
-- haskell syntax, so it can't use +, and I have to put thoppi first to avoid
-- the keyword @do@.  It would be nice if I could make the tracklang syntax
-- consistent, but maybe not a huge deal at the moment.
instance Expr.ToExpr Stroke where
    to_expr s = Expr.generator0 $ Expr.Symbol $ case s of
        Thoppi t -> thoppi t
        Valantalai v -> Solkattu.notationText v
        Both t v -> thoppi t <> Solkattu.notationText v
        where
        thoppi t = case t of
            Thom Low -> "o"
            Thom Up -> "o/"
            Tha _ -> "+"
            Gum -> "/"

instance Expr.ToExpr (Realize.Stroke Stroke) where
    to_expr (Realize.Stroke emphasis stroke) = case (emphasis, stroke) of
        (Realize.Normal, _) -> Expr.to_expr stroke
        (Realize.Light, Thoppi (Thom Low)) -> "."
        (Realize.Light, Thoppi (Tha _)) -> "-"
        (Realize.Heavy, Thoppi (Tha _)) -> "*"
        (Realize.Light, _) -> Expr.with Symbols.weak stroke
        (Realize.Heavy, _) -> Expr.with Symbols.accent stroke

data Strokes a = Strokes {
    k :: a, t :: a
    , l :: a
    , n :: a, d :: a, u :: a, v :: a, i :: a
    -- | Mnemonic: y = kin = , uses 3 fingers, j = tan = ^ uses 1.
    , y :: a, j :: a
    , p :: a, p' :: a
    , o :: a, o' :: a -- ^ gumiki up
    , _' :: a -- Gum
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
    , p = Thoppi (Tha Palm)
    , p' = Thoppi (Tha Fingertips)
    , o = Thoppi (Thom Low)
    , o' = Thoppi (Thom Up)
    , _' = Thoppi Gum
    , od = Both (Thom Low) Din
    }

notes :: Strokes (S.Sequence g (Solkattu.Note (Realize.Stroke Stroke)))
notes = Realize.strokeToSequence <$> strokes

type SequenceR = S.Sequence () (Realize.Note Stroke)

rnotes :: Strokes SequenceR
rnotes = S.singleton . S.Note . Realize.Note . Realize.stroke <$> strokes

bothRStrokes :: CallStack.Stack => Realize.Stroke Stroke
    -> Realize.Stroke Stroke -> Realize.Stroke Stroke
bothRStrokes (Realize.Stroke em1 s1) (Realize.Stroke em2 s2) =
    Realize.Stroke (em1 <> em2) (bothStrokes s1 s2)

bothStrokes :: CallStack.Stack => Stroke -> Stroke -> Stroke
bothStrokes (Thoppi a) (Valantalai b) = Both a b
bothStrokes (Valantalai b) (Thoppi a) = Both a b
bothStrokes a b =
    Solkattu.throw $ "requires thoppi & valantalai: " <> showt (a, b)

val :: Stroke -> Maybe Valantalai
val (Valantalai s) = Just s
val (Both _ s) = Just s
val (Thoppi _) = Nothing

setVal :: Valantalai -> Stroke -> Stroke
setVal v (Valantalai _) = Valantalai v
setVal v (Both t _) = Both t v
setVal _ (Thoppi t) = Thoppi t

thoppi :: Stroke -> Maybe Thoppi
thoppi (Thoppi s) = Just s
thoppi (Both s _) = Just s
thoppi (Valantalai _) = Nothing

setThoppi :: Thoppi -> Stroke -> Stroke
setThoppi _ (Valantalai v) = Valantalai v
setThoppi t (Both _ v) = Both t v
setThoppi t (Thoppi _) = Thoppi t

addThoppi :: Thoppi -> Stroke -> Stroke
addThoppi t (Valantalai v) = Both t v
addThoppi t (Both _ v) = Both t v
addThoppi t (Thoppi _) = Thoppi t

-- * fromString

fromString :: String -> Either Text [Maybe Stroke]
fromString = mapMaybeM parse
    where
    parse c = case c of
        ' ' -> Right Nothing
        '_' -> Right $ Just Nothing
        _ -> case Map.lookup c notations of
            Nothing -> Left $ "unknown mridangam stroke: " <> showt c
            Just s -> Right $ Just $ Just s

notations :: Map Char Stroke
notations = Map.fromList $ (extras++) $ Lists.mapMaybeFst isChar $
    Lists.keyOn Solkattu.notationText $ concat
        [ map Thoppi (lhs ++ [Thom Up, Gum])
        , map Valantalai rhs
        -- Omit little strokes, they're probably inaudible on Both anyway.
        , [Both lh rh | lh <- lhs, rh <- rhs, rh `notElem` [Mi, Kin, Tan]]
        ]
    where
    -- Two ways to write these.
    extras =
        [ ('y', y strokes)
        , ('j', j strokes)
        , ('l', l strokes)
        ]
    isChar t = case untxt t of
        [c] -> Just c
        _ -> Nothing
    lhs = [Tha Palm, Thom Low]
    rhs = [minBound .. ]

printNotations :: IO ()
printNotations = mapM_ putStrLn
    [ [k] <> ": " <> show v
    | (k, v) <- List.sortOn snd (Map.toList notations)
    ]

-- * postprocess

postprocess :: [Technique.Flat Stroke] -> [Technique.Flat Stroke]
postprocess = Technique.postprocess $ Technique.plain technique

technique :: Technique.Technique Stroke
technique prevs cur (next:_)
    -- There are extended analogues of this, e.g.:
    -- [on, k] to [k, on, k] -> on [k, on, ..]
    -- But to apply it I'd have to extend from ktk to ntn, and also to apply
    -- across intervening 'k's, so no need until I see more examples.
    | prev 1 == ([k], t, k) = Just k
    | prev 1 == ([k], p&t, k) = Just (p&k)
    -- Sometimes this happens but sometimes not.  I guess if it matters, I'll
    -- want a way to opt in to specific techniques.
    | prev 2 == ([k, o], o, k) = Just p
    where
    prev n = (Lists.takeEnd n prevs, cur, next)
    Strokes {..} = strokes
    (&) = bothStrokes
technique _ _ _ = Nothing

-- * patterns

__ :: SequenceR
__ = S.singleton Realize.rest

defaultPatterns :: Realize.PatternMap Stroke
defaultPatterns = Solkattu.check $ patterns
    [ (5, k.t.k.n.o)
    , (6, k.t.__.k.n.o)
    , (7, k.__.t.__.k.n.o)
    , (8, k.t.__.k.__.n.__.o)
    , (9, k.__.t.__.k.__.n.__.o)
    ]
    where
    Strokes {..} = rnotes
    (.) = (<>)

-- | Misc patterns I should figure out how to integrate some day.
misc :: [(S.Matra, SequenceR)]
misc =
    [ (7, su $ mconcat [k, __, __, t, __, __, k, __, __, n, __, __, o, __])
    ]
    where Strokes {..} = rnotes

kt_kn_o :: Realize.PatternMap Stroke
kt_kn_o = Solkattu.check $ patterns
    [ (5, k.t.k.n.o)
    , (7, k.t.__.k.n.__.o)
    , (9, k.t.__.__.k.n.__.__.o)
    ]
    where
    Strokes {..} = rnotes
    (.) = (<>)

fives :: [SequenceR]
fives =
    [ k.__.su (k.t.k.t).o
    , k.__.k.su (k.t).o
    ]
    where
    Strokes {..} = rnotes
    (.) = (<>)

-- | Intense variations of sequences, usually on the 3rd time.  I don't have a
-- way to use these yet.
intense :: [SequenceR]
intense =
    [ i.__.__.__.k.n.o
    , i.__.i.__.su (k.t.k.t).o
    ]
    where
    Strokes {..} = rnotes
    (.) = (<>)

families567 :: [Realize.PatternMap Stroke]
families567 = map Solkattu.check $ map patterns $ map (zip [5..]) $
    [ k.t.k.n.o
    , k.t.__.k.n.o
    , k.__.t.__.k.n.o
    ] : map (map su)
    [ [ k.__.t.__.k.__.k.t.o.__
      , k.__.t.__.__.__.k.__.k.t.o.__
      , k.__.__.__.t.__.__.__.k.__.k.t.o.__
      ]
    , [ k.__.t.__.k.__.k.n.o.__
      , k.__.t.__.__.__.k.__.k.n.o.__
      , k.__.__.__.t.__.__.__.k.__.k.n.o.__
      ]
    , [ k.t.p.k.p.k.t.k.n.o
      , kp.k.t.p.k.p.k.t.k.n.o
      , kpnp.k.t.p.k.p.k.t.k.n.o
      ]
    , [ k.t.k.t.p.k.p.t.o.__
      , kp.k.t.k.t.p.k.p.t.o.__
      , kpnp.k.t.k.t.p.k.p.t.o.__
      ]
    , [ n.__.k.t.p.k.p.t.o.__
      , p.__.n.__.k.t.p.k.p.t.o.__
      , k.__.p.__.n.__.k.t.p.k.p.t.o.__
      ]
    , [ u.__.k.t.p.k.p.t.o.__
      , p.__.u.__.k.t.p.k.p.t.o.__
      , k.__.p.__.u.__.k.t.p.k.p.t.o.__
      ]
    , [ k.__.t.__.k.t.__.k.n.o
      , kp.k.__.t.__.k.t.__.k.n.o
      , kpnp.k.__.t.__.k.t.__.k.n.o
      ]
    , [ k.p.k.od.__.k.t.k.n.o
      , k.p.__.k.od.__.k.t.__.k.n.o
      , k.p.__.__.k.od.__.k.__.t.__.k.n.o
      ]
    ]
    where
    Strokes {..} = rnotes
    (.) = (<>)
    kp = k.p
    kpnp = k.p.n.p

su :: S.Sequence g a -> S.Sequence g a
su = S.singleton . S.changeSpeed 1 . S.toList

patterns :: [(S.Matra, SequenceR)]
    -> Either Realize.Error (Realize.PatternMap Stroke)
patterns = Realize.patternMap . map (first Solkattu.pattern)
