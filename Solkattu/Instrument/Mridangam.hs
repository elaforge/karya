-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
-- | Realize an abstract solkattu Notes to concrete mridangam 'Note's.
module Solkattu.Instrument.Mridangam (
    Stroke(..)
    , Thoppi(..), Valantalai(..), Tha(..), Thom(..)
    , legend
    , abbreviations
    , extraCalls
    , Strokes(..)
    , strokes
    , notes
    , bothRStrokes, flamRStrokes
    , addThoppi
    -- * fromString
    , fromString
    -- * postprocess
    , postprocess
    -- * patterns
    , defaultPatterns
    , kt_kn_o
    , families567
) where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import           GHC.Stack (HasCallStack)

import qualified Util.Lists as Lists
import qualified Util.Texts as Texts
import qualified Derive.Expr as Expr
import qualified Derive.Symbols as Symbols
import qualified Solkattu.Realize as Realize
import qualified Solkattu.S as S
import qualified Solkattu.Solkattu as Solkattu
import qualified Solkattu.Technique as Technique

import           Global


data Stroke =
    Thoppi Thoppi | Valantalai Valantalai
    | Both Thoppi Valantalai | Flam Thoppi Valantalai
    deriving (Eq, Ord, Show)
data Thoppi =
    Tha Tha | Thom Thom
    -- | Just the gumiki movement, no strike.  Or possibly a light strike to
    -- make it speak if it doesn't sustain.  This is for explicitly delayed
    -- gum, so while Thom Up is up on strike, this can express up on beat after
    -- thom.
    | Gum
    deriving (Eq, Ord, Show)
data Valantalai =
    Ki
    | Ta
    | Tra -- ^ tabla-style tra, quick kita
    | Mi -- ^ light Ki, played with middle finger
    -- | Min -- ^ middle finger, on meetu?
    | Nam
    | Din
    | AraiChapu -- ^ "half chapu", played covering half the valantalai
    | MuruChapu -- ^ "full chapu", played with just the pinky touching saddam
    | Dim
    | Kin -- ^ ki on meetu
    | Tan -- ^ ta on meetu
    | Dhe -- ^ like tabla dhere
    | Re
    deriving (Eq, Ord, Show, Enum, Bounded)

data Tha = Palm -- ^ full hand tha
    | Fingers -- ^ flat of the fingers
    | Fingertips -- ^ touch with fingertips
    deriving (Eq, Ord, Show)

data Thom =
    Open -- ^ standard stroke
    | Low -- ^ gumiki low stroke with fingertips
    | Up -- ^ gumiki strike then immediately up
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
        Tha tha -> case v of
            Ki -> "P" <> c
            Ta -> "X" <> c
            -- These are logically the same, maybe they should use the same
            -- stroke?
            AraiChapu -> "A" <> c
            -- The rationale is it's like capital I but different.
            -- It seems fine to use A for both chapus, and in fact not
            -- distinguish chapu at all.
            Dim -> "Y" <> c
            _ -> Solkattu.notationText v <> case tha of
                Fingertips -> diaeresis
                -- Hopefully this is big enough to not look like screen gunk,
                -- but small enough to not be too distracting or make the
                -- original character unreadable.
                _ -> overline
            where c = if tha == Fingertips then diaeresis else ""
        -- Append a / for gum up.  I thought of toUpper + acute accent for a
        -- single character, but it's not very obvious and doesn't work with
        -- Kin, Mi, Tan.  A two character notation might be trimmed into one,
        -- but that seems sort of ok since the gum part is secondary to the
        -- thom part.
        Thom dir -> mconcat
            [ case v of
                -- These are symbols, so they have no uppercase.
                Kin -> "o" <> cedillaBelow
                Mi -> "o" <> dotAbove
                Tan -> "ô"
                Tra -> "Kt"
                _ -> Text.toUpper (Solkattu.notationText v)
            , case dir of
                Open -> ""
                Low -> ""
                Up -> "/"
            ]
        Gum -> Solkattu.notationText v <> acute
    notation (Flam t v) = Solkattu.textNotation $ case (t, v) of
        -- Previous options were "ϕ" and "ϴ", but it's too hard to type.
        -- Or f and F for "flam", but somehow it looks weird.
        -- The problem with q Q is that Q is close to O, but I don't actually
        -- use O, so maybe it's ok?  Also nominally q is another one of those
        -- "two sounds" consonants, like x.
        (Tha _, Ki) -> "q"
        (Thom _, Ki) -> "Q"
        _ -> Solkattu.notationText t <> Solkattu.notationText v

instance Pretty Stroke where pretty = Solkattu.notationText

-- COMBINING ACUTE ACCENT
acute :: Text
acute = "\x0301"

-- COMBINING DIAERESIS
diaeresis :: Text
diaeresis = "\x0308"

-- COMBINING CEDILLA
cedillaBelow :: Text
cedillaBelow = "\x0327"

-- COMBINING DOT ABOVE
dotAbove :: Text
dotAbove = "\x0307"

-- COMBINING OVERLINE
overline :: Text
overline = "\x0305"

instance Solkattu.Notation Thoppi where
    notation = Solkattu.textNotation . \case
        Thom Open -> "o"
        Thom Low -> "o."
        Thom Up -> "o/"
        Tha Fingertips -> ":"
        Tha _ -> "p"
        Gum -> "´"

instance Solkattu.Notation Valantalai where
    notation = Solkattu.textNotation . \case
        Ki -> "k"
        Ta -> "t"
        Tra -> "x"
        -- Tra -> "kt"
        Mi -> "."
        Nam -> "n"
        Din -> "d"
        AraiChapu -> "u"
        MuruChapu -> "v"
        Dim -> "i"
        Kin -> ","
        Tan -> "^"
        Dhe -> "h"
        Re -> "r"

instance Solkattu.Abbreviations Stroke where
    abbreviations = abbreviations

-- I tried making abbreviations just [([stroke], [stroke])], but then
-- I need a new Eq constraint and it's annoying.  Back to a function.
abbreviations :: [Stroke] -> Maybe ([Stroke], Int)
abbreviations xs = msum $ map find abbrs
    where
    find (prefix, replacement)
        | prefix `List.isPrefixOf` xs = Just (replacement, length prefix)
        | otherwise = Nothing
    -- Because it's too annoying to incorporate Either Text stroke into
    -- the rendering pipeline, let's reuse stroke for abbreviations.
    -- I'll reuse tra kra, even though it makes it ambiguous.
    abbrs =
        [ ([k, t], [r])
        , ([p, k], [pk])
        , ([o, k], [ok])
        ]
    Strokes {..} = strokes
    pk = Flam (Tha Palm) Ki
    ok = Flam (Thom Low) Ki

instance Pretty Thoppi where pretty = Solkattu.notationText
instance Pretty Valantalai where pretty = Solkattu.notationText

_printLegend :: IO ()
_printLegend = mapM_ Text.IO.putStrLn $
    Texts.columns 2 $ concat [row0 : rows | (row0, rows) <- legend]

legend :: [([Text], [[Text]])]
legend =
    [ ("" : map describeT lhs, ["" : map Solkattu.notationText lhs])
    , ( "" : map describeV rhs
      , ("" : map Solkattu.notationText rhs) : matrix
      )
    , ( ["", "kita", "taka", "domka"]
      , ["2x" : map Solkattu.notationText [Valantalai Tra, pk, ok]]
      )
    ]
    where
    matrix = map (map Solkattu.notationText)
        [Thoppi lh : map (Both lh) rhs | lh <- lhs]
    rhs = [Ki ..]
    pk = Flam (Tha Palm) Ki
    ok = Flam (Thom Low) Ki
    lhs =
        [ Tha Palm
        , Tha Fingertips
        , Thom Open
        , Thom Up
        , Gum
        ]
    describeV = \case
        AraiChapu -> "½chap"
        MuruChapu -> "chap"
        a -> Text.toLower $ showt a
    describeT = \case
        Tha Fingertips -> "fingers"
        Tha _ -> "tha"
        Thom Low -> "thom"
        Thom Open -> "thom"
        Thom Up -> "gumiki"
        Gum -> "gum"

_printStrokes :: IO ()
_printStrokes = mapM_ Text.IO.putStrLn $ Texts.columns 2 $ concat
    [ [ "" : map (t . v) rhs ]
    , [["both"]]
    , [ (map t $ Thoppi lh : [Both lh rh | rh <- rhs])
      | lh <- lhs
      ]
    , [["flam"]]
    , [ (map t $ Thoppi lh : [Flam lh rh | rh <- rhs])
      | lh <- lhs
      ]
    -- Common sequences.
    , [ map t [od, v Tra, Flam (Tha Palm) Ki]
      , map t [Both (Thom Open) Dhe, v Re, v Dhe, v Re]
      ]
    ]
    where
    v = Valantalai
    t = Solkattu.notationText
    rhs = [Ki ..]
    lhs = [Tha Palm, Tha Fingertips, Thom Open, Thom Up, Gum]
    Strokes { od } = strokes

-- | Notation is designed for display only and to be as horizontally concise
-- as possible, ideally taking just one character.  Tracklang is vertical, so
-- it has more horizontal space, and I want to be able to type them.
-- So I don't use the abbreviations for Both.
instance Expr.ToExpr Stroke where
    to_expr stroke = case stroke of
        Thoppi t -> Expr.to_expr t
        Valantalai v -> Expr.to_expr v
        Both t v -> call $ Solkattu.notationText t <> Solkattu.notationText v
        Flam (Tha _) Ki -> call $ Solkattu.notationText stroke
        Flam t v -> Expr.generator $ Expr.call "f"
            [ s $ Solkattu.notationText t
            , s $ Solkattu.notationText v
            ]
            where s = Expr.VStr . Expr.Str

-- | For consistency, I use t<>v for all Both, but I also want to understand
-- the usual solkattu single character abbreviations.
extraCalls :: Thoppi -> Valantalai -> Maybe Text
extraCalls t v = case t of
    Tha Palm | v `elem` [Ki, Ta, AraiChapu, MuruChapu] ->
        Just $ Solkattu.notationText $ Both t v
    Thom Open | v `notElem` [Kin, Mi, Tan] ->
        Just $ Solkattu.notationText $ Both t v
    _ -> Nothing

instance Expr.ToExpr Valantalai where
    to_expr = call . Solkattu.notationText
instance Expr.ToExpr Thoppi where
    to_expr = call . Solkattu.notationText

call :: Text -> Expr.Expr val
call = Expr.generator0 . Expr.Symbol

instance Expr.ToExpr (Realize.Stroke Stroke) where
    to_expr (Realize.Stroke emphasis stroke) = case emphasis of
        Realize.Normal -> Expr.to_expr stroke
        Realize.Light -> Expr.with Symbols.weak stroke
        Realize.Heavy -> Expr.with Symbols.accent stroke

data Strokes a = Strokes {
    k :: a
    , t :: a
    , r :: a
    , l :: a
    , n :: a, d :: a, u :: a, v :: a, i :: a
    -- | Mnemonic: y = kin = , uses 3 fingers, j = tan = ^ uses 1.
    , y :: a, j :: a
    , p :: a, p' :: a
    , o :: a, o' :: a -- ^ gumiki up, should be o/ but invalid syntax
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
    , r = Valantalai Tra
    , l = Valantalai Mi
    , n = Valantalai Nam
    , d = Valantalai Din
    , u = Valantalai AraiChapu
    , v = Valantalai MuruChapu
    , i = Valantalai Dim
    , y = Valantalai Kin
    , j = Valantalai Tan
    , p = Thoppi (Tha Palm)
    , p' = Thoppi (Tha Fingertips)
    , o = Thoppi (Thom Open)
    , o' = Thoppi (Thom Up)
    , _' = Thoppi Gum
    , od = Both (Thom Open) Din
    }

notes :: Strokes (S.Sequence g (Solkattu.Note (Realize.Stroke Stroke)))
notes = Realize.strokeToSequence <$> strokes

type SequenceR = S.Sequence () (Realize.Note Stroke)

rnotes :: Strokes SequenceR
rnotes = S.singleton . S.Note . Realize.Note . Realize.stroke <$> strokes

bothRStrokes :: HasCallStack => Realize.Stroke Stroke
    -> Realize.Stroke Stroke -> Realize.Stroke Stroke
bothRStrokes (Realize.Stroke em1 s1) (Realize.Stroke em2 s2) =
    Realize.Stroke (em1 <> em2) (bothStrokes s1 s2)

bothStrokes :: HasCallStack => Stroke -> Stroke -> Stroke
bothStrokes (Thoppi a) (Valantalai b) = Both a b
bothStrokes (Valantalai b) (Thoppi a) = Both a b
bothStrokes a b =
    Solkattu.throw $ "requires thoppi & valantalai: " <> showt (a, b)

flamRStrokes :: HasCallStack => Realize.Stroke Stroke
    -> Realize.Stroke Stroke -> Realize.Stroke Stroke
flamRStrokes (Realize.Stroke em1 s1) (Realize.Stroke em2 s2) =
    Realize.Stroke (em1 <> em2) (flamStrokes s1 s2)

flamStrokes :: HasCallStack => Stroke -> Stroke -> Stroke
flamStrokes (Thoppi a) (Valantalai b) = Flam a b
flamStrokes (Valantalai b) (Thoppi a) = Flam a b
flamStrokes a b =
    Solkattu.throw $ "requires thoppi & valantalai: " <> showt (a, b)

addThoppi :: Thoppi -> Stroke -> Stroke
addThoppi t (Valantalai v) = Both t v
addThoppi t (Both _ v) = Both t v
addThoppi t (Flam _ v) = Flam t v
addThoppi t (Thoppi _) = Thoppi t

-- * fromString

fromString :: String -> Either Text [Maybe Stroke]
fromString = mapMaybeM parse
    where
    parse = \case
        ' ' -> Right Nothing
        '_' -> Right $ Just Nothing
        c -> case Map.lookup c notations of
            Nothing -> Left $ "unknown mridangam stroke: '"
                <> Text.singleton c <> "'"
            Just s -> Right $ Just $ Just s

notations :: Map Char Stroke
notations = Map.fromList $ (extras++) $ Lists.mapMaybeFst isChar $
    Lists.keyOn Solkattu.notationText $ concat
        [ map Thoppi (lhs ++ [Gum])
        , map Valantalai rhs
        -- Omit little strokes, they're probably inaudible on Both anyway.
        , [Both lh rh | lh <- lhs, rh <- rhs, rh `notElem` [Mi, Kin, Tan]]
        , [Flam (Tha Palm) Ki, Flam (Thom Low) Ki]
        ]
    where
    -- Two ways to write these, yjl are valid haskell ids, ,^. are not.
    extras =
        [ ('y', y strokes)
        , ('j', j strokes)
        , ('l', l strokes)
        -- The notation is o/, but that's two characters and I need a single
        -- character for toString.
        , ('/', Thoppi (Thom Up))
        , ('`', Thoppi Gum) -- also ´, but backtick is easier to type
        , ('?', Both (Thom Up) Din)
        -- Can't use n overline because it's actually two chars.
        -- H looks like N but conflicts with Dhere, but G is a nasal consonant
        , ('G', Both (Tha Palm) Nam)
        -- , ('n̅', Both (Tha Palm) Nam)
        ]
    isChar t = case untxt t of
        [c] -> Just c
        _ -> Nothing
    lhs = [Tha Palm, Thom Open]
    rhs = [minBound .. ]

_printNotations :: IO ()
_printNotations = mapM_ putStrLn
    [ [k] <> ": " <> show v
    | (k, v) <- List.sortOn snd (Map.toList notations)
    ]

-- * postprocess

postprocess :: [Technique.Flat Stroke] -> [Technique.Flat Stroke]
postprocess = Technique.postprocess $ Technique.plain technique

technique :: Technique.Technique Stroke
technique prevs cur (next:_)
    -- Usually k_t_... is reduced to k_...  The rests seem complicated though,
    -- so let's do it globally for now.
    -- Except k_t_k_kt_kno, I want t_k_kt_kno
    | prev 1 == ([k], t) = Just k
    | prev 1 == ([k], p&t) = Just (p&k)
    -- Sometimes this happens but sometimes not.  I guess if it matters, I'll
    -- want a way to opt in to specific techniques.
    | prevNext 2 == ([k, o], o, k) = Just p
    where
    prevNext n = (Lists.takeEnd n prevs, cur, next)
    prev n = (Lists.takeEnd n prevs, cur)
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
    , (5, su $ o.k.t.p.u.p.k.t.p.k)
    ]
    where
    Strokes {..} = rnotes
    (.) = (<>)

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
    , k.t.__.k.__.n.__.o
    , k.__.t.__.k.__.n.__.o
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
