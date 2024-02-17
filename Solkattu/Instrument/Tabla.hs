-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
module Solkattu.Instrument.Tabla (
    Stroke(..)
    , Baya(..), Daya(..)
    , Strokes(..)
    , strokes, notes
    , both, flam
    , bothR, flamR
) where
import qualified Data.Text as Text
import           GHC.Stack (HasCallStack)

import qualified Derive.Expr as Expr
import qualified Solkattu.Realize as Realize
import qualified Solkattu.S as S
import qualified Solkattu.Solkattu as Solkattu

import           Global

{-
    Strokes can be derived much more directly from sollu / bols, but it's still
    not exactly 1:1.  But I want to use the "konnakol" version, not the
    strokes, because I can play directly from that.

    Also I could translate to and from mridangam scores.
-}

data Stroke = Baya Baya | Daya Daya | Both Baya Daya | Flam Baya Daya
    deriving (Eq, Ord, Show)
data Baya = Ka | Ge
    deriving (Eq, Ord, Show)
data Daya =
    The -- dhere, thumb side.  Usually dhe but the because without ge.
    | Rhe -- dhere, right side
    | Na -- nam
    | Ne -- like tet, but closer to edge
    | Nhe -- tun but 3 fingers, swipe to left, from dhenne ghenne
    | Ran -- tun one finger on edge, like Nhe but 1
    | Re -- halfway between Tet and Ne
    | Tak -- tin but closed, middle finger near middle of syahi
    | Te -- actually ṭe
    | Tet
    | Tette -- infer tet or te based on next stroke
    | Ti -- te with middle finger, like mi
    | Tin -- din
    | Tre -- tette flam
    | Tu3 -- dheem, 3 fingers
    | Tun -- dheem, 1 finger
    deriving (Eq, Ord, Show)

instance Pretty Stroke where pretty = showt
instance Pretty Baya where pretty = showt
instance Pretty Daya where pretty = showt

instance Solkattu.Notation Stroke where
    notation = \case
        Baya a -> Solkattu.notation a
        Daya a -> Solkattu.notation a
        Both Ge b -> t $ Text.toUpper $ Solkattu.notationText b
        Both Ka b -> t $ Solkattu.notationText b <> overline
        Flam Ka Tet -> t "kr" -- kre
        Flam Ka Na -> t "kn" -- kran
        Flam _ _ -> t "?" -- unknown
        where
        t = Solkattu.textNotation

instance Expr.ToExpr Stroke where
    -- TODO I have no tabla instrument, so this is just theoretical
    -- Korvai.GInstrument wants it
    to_expr s = Expr.generator0 $ Expr.Symbol $ Solkattu.notationText s

instance Expr.ToExpr (Realize.Stroke Stroke) where
    to_expr (Realize.Stroke _emphasis stroke) = Expr.to_expr stroke
    -- TODO

-- COMBINING OVERLINE
overline :: Text
overline = "\x0305"

instance Solkattu.Notation Baya where
    notation = Solkattu.textNotation . \case
        Ka -> "k"
        Ge -> "e"

instance Solkattu.Notation Daya where
    notation = Solkattu.textNotation . \case
        The -> "\\"
        Rhe -> "/"
        Na -> "n"
        Ne -> "l"
        Nhe -> "u"
        Ran -> "u"
        Re -> "r"
        Tak -> "t"
        Te -> "ṭ"
        Tet -> "t"
        Tette -> "t"
        Ti -> "."
        Tin -> "d"
        Tre -> "tr" -- TODO?
        Tu3 -> "v"
        Tun -> "u"

data Strokes a = Strokes {
    ka :: a, ge :: a
    , the :: a
    , rhe :: a
    , na :: a
    , ne :: a
    , tin :: a
    , tun :: a
    , tet :: a
    , te :: a
    , tette :: a
    } deriving (Functor, Show)

strokes :: Strokes Stroke
strokes = Strokes
    { ka = Baya Ka
    , ge = Baya Ge
    -- daya
    , the = Daya The
    , rhe = Daya Rhe
    , na = Daya Na
    , ne = Daya Ne
    , tin = Daya Tin
    , tun = Daya Tun
    , tet = Daya Tet
    , te = Daya Te
    , tette = Daya Tette
    }

notes :: Strokes (S.Sequence g (Solkattu.Note (Realize.Stroke Stroke)))
notes = Realize.strokeToSequence <$> strokes

both :: HasCallStack => Stroke -> Stroke -> Stroke
both (Baya a) (Daya b) = Both a b
both (Daya b) (Baya a) = Both a b
both a b = Solkattu.throw $ "requires baya & daya: " <> showt (a, b)

bothR :: HasCallStack => Realize.Stroke Stroke -> Realize.Stroke Stroke
    -> Realize.Stroke Stroke
bothR (Realize.Stroke em1 s1) (Realize.Stroke em2 s2) =
    Realize.Stroke (em1 <> em2) (both s1 s2)

flam :: HasCallStack => Stroke -> Stroke -> Stroke
flam (Baya a) (Daya b) = Flam a b
flam (Daya b) (Baya a) = Flam a b
flam a b = Solkattu.throw $ "requires baya & daya: " <> showt (a, b)

flamR :: HasCallStack => Realize.Stroke Stroke -> Realize.Stroke Stroke
    -> Realize.Stroke Stroke
flamR (Realize.Stroke em1 s1) (Realize.Stroke em2 s2) =
    Realize.Stroke (em1 <> em2) (flam s1 s2)
