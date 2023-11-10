-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
module Solkattu.Instrument.Tabla where
import           GHC.Stack (HasCallStack)

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
    Dhe -- dhere, thumb side
    | Rhe -- dhere, right side
    | Na -- nam
    | Ne -- like tet, but closer to edge
    | Re -- halfway between Tet and Ne
    | Tin -- din
    | Tun -- dheem, 1 finger
    | Tet
    | Te -- actually ṭe
    | Thi -- te with middle finger, like mi
    deriving (Eq, Ord, Show)

instance Pretty Stroke where pretty = showt

data Strokes a = Strokes {
    ka :: a, ge :: a
    , dhe :: a
    , rhe :: a
    , na :: a
    , tin :: a
    , tun :: a
    , tet :: a
    , te :: a
    } deriving (Functor, Show)

strokes :: Strokes Stroke
strokes = Strokes
    { ka = Baya Ka
    , ge = Baya Ge
    -- daya
    , dhe = Daya Dhe
    , rhe = Daya Rhe
    , na = Daya Na
    , tin = Daya Tin
    , tun = Daya Tun
    , tet = Daya Tet
    , te = Daya Te
    }

notes :: Strokes (S.Sequence g (Solkattu.Note (Realize.Stroke Stroke)))
notes = Realize.strokeToSequence <$> strokes

bothStrokes :: HasCallStack => Stroke -> Stroke -> Stroke
bothStrokes (Baya a) (Daya b) = Both a b
bothStrokes (Daya b) (Baya a) = Both a b
bothStrokes a b =
    Solkattu.throw $ "requires baya & daya: " <> showt (a, b)

{-
dha = Both Ge Ta
dhin = Both Ge Tin
dhet = Both Ka Te
tirikita = [tet, te, ka, tet]
    where Strokes {..} = strokes
-}
