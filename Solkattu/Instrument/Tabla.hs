-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
module Solkattu.Instrument.Tabla where
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

data Stroke = Baya Baya | Daya Daya | Both Baya Daya
    deriving (Eq, Ord, Show)
data Baya = Ka | Ge
    deriving (Eq, Ord, Show)
data Daya =
    Na -- nam
    | Taa -- pakhawaj chapu, tabla like Tin
    | Tin -- din
    | Tu -- dheem, 1 finger
    | Tet
    | Te -- actually ṭe
    | Thi -- te with middle finger, like mi
    deriving (Eq, Ord, Show)

instance Pretty Stroke where pretty = showt

data Strokes a = Strokes {
    ka :: a, ge :: a
    , na :: a
    , taa :: a
    , tin :: a
    , tu :: a
    , tet :: a
    , te :: a
    } deriving (Functor, Show)

strokes :: Strokes Stroke
strokes = Strokes
    { ka = Baya Ka
    , ge = Baya Ge
    , na = Daya Na
    , taa = Daya Taa
    , tin = Daya Tin
    , tu = Daya Tu
    , tet = Daya Tet
    , te = Daya Te
    }

notes :: Strokes (S.Sequence g (Solkattu.Note (Realize.Stroke Stroke)))
notes = Realize.strokeToSequence <$> strokes

{-
dha = Both Ge Ta
dhin = Both Ge Tin
dhet = Both Ka Te
tirikita = [tet, te, ka, tet]
    where Strokes {..} = strokes
-}
