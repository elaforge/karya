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
    | Nhe -- tun but 3 fingers, swipe to left, from dhenne ghenne
    | Ran -- tun one finger on edge, like Nhe but 1
    | Re -- halfway between Tet and Ne
    | Tak -- tin but closed, middle finger near middle of syahi
    | Tin -- din
    | Tun -- dheem, 1 finger
    | Tu3 -- dheem, 3 fingers
    | Tet
    | Te -- actually ṭe
    | Tette -- infer tet or te based on next stroke
    | Ti -- te with middle finger, like mi
    deriving (Eq, Ord, Show)

instance Pretty Stroke where pretty = showt

data Strokes a = Strokes {
    ka :: a, ge :: a
    , dhe :: a
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
    , dhe = Daya Dhe
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

bothStrokes :: HasCallStack => Stroke -> Stroke -> Stroke
bothStrokes (Baya a) (Daya b) = Both a b
bothStrokes (Daya b) (Baya a) = Both a b
bothStrokes a b =
    Solkattu.throw $ "requires baya & daya: " <> showt (a, b)

-- TODO parse strings to bols, put in stroke map
sequences :: [(Text, [Stroke])]
sequences =
    [ ("dheredhere", [ge & dhe, rhe, dhe, rhe])
    , ("terekita", [tet, te, ka, tet])
    , ("kitataka", [ka, tet, te, ka])
    , ("takaterekitataka", [te, ka, tet, te, ka, tet, te, ka])
    , ("dha", [ge & na]) -- which one depends on context
    , ("dha", [ge & tin])
    , ("dhen", [ge & tun])
    , ("dhenne", [ge & tun, ne])
    , ("dhennegene", [ge & tun, nhe, ge, ne])
    , ("tennekene", [tun, nhe, ka, ne])
    , ("taran ne", [tun, Daya Ran, ne]) -- play on rim when followed by ne
    , ("taran", [Daya Tu3, tun]) -- otherwise play in middle
    , ("dhet", [ge & tette])
    , ("dhin", [ge & tin]) -- dhin depends on context
    , ("dhin", [ge & tun])
    ]
    where
    Strokes { .. } = strokes
    (&) = bothStrokes
    nhe = Daya Nhe
