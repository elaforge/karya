-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Hardcoded scale map.

    This is to scales as Derive.Call.All is to calls.
-}
module Derive.Scale.All (lookupScale, docs, scales, shadowed) where
import qualified Data.Map as Map

import qualified Util.Lists as Lists
import qualified Util.Maps as Maps
import qualified Derive.Derive as Derive
import qualified Derive.Scale as Scale
import qualified Derive.Scale.BohlenPierce as BohlenPierce
import qualified Derive.Scale.Edo as Edo
import qualified Derive.Scale.Harmonic as Harmonic
import qualified Derive.Scale.Hex as Hex
import qualified Derive.Scale.Interpolate as Interpolate
import qualified Derive.Scale.Java as Java
import qualified Derive.Scale.Just as Just
import qualified Derive.Scale.Legong as Legong
import qualified Derive.Scale.Octa as Octa
import qualified Derive.Scale.Raga as Raga
import qualified Derive.Scale.Ratio as Ratio
import qualified Derive.Scale.Selisir as Selisir
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.Scale.Wayang as Wayang
import qualified Derive.Scale.WendyCarlos as WendyCarlos

import qualified Perform.Pitch as Pitch

import           Global


lookupScale :: Scale.LookupScale
lookupScale = Scale.LookupScale lookup
    where
    lookup env scaleId = make env <$> Map.lookup scaleId scales
    make _ (Scale.Simple scale) = Right scale
    make env (Scale.Make _ _ make) = make env lookupScale

-- | (scale_id, pattern, doc)
docs :: [(Pitch.ScaleId, Text, Derive.DocumentedCall)]
docs = map extract (Map.elems scales)
    where
    extract (Scale.Simple scale) = (Scale.scaleId scale,
        Scale.scalePattern scale, Scale.scaleCallDoc scale)
    extract (Scale.Make scaleId (pattern, doc) _) = (scaleId, pattern, doc)

-- | This is the hardcoded scale map.  It is merged with the static config
-- scale map at startup.
scales :: Map Pitch.ScaleId Scale.Definition
shadowed :: [Pitch.ScaleId]
(scales, shadowed) = mk $ concat
    [ BohlenPierce.scales
    , Edo.scales
    , Hex.scales
    , Interpolate.scales
    , Java.oldScales
    , Just.scales
    , Harmonic.scales
    , Legong.scales
    , Octa.scales
    , Raga.scales
    , Ratio.scales
    , Selisir.scales
    , Twelve.scales
    , Wayang.scales
    , WendyCarlos.scales
    ]
    where
    mk = second (map fst) . Maps.unique . Lists.keyOn Scale.scaleIdOf
