-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Hardcoded scale map.

    This is to scales as Derive.Call.All is to calls.
-}
module Derive.Scale.All where
import qualified Data.Map as Map

import Util.Control
import qualified Util.Map as Map
import qualified Util.Seq as Seq

import qualified Derive.Scale as Scale
import qualified Derive.Scale.Just as Just
import qualified Derive.Scale.Legong as Legong
import qualified Derive.Scale.Octa as Octa
import qualified Derive.Scale.Ratio as Ratio
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.Scale.Wayang as Wayang

import qualified Perform.Pitch as Pitch


-- | This is the hardcoded scale map.  It is merged with the static config
-- scale map at startup.
scales :: Map.Map Pitch.ScaleId Scale.Scale
shadowed :: [Pitch.ScaleId]
(scales, shadowed) = mk $
    [Ratio.scale, Twelve.scale, Legong.scale, Wayang.scale]
    ++ Octa.scales ++ Just.scales
    where mk = second (map fst) . Map.unique . Seq.key_on Scale.scale_id
