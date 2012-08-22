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
import qualified Derive.Scale.Octa as Octa
import qualified Derive.Scale.Ratio as Ratio
import qualified Derive.Scale.Semar as Semar
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.Scale.Wayang as Wayang

import qualified Perform.Pitch as Pitch


-- | This is the hardcoded scale map.  It is merged with the static config
-- scale map at startup.
scales :: Map.Map Pitch.ScaleId Scale.Scale
shadowed :: [Pitch.ScaleId]
(scales, shadowed) = mk $
    [Ratio.scale, Twelve.scale, Semar.scale]
    ++ Wayang.scales ++ Octa.scales ++ Just.scales
    where mk = second (map fst) . Map.unique . Seq.key_on Scale.scale_id
