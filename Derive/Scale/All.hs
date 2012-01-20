{- | Hardcoded scale map.

    This is to scales as Derive.Call.All is to calls.
-}
module Derive.Scale.All where
import qualified Data.Map as Map

import qualified Derive.Scale as Scale
import qualified Derive.Scale.Ratio as Ratio
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.Scale.Semar as Semar
import qualified Derive.Scale.Wayang as Wayang

import qualified Perform.Pitch as Pitch


-- | This is the hardcoded scale map.  It is merged with the static config
-- scale map at startup.
scales :: Map.Map Pitch.ScaleId Scale.Scale
scales = Map.fromList $ map (\scale -> (Scale.scale_id scale, scale))
    [Ratio.scale, Twelve.scale, Semar.scale, Wayang.scale]
