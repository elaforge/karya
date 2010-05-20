{- | Hardcoded scale map.
-}
module Derive.Scale where
import qualified Data.Map as Map

import qualified Derive.Scale.Relative as Relative
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.Scale.Semar as Semar
import qualified Derive.Scale.Wayang as Wayang

import qualified Perform.Pitch as Pitch


-- | This is the hardcoded scale map.  It is merged with the static config
-- scale map at startup.
-- TODO: well, no it's not, not yet, but it should be easy to do someday.
scale_map :: Pitch.ScaleMap
scale_map = Map.fromList $ map (\scale -> (Pitch.scale_id scale, scale))
    [Relative.scale, Twelve.scale, Semar.scale, Wayang.scale]
