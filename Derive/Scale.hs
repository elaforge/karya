{- | Hardcoded scale map.
-}
module Derive.Scale where
import qualified Data.Map as Map

import qualified Derive.Twelve as Twelve

import qualified Perform.Pitch as Pitch


scale_map :: Pitch.ScaleMap
scale_map = Map.fromList $ map (\scale -> (Pitch.scale_id scale, scale))
    [Twelve.scale]
