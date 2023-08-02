-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Hardcoded scale map.

    This is to scales as Derive.Call.All is to calls.
-}
module Derive.Scale.All2 (scales, shadowed) where
import qualified Data.Map as Map

import qualified Util.Maps as Maps
import qualified Derive.Derive as Derive
import qualified Derive.Scale.Java as Java

import           Global


scales :: Map Derive.CallName Derive.ScaleCall
shadowed :: [Derive.CallName]
(scales, shadowed) = fmap Map.keys $ Maps.uniqueUnions
    [ Java.scales
    ]
