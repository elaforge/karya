-- Copyright 2026 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
module Solkattu.Score.Mridangam2026 where
import           Prelude hiding ((.))

import           Solkattu.Dsl.Mridangam


c_tadin :: Korvai
c_tadin = elaforge $ date 2026 2 13 $ korvaiV adi
    [ __D 5 . "_" . tri "D_" (r3 "nd_" . "N_k_")
    , __D 3.75  . tri2 "D_np" "D_npnp" (r3 "nd_" . "N_k_")
    ]
