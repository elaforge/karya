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

karaikudi_korvai :: Korvai
karaikudi_korvai = date 2026 4 28 $ source "Karaikudi Mani" $ korvaiS1 adi $
    su $ reduce3x 4 2 theme . tri "__" (r3 "ko_" . r3 p7)
    where
    -- TODO k t doesn't reduce to k because it's followed by p.
    theme = "k_t_pknpupkto_"

reduce3x :: Pretty sollu => FMatra -> FMatra -> SequenceT sollu
    -> SequenceT sollu
reduce3x to by seq = mconcatMap r3 (reduceToL to by seq)

end :: Korvai
end = korvaiS1 adi $
    "U__kD_onNNkD__NNkNNkD_oD_oD_" . nadai 6 (su ("__ok".nakatiku)).od
