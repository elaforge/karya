-- Copyright 2025 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Solkattu.Score.Mridangam2025 where
import           Prelude hiding ((.))

import           Global
import           Solkattu.Dsl.Mridangam


s_25_01_26 :: Korvai
s_25_01_26 = date 2025 1 26 $ elaforge $ sarvalaghu $ korvaiV adi
    [ theme . su "pk_kpk"."D_kD_" . theme . su "k_k_k_pk_kpkok_k"
    , theme . su "N_p,^,"."d_kd_" . theme . theme
    ]
    where
    theme = "N,nD_kD_"
