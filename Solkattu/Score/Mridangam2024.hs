-- Copyright 2024 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Solkattu.Score.Mridangam2024 where
import           Prelude hiding ((.), repeat)

import qualified Solkattu.Tala as Tala

import           Global
import           Solkattu.Dsl.Mridangam


misc_sarva :: Korvai
misc_sarva = date 2024 7 5 $ elaforge $ sarvalaghu $ korvaiS adi
    [ "D_" . r4 "kd_" . r2 (g "k_kd_") . r2 "kd_" . "k_"
    , "D_" . r4 "kd_" . r2 (g "k_kd_") . "kd_" . su "N_ktoko_k_"
    , "D_" . r4 "kd_" . r3 (g "k_kd_") . "kd_" -- 2 12 15 3
    , "D_" . r5 "kd_" . r3 (g "k_kd_") -- 2 15 15
    , r5 "kd_" . r3 (g "k_kd_") . "k_" -- 15 15 2
    , "D_" . r4 "kd_" . r2 (g "k_kd_") . "knpnd_k_"
    , "D_" . r4 "kd_" . g "k_kd_" . r2 "knpnd_" . "k"
    , "D_" . r4 "kd_" . "k_" . "knpnd_" . "knpnpnpnd_"
    ]
    -- for decrescendo, switch from "D_kD_" to "d_nd_nd"...
