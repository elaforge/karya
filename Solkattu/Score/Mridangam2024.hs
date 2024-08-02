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

e_24_06_30 :: Korvai
e_24_06_30 = date 2024 6 30 $ ganesh $ exercise $
    comment "use different sarvalaghu, add speed w/ simpler sarva" $ korvaiS adi
    [ tkon $ su ("ktktpk".nakatiku) . sarvaD_ 2 ]
    where
    tkon = prefixes [p, k, o, n]
    -- r2 NNNdnNNd
    -- r2 NDNdnDNd
    -- NDDNNDDN

e_24_06_30_koraippu :: Korvai
e_24_06_30_koraippu = date 2024 6 30 $ ganesh $ koraippu $ korvaiS adi
    [ kor "pk" 5
    , kor (su "n_ktpk") 4
    , kor "kook" 3
    , kor (su "kpk_oD_N_k") 2
    , kor (su "kpnpk_oD_N_k") 1
    , kor p7 0
    , __D 2 . (p7b.p7).(p7b'.p7').p7b . tri p7
    ]
    where
    kor pat gap = __D 2 . tri_ pat (p7a.__M gap) . tri_ (karv gap) pat
    karv n = if n <= 0 then mempty else od.__n n
        -- TODO this is one of those karvai filled in with D, which should
        -- disappear when the karvai becomes 0
    p7a = g "N_kD_kN"
    p7b = g "N,D,ND,"
    p7b' = g "n,d,nd,"
    p7 = su "onpknok_oD_N_k"
    p7'= su "pnpknpk_oD_N_k"
