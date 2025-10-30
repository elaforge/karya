-- Copyright 2025 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
module Solkattu.Score.MridangamTranscribe where
import           Prelude hiding ((.), repeat)

import           Solkattu.Dsl.Mridangam


-- t0 = Html.writeAll "balachandran.html" $ Korvai.Single yt_ramana_balachandran

realizeWide :: Korvai -> IO ()
realizeWide = realizeM (wider • wider)

yt_ramana_balachandran :: Korvai
yt_ramana_balachandran = source "https://www.youtube.com/watch?v=n3HDFlpmI60" $
    korvaiS adi $ map su
    [ r3 rh . "d_o_o_ou__o_N_k_"
    , "N_´_o_D_ ____D___" . r2 rho . "N__D__D´" . "Nno?_N_k"
    , "D__k_oD_ __o_D___" . "N_/_o_ou__o_ou__" . "n__d__d_ __k_d_pk"
        . "npktpkD´" . "Nno?_N_k"
    , "D__k_oD_ __D_N_".ktok . "o_k_K__D__o_D___" . "n__d__d_ __k_d_pk"
        . "npktpkD_" . "kokokook"
        . "oono".su"ktok"."n_k_N_D___" . "D_k_K__D__n_ktpk"
        . "n__d__d_k_k_d_pd" . "dpktpk" . "oouououoou"
    ,  "oouoUko_n_K_D___" . "D_k_K__D__N_ktpk"
        -- . "n__d__d_k_k_d___" . "n_k_" . su "ktpko_o_ktpko_k_o_ktpko_"
        -- . "n__d__d_k_k_d___" . "n_k_" . ktpk.o.o.ktpk.o.k.o.ktpk.o
        . "n__d__d_k_k_d___" . "n_k_" . "xqooxqokoxqo"
    -- , "u__o__N_k_K_D__,N_k_k_d" . __M 25 . tri_ "o_" (k.o.ktpk)
    -- , "ou__"

    -- n__d__d_ ____d___
    -- 1    .125    .25     .375    .5  .625    .75     .825    2
    -- n    d               d                   d
    --                      .375               .75
    -- n                    d                  d
    -- 0 3 3 4
    ]
    where
    -- ktpk = su "ktpk"
    ktok = su "ktok"
    rh = n.__3.d.__3.d.__6.d.__4
    rho = "N_´D__D_ ____D___"
    -- 0    3/8     6/8     12/8
    -- 0    .365    .75     1.5
    -- sometimes he rushes d__d__:
    -- 0   .33     .73

tirmanam_sikkil :: Korvai
tirmanam_sikkil = tirmanam $ korvaiV adi
    -- Sikkil Sisters, Dinamani Vamsa - Hari Kamboji - Adi, 3:54
    [ __D 4 . trin "d__kko" (r4 "Dk_") (r4 (su "D__k__")) (r4 "D_k")
    -- Or a variation, gradually move k back:
    , __D 4 . tri_ "d__kko" ("Dk_"."Dk_".su "D__k__"."D_k")
    -- The actual one sounds like the k moves sort of randomly.
    , __D 2 . tri123 "D_o" (g "Dk_")
    -- "Bhajare - Abheri - Adi"
    , __ . tri2 "D__" (tri_ "D_" "kkoDk") (tri_ "D_" "X_oDk")
    ]
    -- To be totally even, have to go to tisram:
    -- d  .  k  .  _  .  |
    -- d  .    k.  _  .  |
    -- d  .     .k _  .  |
    -- d  .  _  .  k  .  |

peshkar1 :: Korvai
peshkar1 = date 2025 9 5 $ rohan $ korvaiV adi
    [ "D__xD`N_p_N_ktpkD__N`kN_Y_n_p_k_"
    , "__koD`N___koD`N___koD`N_D`N_Y_n_"
    , "pknpY__qi_n_pkno" . "i,n,_kn_"&"o`o_o_oo" . "i,_kn,"&"o`o_o"."pk"
    , "doon_kd_Y_k_N__kN_i_k_N__kN_i_k_"
    , "U__xD`N_ ___kD_N_ ___N_knoY_n_p_n_"
    , "__koD`N___koD__N_kko?noD_kko?_N_"
    , "pknpi__qd_n_ pknoIkDnpkN_knpkD`n_"
    , "__on_kN_Y_k_" . "_D_kD_i_k__D_kD_i_k_"
    ]
