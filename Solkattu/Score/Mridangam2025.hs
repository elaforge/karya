-- Copyright 2025 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
module Solkattu.Score.Mridangam2025 where
import           Prelude hiding ((.))

import           Solkattu.Dsl.Mridangam


s_25_01_26 :: Korvai
s_25_01_26 = date 2025 1 26 $ elaforge $ sarvalaghu $ korvaiV adi
    [ theme . su "pk_kpk"."D_kD_" . theme . su "k_k_k_pk_kpkok_k"
    , theme . su "N_p,^,"."d_kd_" . theme . theme
    ]
    where
    theme = "N,nD_kD_"

s_nnd :: Korvai
s_nnd = date 2025 2 17 $ elaforge $ sarvalaghu $ korvaiV adi
    [ sd "onnD_NND_NND_NNo"
    , sd "onND_NND_NND" . "_nN_oD_o"
    , sd "_nND_NND_NN" . "pu_kD_pu_k"
    , sd "_nND_NND_N" . "nnpk"."noD_D_k_"
    , sd "_nND_NND_NN" . "pnpkpk onpk"
    , sd "_nND_" . "pnpkpk onpk" . sd "oNND_n"
    , sd "ND" . "pnpkpk onpk" . "__" . tri_ "o_" "onpk"
    , sd "pnND_NND_NND_" . su "_oD___oD___o"
    , sd "DnND_NND_N" . nadai 6 "N____nN__dD__nN__n"
    ]

e_sarva :: Korvai
e_sarva = date 2025 3 13 $ elaforge $ sarvalaghu $ korvaiV adi
    [ "N_D_ND_" . r2 (g "npndpndp") . __ . su ("t_o_ktpk".nakatiku)
    , "N_D_ND_" . r2 (g "npndpndp") . g "npndp" . "nook"
    , "N_D_ND_" . r2 (g "npndpndp") . n.su ktok."Tk_D_d"
    , "N_D_ND_" . r2 (g "npndpndp") . n.su ktok."Tko".on.su ktok
    , "N_D_ND_" . r2 (g "npndpndp") . n.su "ktokokokou_kou_k"
    , "N_D_ND_" . r2 (g (su "n_pkn_d_pkn_d_pk")) . g (su "n_pkn_d_pk")
        . su "n_u_ktok"
    ]

e_3sequence :: Korvai
e_3sequence = date 2025 3 6 $ tirmanam $ korvaiV adi $ map (__D 2 .)
    [ tri_ (su "u_pkno") (r2 "NN_")
    , tri123 (su "u_pkno") "NN_"
    , tri123 tanggu (su p6)
    , trin tanggu (su p5) (su (p6.p7)) (su (p5.p6.p7))
    , su p5 . tanggu . su (r2 p5) . r2 tanggu . su (r3 p5)

    , tri_ tanggu "koNkok"
    , tanggu . reduceTo 3 2 "koNkokD__"
    , tri_ "D__" (sd "kok")
    , "D__" . reduceTo 3 2 "k_o_k_D__"
    , "D__" . expand 4 2 "k_o_k_D__"
    , "k_o_kD__" . "k_o_kD__D__" . "k_o_k"
    , su "k_o_k" . "D__" . r2 (su "k_o_k") . r2 "D__" . r3 (su "k_o_k")
    ] ++
    -- 3x = 18, 4x = 24 = 3 avartanam
    -- so, 123, 222, 321 + 3(3)3(3)3
    -- pk koNkok -> koNkok -> Nkok
    -- or su "_kpk"
    [ trin "D___" (trin "D__" p4 p3 p2) (tri_ "D__" p3) (trin "D__" p2 p3 p4)
        . __ . tri_ (su "u_pkno") "NN_"
    , tri_ "i___" (tri_ "D__" "koNkok") . __ . tri_ (su "u_pkno") "NN_"
    ]
    -- each one is 6, can I sequence them?
    -- Yes, I wind up at +2 again!  6*3 = 18 - 16 = 2
    -- So actually 4x
    where
    tanggu = "D__"
    p3 = sd "kok"
    -- p3 = "koNkok"
    p2 = dropM 2 p3
    p4 = su "_kpk".p3

simple_korvai :: Korvai
simple_korvai = date 2025 5 2 $ rohan $ korvaiS adi
    [ tri_ "D_" ("P_" .su ("u_pk".nakatiku)."o_k_") . od.__3 . r3 p7
    , theme . su "pkno".theme . su "ktktpkno" . theme
        . __ . r3 p5
    ]
    where
    theme = su ("P___u_pk".nakatiku)."o_k_D_"

s_tirmanams :: Korvai
s_tirmanams = elaforge $ tirmanam $ korvaiV adi
    [ __.__."kt_p_k_ no_o_k_" . od.__7 . "kt_pk_no_ok_".od.__6
        . "ktpk_nook_".od.__5 . "ktpknook"
        . od
    , __D 4 . "kookD_ookD_okD_kD"
    , __D 4 . su ("k_u_ktpkD___" . "u_ktpkD___" . "ktpkD___" . "pkD")
    ]

k_tatdit :: Korvai
k_tatdit = date 2025 9 1 $ elaforge $ korvaiS adi
    [ reduceTo 5 2 "k_t_kookD__" . trin "k_D__" (r3 p5) (r3 p6) (r3 p7)
        -- 3 avartanams, I want divisible by 4, unless fast tintal
    ]

-- kanjira
{-

    k _ i tktktkkook
    k _ i tktkikkttkiki i
    k _ u pkn8------npk u pkn8------k _ u pkn8------pu ko k o k D
    0   .   1   .   2   .   3   .   X   .   0   .   X   .   0   .   |

    n8------
    tkktkktk
    or if fast, just
    tktktktk but emphasis takaTArikitataka

    work out timings for say 5m solo, so it's not lopsided
-}
