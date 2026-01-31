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
    , sd "ND" . "pnpkpk onpk" . "__" . tri "o_" "onpk"
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
    [ tri (su "u_pkno") (r2 "NN_")
    , tri123 (su "u_pkno") "NN_"
    , tri123 tanggu (su p6)
    , trin tanggu (su p5) (su (p6.p7)) (su (p5.p6.p7))
    , su p5 . tanggu . su (r2 p5) . r2 tanggu . su (r3 p5)

    , tri tanggu "koNkok"
    , tanggu . reduceTo 3 2 "koNkokD__"
    , tri "D__" (sd "kok")
    , "D__" . reduceTo 3 2 "k_o_k_D__"
    , "D__" . expand 4 2 "k_o_k_D__"
    , "k_o_kD__" . "k_o_kD__D__" . "k_o_k"
    , su "k_o_k" . "D__" . r2 (su "k_o_k") . r2 "D__" . r3 (su "k_o_k")
    ] ++
    -- 3x = 18, 4x = 24 = 3 avartanam
    -- so, 123, 222, 321 + 3(3)3(3)3
    -- pk koNkok -> koNkok -> Nkok
    -- or su "_kpk"
    [ trin "D___" (trin "D__" p4 p3 p2) (tri "D__" p3) (trin "D__" p2 p3 p4)
        . __ . tri (su "u_pkno") "NN_"
    , tri "i___" (tri "D__" "koNkok") . __ . tri (su "u_pkno") "NN_"
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
    [ tri "D_" ("P_" .su ("u_pk".nakatiku)."o_k_") . od.__3 . r3 p7
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
    , __D 3.5 . reduceTo 3 1 "ktpknookD_"
    -- 3x of the above, with 5m karvai
    , tri "___" (reduceTo 3 1 "ktpknookD_")
    ]

k_tatdit :: Korvai
k_tatdit = date 2025 9 1 $ elaforge $ korvaiS adi
    [ reduceTo 5 2 "k_t_kookD__" . trin "k_D__" (r3 p5) (r3 p6) (r3 p7)
        -- 3 avartanams, I want divisible by 4, unless fast tintal
    ]

yella_tani :: Score
yella_tani = tani
    [ K yella_sequence
    , Comment "mohra"
    , K yella_mohra_korvai
    , K yella_tirmanam
    ]

yella_mohra_korvai :: Korvai
yella_mohra_korvai = date 2025 9 13 $ korvaiS adi
    [ purvangam . r3 ("u_i_".su"ktkt".o)
    , purvangam . r3 (su $ "pu_k__".nakatiku)
    , purvangam . r3 (su $ "pu__kp".nakatiku)
    , purvangam . r3 (su $ "u___ktkt pkpto_") -- or pktp ktkto
    ]
    where
    purvangam = reduceTo 8 2 ("o_k_D__".p5).p5.p5.od.__3

yella_tirmanam :: Korvai
yella_tirmanam = date 2025 9 13 $ korvaiV adi
    [ su $ sd (sd "NDDN") . "nod_ktpk".nakatiku
    . r2 (tri "N_pk" nakatiku)
    . nakatiku . "N_pk".nakatiku . r4 "N_k"
    ]

yella_sequence :: Korvai
yella_sequence = date 2025 9 13 $ korvai adi $
    [ x2 $ s $ "N_D_D_N_".ktpk."D_D_N_" . "N_d_d_n_".su "ktpkd___o_ktktpk"
    , x2 $ s $ "N_D_D_N_".ktpk."D_D_N_" . "N_d_d_n_".su ("o_ktktpk".nakatiku)
    , s $ "N_D_D_N_".su ("o_ktktpk".nakatiku) . "N_d_d_n_".su ("o_ktktpk".nakatiku)
    , s $ tri "D___" (su ("o_ktktpk".nakatiku))

    , s $ "N_D_D_N_".ou_k."D_D_N_" . "N_d_d_n_".ou_k.od.__.ou_k.ou_k
    , s $ "N_D_D_N_".ou_k."D_D_N_" . "N_d_d_n_".n.o.r3 ou_k
    , s $ "N_D_D_N_".tri o ou_k . "N_d_d_n_".tri o ou_k
    , s $ tri "N___" (tri o ou_k)

    -- madyakalam
    , x2 $ s $ r4 "N_dD_dD_" . o & r3 "n_dd_dd_" . t_o_faran
    , s $ "N_dD_dD_" . t_o_faran . "N_dd_dd_" . t_o_faran . tri "D___" t_o_faran
    , x2 $ s $ r2 d_nd . "D_nd_dn_d_nd_" . on.pu_k
    , s $ r2 (d_nd . "D_nd_" . on.pu_k)
    -- , s $ r2 ("D_nd_" . on.pu_k) . __4. tri "D_" (on.pu_k)
    , s $ r2 ("D_nd_" . on.pu_k) . __4. tri "D_" (on.pu_k)
    ]
    ++ nd_k_seq (g (n.o.ktok))
    ++ nd_k_seq (g (su "kpkD_kD_"))
    ++ map (s • su)
    [ r2 (r2 (sd "D__"."ktktpk").nakatiku)
    , r2 (r2 ("N_pk".nakatiku).nakatiku)
    , r2 (r2 "npktpk tptkpk".nakatiku)
    , r4 ("t_o_ktpk".nakatiku) -- volume on nakatiku
    . r7 "t_o_ktpk".nakatiku
    . r2 (r3 "t_o_ktpk".nakatiku)
    . r4 ("t_o_ktpk".nakatiku)
    . r8 nakatiku
    ]
    where
    nd_k_seq end =
        [ x2 $ s $ r4 nd_k . o & r3 nd_k' . end
        , s $ r3 nd_k . end . r3 nd_k' . end
        , s $ r2 (nd_k . end) . tri "D_" end
        ]
    nd_k = "ND_k"
    nd_k' = "nd_k"
    d_nd = "D_ND_DN_"
    t_o_faran = su ("t_o_ktpk".nakatiku)
    ou_k = su "ou_k"
    pu_k = su "pu_k"
    ktpk = su "ktpk"
    ktok = su "ktok"

s_sketch :: Korvai
s_sketch = date 2025 12 7 $ elaforge $ korvaiV adi $
    [ "d_kd_k_d__k_d__k_" . "d_kd_k_kd_k_kd_"
    , "d_kd_kd_k_d__k_d__k_d_d_kd_kd_k_"
    , "d_kd_k_d__k_d__k" . "d_kd_k_kd_k_kd_k"
    , "d_kd_k_kd_k_kd_k" . "d_kd_kdk_kd_dk_k"
    , "d_kd_k_d__k_d__k" . "_k_kd_kd_k_k_kd_"
    , "d_kd_k_d___k_d_k" . "d_kd_k_kd_k_kd_k"
    ]
    -- messing around with 5s and 7s with an initial theme of
    -- d_kd_k_d__"

trichy_reverse :: Korvai
trichy_reverse = korvaiS adi
    [ mconcat (reverse $ reduceToL 2 1 theme) . tri __ (su p6) . p6 . sd p6
    ]
    where
    theme = su "k_ktkook npk_pu_k"
