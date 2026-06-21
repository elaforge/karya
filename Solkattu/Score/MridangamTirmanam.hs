-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
module Solkattu.Score.MridangamTirmanam where
import           Prelude hiding ((.), repeat)

import qualified Solkattu.Tala as Tala

import           Solkattu.Dsl.Mridangam


tir_short_adi :: Korvai
tir_short_adi = tirmanam $ korvaiS adi
    [ __D 1 . tri (p&u.__.k.k.o) (on.d.__.on.d.__) . p&u
    , __D 4 . su (tri (u.__4) (k.n.o.od.__.od.__3)) . u
    , __D 4 . __ . r3 (od.__ . su2 (p.k.n.p).k.__) . od
    , __D 7 . tri (od.__.k) (su (ktkt.p.kt.p.hv k.t.kt.p.k).od.od.k) . od
    ]

tirmanams :: Korvai
tirmanams = tirmanam $ korvaiV adi
    [ sarvaD_ 4.25 . su (tri "D_pkno" "N_N_k_")
    ]

tir_long_adi :: Korvai
tir_long_adi = tirmanam $ korvaiS adi
    -- 1:08:07
    [ let p8 = group (mconcatMap (.__) [kt, k.n, k.o, o.k])
        in __D 4.5 . p8 . (od.__.k.k.o) . p8 . (od.__.on.o'&n.k)
            . (group (su (stride 3 (k.t.k.n.k.o.o.k))))
        . od
    ]

tir_15 :: Korvai
tir_15 = tirmanam $ korvaiS adi
    [ __D 4 .__ . tri "D__" "D_k" -- 3 (3) 3 (3) 3
    ]

tir_patterns :: Korvai
tir_patterns = tirmanam $ korvaiS adi
    [ __D 6 . su (__ . r3 p5)
    , __D 5 . su (__.__6 . r3 p6)
    , __D 5 . su (__.__3 . r3 p7)
    , __D 5 . su (r3 p8)
    , __D 4 . su (__.__5 . r3 p9)

    , __D 5 . su (__.__7 . tri __ p5)
    , __D 5 . su (__.__4 . tri __ p6)
    , __D 5 . su (__ . tri __ p7)

    , __D 5 . su (__.__5 . tri (od.__) p5)
    , __D 5 . su (__.__2 . tri (od.__) p6)
    , __D 4 . su (__.__7 . tri (od.__) p7)
    , __D 5 . su (__.__3 . tri (od.__3) p5)
    , __D 5 . su (tri (od.__3) p6)
    , __D 4 . su (__.__5 . tri (od.__3) p7)
    ]

-- sam to sam
tir_sam_adi_kirkalam :: Korvai
tir_sam_adi_kirkalam = tirmanam $ date 2022 4 23 $ korvaiS adi
    [ tri "D__k" "kookokk_"
    ]

tir_long_rupaka :: Korvai
tir_long_rupaka = tirmanam $ korvaiS Tala.rupaka_fast
    -- 20:06
    [ tri (od.__.k) (su (ktkt.p.kt.p.hv k.t.kt.p.k).od.od.k)
    ]

-- https://www.youtube.com/watch?v=lSVqfzQBkMs
tir_mysore_vidiraj22 :: Korvai
tir_mysore_vidiraj22 = tirmanam $ date 2023 1 6 $ korvaiS adi
    -- 15
    [ __D 4 . r3 (__ . tri "D_o" "NNk")
    , __D 4 . __ . tri (su "D_ktpk") "NNk"
    , __D 4 . __ . su (tri (o&v.__.ktok) (r2 (o&v.__.o)))
    , __D 4 . __ . tri "v_o" (su "okokk_")
    -- 16
    , __D 4 . tri "D_" (su "t_o_ktok")
    , __D 4 . tri "D_" (su "k_oD_N_k")
    -- 24
    , __D 2 . tri "D_o" "NNkNNk"
    , __D 2 . tri (su "D_ktok") (su "N_N_pkN_N_pk")
    , __D 2 . tri (su "D_ktok") (su "ookD_kD_D_k_") -- (su "ookD_kookD_k")
    , __D 2 . tri (su "D_ktok") (su "npkD_kD_D_k_") -- (su "npknpknpknpk")
    , __D 2 . tri (su "D_ktok") (su "NkoD_kD_D_k_")
    , __D 2 . tri (o&v.__.o) (su "ktkto_ktkto_")

    , __D 2 . tri (su "D_pkno") "DDkDDk"
    -- 32
    , let kook = su "kook" in
        kook."D_kD_" . kook."D_kD_k_D_" . kook."D_kD_k_D_k__"

    ]

tir_itunes :: Korvai
tir_itunes = tirmanam $ date 2023 3 10 $ source "Mannargudi Easwaran" $
    korvaiS adi
    [ __D 7 . r2 (tri "D__" (su "kook"))
        -- TODO more elegant way to put in the extra k?
        . su "kook" . "D__" . su "kook" . "D_k" . su "kook"
    ]

tir_sivamani :: Korvai
tir_sivamani = tirmanam $ date 2023 1 20 $ korvaiS adi
    [ __D 2 . tri (su "v_pkno") "DD_NN_" -- another 63636
    ]

tir_icarnatic_guhan_kamalakiran_rupaka :: Korvai
tir_icarnatic_guhan_kamalakiran_rupaka =
    tirmanam $ date 2023 3 10 $ korvaiS Tala.rupaka_fast
    [ tri "D_o" (g $ su "ktpkpktp ktpkpk" . "DDk") -- at 26:00
    , tri "D_o" (g $ su "ktpktpkp tpktpk" . "DDk") -- alternate fingering
    ]

tir_icarnatic_guhan_kamalakiran_adi :: Korvai
tir_icarnatic_guhan_kamalakiran_adi = tirmanam $ date 2023 3 10 $ korvaiS adi
    -- TODO check out sequence at 3:30
    [ __D 0.5 . tri "u__kko" "Nd_Nd_" -- 3:51, also since 222, 123 works
    -- at 1:08:07
    , __D 4 . triAABg "D__NNk" "kt_kn_ko_ok_" (su (stride 3 "ktknkook"))
    -- trikalam version at 1:10:30
    ]
    -- TODO thani at 1:00:00

tir_indian_raga :: Korvai
tir_indian_raga = tirmanam $ date 2023 3 10 $ korvaiS adi
    -- Mayamma - Ahiri's Bliss
    [ __.__ . tri "ko_" "D´oD´oD__" -- 333(3)
    , __D 4 . tri (od.__6) "okokkoD__Nok" -- 66(6)
    ]

tir_misc :: Korvai
tir_misc = tirmanam $ date 2023 3 10 $ korvaiS adi
    [ __D 4 . triAAB "Dk" (su "tkooktok") (su "pu_knook")
    , __D 3 . let seq = su "pu_knook" in seq . "D__k".seq."D_kk".seq
    -- from facebook Shiva Ramesh, lalgudi tillana
    , __D 2 . tri123 "D__" (su "N_ktok") -- 3(3)33(3)333
    , __D 2 . tri (su "D_pkno") "DDkDDk" -- 33(3)33(3)33 = 8*3 = 4*6 = 6*4

    , __D 2.5 . su (tri "D_pk" "npnD_kD_N_k_")
    , __D 2 . su (tri2 "D__pk" "D__kpnp" "npnD_kD_N_k_")
    , __D 0.5 . su (trin "D__" (r3 p5) (r3 p6) (r3 p7))
    ]

tir_elaforge :: Korvai
tir_elaforge = tirmanam $ elaforge $ korvaiV adi
    [ __D 6.25 . triAAB "D__kko" (r3 "Dk_") (r3 "D_k") -- r3 tir_sikkil
    , __D 6 . su (triAAB "u___" "k_kto_k_k_kto_k_t_k_kto_" (r4 "k_kto_"))
    , __D 2.5 . trin "v_" (r3 "Nd_") (r2 "Nd_") "Nd_" . v
    -- , __D 5.25 . su (trin "v_" (r3 "Nd_") (r2 "Nd_") "Nd_" . v)
    , __D 2 . tri123 "D_o" (su "N_ktok") . od
    , __D 2 . tri123 "D_o" (su p6) . od
    , __D 3 . su (r3 "k_D___" . r3 "k_D__" . r3 "kD_")
    , __D 3 . r3 "kD__" . p5 . r3 "k_D__" . p5 . r3 "k__D__"
    , __D 3 . tri (od.__7) (k.o)
    , __D 4 . tri "D_" "Dnpk"
    , __D 4 . __ . tri (su "u_pkno") "NN_" . u
    , __D 5 . tri "D__" "ko"
    ]

tir_sikkil :: Korvai
tir_sikkil = tirmanam $ korvaiV adi
    -- Sikkil Sisters, Dinamani Vamsa - Hari Kamboji - Adi, 3:54
    [ __D 4 . trin "d__kko" (r4 "Dk_") (r4 (su "D__k__")) (r4 "D_k")
    -- Or a variation, gradually move k back:
    , __D 4 . tri "d__kko" ("Dk_"."Dk_".su "D__k__"."D_k")
    -- The actual one sounds like the k moves sort of randomly.
    , __D 2 . tri123 "D_o" (g "Dk_")
    -- "Bhajare - Abheri - Adi", 15:22
    , __ . triAAB "D__" (tri "D_" "kkoDk") (tri "D_" "X_oDk")
    ]
    -- To be totally even, have to go to tisram:
    -- d  .  k  .  _  .  |
    -- d  .    k.  _  .  |
    -- d  .     .k _  .  |
    -- d  .  _  .  k  .  |

rohan_end :: Korvai
rohan_end = rohan $ date 2025 2 8 $ korvaiV adi
    [ join (o.__) (map (\n -> tri o (dropM n seq)) [0, 1, 2, 3, 4]) .o.u
    ]
    where
    seq = g $ "pktk".su "kt"

reentry :: Korvai
reentry = date 2025 9 13 $ elaforge $ korvaiV adi
    -- sequences to re-enter after arrival
    [ od.__5 . g (su "ktpkpktkno").u.__3.su "n_ktpk" . r2 "N.dD.dD."
    ]

tir_transcribe :: Korvai
tir_transcribe = date 2026 2 5 $ tirmanam $ korvaiV adi
    [ __M 3 . su (prefixes ["pk_", "pkpk_", "pkpkpk_"] (r3 "N_kD_"))
    ]

tir_vanajakshi :: Korvai
tir_vanajakshi = date 2026 4 21 $ tirmanam $ korvaiV adi
    -- 12 beats, aka 6 rendekalai
    [ __D 4 . (tri123 (o&u.__6) (su "U_U___ktkto_") . "U")
        `replaceEnd` "U__U__U" -- alternate end
    ]
