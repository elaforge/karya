-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

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
    ]

tir_elaforge :: Korvai
tir_elaforge = tirmanam $ elaforge $ korvaiV adi
    [ __D 2.5 . trin "v_" (r3 "Nd_") (r2 "Nd_") "Nd_" . v
    -- , __D 5.25 . su (trin "v_" (r3 "Nd_") (r2 "Nd_") "Nd_" . v)
    , __D 2 . tri123 "D_o" (su "N_ktok") . od
    , __D 2 . tri123 "D_o" (su p6) . od
    , __D 4 . tri "D_" "Dnpk"
    , __D 4 . __ . tri (su "u_pkno") "NN_" . u
    , __D 5 . tri "D__" "ko"
    ]

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

-- -- ? why do these not end on 0.75?
-- adi_3_eddupu :: Korvai
-- adi_3_eddupu = rohan $ date 2026 1 10 $ korvaiV adi
--     [ "NokN _N_o kN_N __ok N_N_ ___o k"
--     , r3 "kookN_" . r3 p5
--     ]

adi_6_eddupu :: Korvai
adi_6_eddupu = rohan $ date 2026 1 10 $ korvai adi $ map (eddupu 1.5 • s)
    -- korvai
    [ r3 $ r3 "k_D_k" . tri "N_k" (r3 p5) -- x3 to end on 1.5
    -- arudhi
    , su $ "N___" .  r3 p6 . r3 (k.__.p6) . r3 ("k_k_".p6)
    -- tirmanam
    , __D 4 . __ . r3 p7
    , __D 4 . tri "D_" p6
    ]

adi_chatusra :: Korvai
adi_chatusra = rohan $ date 2026 1 10 $ korvaiV adi
    -- taka, takatiku, but played kptp instead of kpnp, and slow
    [ reduce3 2 ("N_k".p5) "k_t_kook" . "N_k" . tri2 "N_kpk" "N_kkptp" p5
    -- irregular reduction
    , g "kD_kD_k_k_D__".p5.__ . g "D_kD_k_k_D__".p5.__ . g "kD_k_k_D__"
        . tri __ p5
    , reduce3 1 ø (su "k_t_k_kto_") . sd p6
        . su ("k_k_kto_" . "k_kto_") . p6 . su "k_kto_" . su p6
        . sd p6 . p6 . su p6
    , tri2 taka takatiku (g "k_tkktkkooko_")
        . tri2 (od.__.taka) (od.__.takatiku) p5
    -- expand by repetition ab abb abbb
    , "kD_kD_" . "k_k_D__" . "kD_kD_" . r2 "k_k_D__" . "kD_kD_" . r3 "k_k_D__"
        . triAAB ø
            ("N__D_" . p5 . "N_N_N__D_" . p5)
            ("N__D_" . p5 . "N__D_" . p5)
    -- expand with repetition aaa bbb ccc
    , r3 (oktp."ko_") . r3 ("k_".oktp."ko_") . r3 ("k_k_".oktp."ko_")
        . tri __ (r3 (oktp.p5))
    -- arudhi
    , mconcatMap (prefixes ktkno) ["_p_p_k", "_p_p_", "_p_k", "_p_", "__"]
        . sd p5 . tri "N_k" (tri __ p5)
    -- Sankaran reduction with irregular reduction.
    , su (mconcat $ map g
        [ "ktooktpknpk_pu_k"
        , "pktoo  knpk_pu_k"
        ,   "koo  knpk_pu_k"
        ,       "oknpk_pu_k"
        ,         "npk_pu_k"
        ,           "k_pu_k"
        ,             "pu_k"
        ]) . sd p6 . p6 . tri __ (su p6)
    , let ktpk = su (su "ktpk") in
        tri __ (ktpk.d) . tri __ (o.ktpk.d) . tri __ (o.d.ktpk.d)
    -- sequence
    , "N___" . "koD_D_D_n_" . su "ktpk"."okookoD_D_D_u___"
    . __D 1  . "koD_D_D_n_" . su "ktpk"."okookoD_D_D_u___"
    . "okoo" . "koD_D_D_n_" . su "ktpk"."okookoooooooD___"
    . "_npk" . "koD_D_D_n_" . su "ktpk"."okookoD_D_D_u___"
    , tri "o_" (g "N___kook_N_kkook_o_k")
    ]
    where
    ktkno = map g [k, t, k, n, o]
    oktp = su "oktp"
    taka = su "kp"
    takatiku = su "kptp"

adi_tirmanams :: Korvai
adi_tirmanams = rohan $ tirmanam $ date 2026 1 10 $ korvaiV adi
    [ tri "o_" ("k_" . su ("N_pk".nakatiku."npk_k_pk".nakatiku) . "o_k_")
    , su $ suffixes ("ktooktpk".nakatiku) ["pko_", "pkpko_", "pkpkpko"]
    , "N_" . r3 (p6 . nadai 6 p6)
    , __D 7.75 . r3 (tri "d_" (su "kook") . su "tp")
    -- TODO why doesn't the t turn into a k on 2nd reduction?
    , tri "o_" (su $ reduce3 2 ø "k_t_oknpupkto_" . "ktkt")
    , __D 6 . triAAB "N_" (g "tkoonooko_k_") (g "ko_k_N_ko_k_")
    , __D 1 . "_kDk DkD__kD_kD_kD__kD__kD__k"
    , __D 1 . __ . trin "D__" (tri od k) (tri "D_" k) (tri "D__" k)
    , join "o_" (map (tri o) (reduceToL 1 1 ("pktk".su "kt"))) . o
    ]

adi_mohra :: Korvai
adi_mohra = rohan $ date 2026 1 10 $ korvaiV adi
    -- TODO I think these are mohra schemas, but how to complete them?
    [ "N__k" . su ("N_pk".nakatiku) . "o_k__"
        . "N__" . su ("N_pk".nakatiku."t_o_ktpk".nakatiku)
    . "N__k" . su ("N_pk".nakatiku) . "o_k__"
        . "N__" . su ("N_pk".nakatiku."pu_k")."okoko_"
    , su $ "__Nd_ou_ __ktktpk npk_k_pk".nakatiku
         . "__Nd_ou_ __ktktpk" . r2 "__Nd_ou_"
    , "Nok_ kook _ktk no_k tokt o_ko Tknp k_ko"
    . "okko ok_kptpk tkno" . r2 (su "pu_k"."oko___")
    ]

adi_tisra :: Korvai
adi_tisra = rohan $ date 2026 1 10 $ korvaiV adi $ map (nadai 6)
    -- arudhi
    [ sd $ r3 $ sd p6 . k.__.p6 . r3 (k.su p6)
    -- transition / arudhi
    , r3 $ "k_otkn".ktpk."d___ kkotkn" . tri "d__n" ktpk
    -- sequence
    , "onkd_kd_nkd_" . r3 "pnkd_kd_nkd_" . r2 ("onkd_kd_nkd_" . "pnkd_kd_nkd_")
    . r2 ("onkd_k" . "pnkd_k") . r3 ("onkd" . "pnkd")
    . r2 ("D_kD_k_t_kd_ __kD_k_t_kd_ D_kD_k_t_kd_" . "TkN".ktpk."TkTkN".ktpk)
    -- TODO abstract patterns
    . tri (o.__6.u.__6) ("TkN".ktpk."TkTkN".ktpk) . r3 ("TkN".ktpk."TkTkN".ktpk)
    . r4 ("tkN".ktpk."tktkN".ktpk)
    . r2 (("TkN".ktpk."TkTkN".ktpk) . ("tkN".ktpk."tktkN".ktpk))
    . ("TkN".ktpk."TkTkN".ktpk) . r3 ("tkN".ktpk) . r3 ("tktkN".ktpk)

    , t123 "k_pktknpuook" p6 (d.__6) . tri (d.__6) (spread 3 ktkno . sd p5 . p5)

    -- arudhi
    , tri "_ooko_" "N___kook" . tri "o_o_u_" "_ooko__ooko__ook"
    , r3 "kookN_k" . r3 ("k_tkn" . su "ktpkpt". o)
    ]
    where
    ktpk = su "ktpk"

-- TODO this is a somewhat common pattern, move to Notation?
t123 :: Monoid a => a -> a -> a -> a
t123 pre mid end = pre.mid.end . pre.mid.mid.end . pre.mid.mid.mid.end

adi_kanda :: Korvai
adi_kanda = rohan $ date 2026 1 10 $ korvaiV adi $ map (nadai 5)
    [ "k_o" . r2 "N_N_N_k d__" . r2 "N_N_kd__" . r2 "N_kd__"
        . tri "d___" "N_N_N_k"
    , suffixes ("k_" . su ("N_pk".nakatiku))
        ["kookN__", "kook_kookN__", "kook_kook_kookN__"]
        . r3 p5 . tri __ p6 . tri "D_" p7
    -- transition / korvai
    , "NokNk TkNok npk_o N_N_k Noknp kk_oN _N_kN okk_o"
    . "N_N_k pkk_o N_N_kkk__" . tri "N__" "N_N_k"
    ]

adi_misra :: Korvai
adi_misra = rohan $ date 2026 1 10 $ korvaiV adi $ map (nadai 7)
    [ tri (o.__7) "k_t_ktk kooknpk" . o.__5 . r3 p7 . "N_k" . r3 ("k_t_".p5)
    -- cholu
    , __M (4*7) . r2 (su "N_ktokN_k_T_k_") . "NkDNkTk nkdnkTk"
    , __M (4*7) . "k_D_kD_ koD_kD_ okD_kD_" . su "ktko" . "D_kD_"
    ]
