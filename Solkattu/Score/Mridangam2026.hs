-- Copyright 2026 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
{-# LANGUAGE NoMonomorphismRestriction #-}
module Solkattu.Score.Mridangam2026 where
import           Prelude hiding (repeat, (.))

import           Solkattu.Dsl.Mridangam


c_tadin :: Korvai
c_tadin = elaforge $ date 2026 2 13 $ korvaiV adi
    [ __D 5 . "_" . tri "D_" (r3 "nd_" . "N_k_")
    , __D 3.75  . tri2 "D_np" "D_npnp" (r3 "nd_" . "N_k_")
    ]

karaikudi_korvai :: Korvai
karaikudi_korvai = date 2026 4 28 $ source "Karaikudi Mani" $ korvaiS1 adi $
    su $ reduce3x 4 2 theme . tri "__" (r3 "ko_" . r3 p7)
    where theme = "k_t_pknpupkto_"

reduce3x :: Pretty sollu => FMatra -> FMatra -> SequenceT sollu
    -> SequenceT sollu
reduce3x to by seq = mconcatMap r3 (reduceToL to by seq)

-- what is this from?
end :: Korvai
end = korvaiS1 adi $
    "U__kD_onNNkD__NNkNNkD_oD_oD_" . nadai 6 (su ("__ok".nakatiku)).od

mohan_concert :: Korvai
mohan_concert = date 2026 4 12 $ rohan $ korvaiS adi
    [ __D 4 . p.ktk . r3 "kD_" . dropM 1 ktk . r3 "kD__" . dropM 2 ktk
        . r3 "kD___"
    ]
    where
    ktk = "ktkk".su "kt".o
    -- ktk = "ktkkxo"

e_nakanakadin :: Korvai
e_nakanakadin = rohan $ exercise $ date 2026 5 29 $ korvaiV adi
    [ su $ r3 rh . end . rh . end . rh . end
        . r2 end . r2 "d_p,pu_k" . r4 "pu_k"
    ]
    where
    rh = g "d_p,^,d_p,^,n,^,"
    end = rh `replaceEnd` "pu_k"


-- * layamrutha varshini

kumar :: Korvai -> Korvai
kumar = source "Patri Satish Kumar"

-- (2) join 0: 9 8 7 6  x3
-- (3) join 1: 8 7 6 5  x2
-- (4) join 2: 7 6 5 4  x2
-- (5) join 3: 6 5 4 3  x2
-- (0) join 1: 10..1
-- +4d: join (kD) 999 .. 111
kumar1 :: Korvai
kumar1 = date 2026 6 12 $ kumar $ korvai adi
    [ x3 $ s $ r3 tang_kitataka . end . "U_"
    , s $ r3 (r2 tang_kitataka . end . "U_")
        . tri "D_" (tang_kitataka . end) . end . end . pu
    , s $ __D 5 . r4 "u__"
    , x2 $ s $ ndnd . ndnd `replaceEnd` "_A_A___"
    , x3 $ s $ ndnd . ndnd . __.__ . t9.t8.t7.t6
    , x2 $ s $ ndnd . ndnd . "u__" . join __ [t8, t7, t6, p5]
    , s $ ndnd . ndnd . "u___" . join "U_" [t7, t6, p5, "kD".su ktpk] -- 2:00
        . ndnd . ndnd . "u___" . join "U_" [t7, t6, p5, "/".su (kt.ktpk)]
    -- 2:22
    -- playing: "A_A__" Pi_xxo, ktkxo
    , x2 $ s $ ndnd . ndnd . "A_A__" . join "U__" [t6, p5, "koNk", su p6]
    , s $ ndnd . ndnd . join __ -- 2:52
        [pattern $ "kpkD_".p5, t9, t8, t7, t6, p5, "koNk", "NNk", "Nk", "N"]
    , s $ "U_od" `replaceStart` ndnd . ndnd . ndnd
        . join (r3 "kD__")
            (map r3 [t9, t8, t7, t6, p5, "koNk", "NNk", "Nk", "N"])
        . r3 "kD__"
    ]
    where
    tang_kitataka = su $ "D_pk".takatari
    end = g "D_kD_k_D_k__"
    ndnd = sd "NDNd". "nn" . sd "DND"
    t9 = pattern $ kpnp.p5
    t8 = pattern $ "kD_".p5
    t7 = pattern $ "P_i_kno"
    t6 = pattern $ "Pi_kno"

-- sar3 kd5     x2
-- sar2 kd5 2*kd5 3*kd5
-- sar4 sar1 kd5 sar3 2*kd5 sar3 3*kd5
--      sar2 kd5 sar2 2*kd5 sar2 3*kd5
--      sar1 kd5 sar1 2*kd5 sar1 3*kd5
--      kd5 2*kd5 3*kd5 555 555
-- sar3 kd5
-- sar1 kd5 2*kd6 3*kd7
-- sar3 kd5 sar3 2*kd6 sar3 3*kd7
-- =>   sar2 kd5 sar2 2*kd6 sar2 3*kd7
--      sar1 kd5 sar1 2*kd6 sar1 3*kd7
--      kd5 2*kd6 3*kd7 (3) 777 (3) 777
-- sar3 kd5     x2
-- kd5 2*kd7 3*kd9  (5 is taka tdgnt)
-- sar4 sar1 kd5 sar3 2*kd7 sar3 3*kd9
--      sar2 kd5 sar2 2*kd7 sar2 3*kd9
--      sar1 kd5 sar1 2*kd7 sar1 3*kd9
-- kd5 2*kd7 3*kd9 (5) 3*kd9 (5) 9 n3 999
kumar2 :: Korvai
kumar2 = date 2026 6 12 $ kumar $ korvai adi
    [ x2 $ s $ sarva . sarva2 . tadin.p5
    , s $ sarva . tadin.p5 . rs 2 tadin p5 . rs 3 tadin p5
    , s $ sarva . sarva . sarva1 . tadin.p5 -- 0:31
        . sarva . sarva1 . rs 2 tadin p5
        . sarva . sarva1 . rs 3 tadin p5
        . sarva . tadin.p5 . sarva . rs 2 tadin p5 . sarva . rs 3 tadin p5
        . sarva1 . tadin.p5 . sarva1 . rs 2 tadin p5 . sarva1 . rs 3 tadin p5
        . tadin.p5 . rs 2 tadin p5 . r3 tadin
        . trin __ (r3 p5) (r3 (g (su "k_t_k_kto_"))) (r3 (g (su "k_t_ktkto_")))
    , s $ sarva . sarva1 . tadin.p5 -- 1:43
    , s $ sarva1 . tadin.p5 . rs 2 tadin p6 . rs 3 tadin p7 -- 1:49
    , s $ sarva . sarva1 . tadin.p5 -- 2:01
        . sarva . sarva1 . rs 2 tadin p6
        . sarva . sarva1 . rs 3 tadin p7
    , s $ sarva . tadin.p5 . sarva . rs 2 tadin p6 . sarva . rs 3 tadin p7
        . sarva1 . tadin.p5 . sarva1 . rs 2 tadin p6 . sarva1 . rs 3 tadin p7
        . tadin.p5 . rs 2 tadin p6
        . r3 tadin . trin "U__" (r3 p7)
            (r3 (g ("k_i_" . su "k_kto_")))
            (r3 (g ("k_i_". su "ktkto_")))
    , x2 $ s $ sarva.sarva1 . tadin.p5 -- 3:19
    , s $ tadin.p5 . rs 2 tadin (kp.p5) . rs 3 tadin (kpnp.p5)
    , s $ sarva . sarva . sarva1 . tadin.p5 -- 3:43
        . sarva . sarva1 . rs 2 tadin (kp.p5)
        . sarva . sarva1 . rs 3 tadin (kpnp.p5)
        . sarva . tadin.p5 . sarva . rs 2 tadin (kp.p5)
            . sarva . rs 3 tadin (kpnp.p5)
        . sarva1 . tadin.p5 . sarva1 . rs 2 tadin (kp.p5)
            . sarva1 . rs 3 tadin (kpnp.p5)
    , s $ tadin.p5 . rs 2 tadin (kp.p5) . r3 tadin -- 4:50
        . trin "U_U__" (r3 (kpnp.p5)) (r3 (kpnp.p5))
            ("k_i_k_n_o" . r3 (nadai 6 "D_D_D_D_k"))
    ]
    where
    sarva = sarva1 . sarva2
    sarva1 = "N,N,NND_"
    sarva2 = "__N,NND_"
    tadin = "kD_"
    p6 = pattern "ki_kno"
    p7 = pattern "k_i_kno"
    rs n a b = repeat n a . repeat n b

kumar3 :: Korvai
kumar3 = date 2026 6 12 $ kumar $ korvai adi
    [ s $ r2 sarvaA . noThom sarvaA . "__U__U__"
        . r3 sarvaA . "__UU_U__"
    , s $ r2 sarvaA . sarva . sarva . noThom sarva
    , s $ sarva . "ND,N,D" . su ("N_pk".takatari.takatari) -- 0:32
    , s $ sarva . "ND," . su ("N_ktpk" . tang_kita.takatari)
    -- 0:44 -- ktok instead of ktpk
    , x2 $ s $ sarva . noThom sarva . sarva . su tang_kita2
    , s $ r2 $ sarva . sarva . su tang_kita2
    , x2 $ s $ sarva . su tang_kita2
    , s $ su $ r2 tang_kita2 -- 1:38
        . r2 (tang_kita.takatari.tang_kita)
        . r2 (takatari . r2 tang_kita)
    , s $ sarva . sarva -- 1:56
    , s $ suffixes (su tang_kita) tadin123 . tri2 "kD_kD_" "kD_" (r3 p5)
    , s $ suffixes (su (tang_kita.takatari)) tadin123 -- 2:20
        . tri2 "_kD_kD_" "_kD_" (r3 (su oknp.p5))
    , s $ suffixes (su (tang_kita.takatari.takatari)) tadin123 -- 2:44
        . suffixes (r3 (su ktktoknp.p5)) ["D_kD_kD_", "D_kD_"]
        . su ktktoknp.p5. r3 (nadai 6 "D_D_D_D_k")
    , s $ sd (sd (r8 "U")) -- 3:14
    , s $ su $ r3 tang_kita . sd (r4 "U__" . "i__") -- 3:20 (3:42)
        . r3 tang_kita . sd (r2 "DD_NN_" . flam p u . "__")
        . r3 tang_kita . r2 "D_ktpk" . "D_kD_kD_D_k_" . flam p u
    ]
    where
    sarva = "ND_N,ND,N,ND,N" . su ktpk
    sarvaA = "ND_N,ND_"
    tang_kita2 = r2 tang_kita . takatari
    tang_kita = "D_pk".takatari
    oknp = "oknp"
    ktktoknp = "ktktoknp"
    tadin123 = ["D_kD_", "D_kD_kD_", "D_kD_kD_kD_"]

kumar4_transition :: Korvai
kumar4_transition = date 2026 6 12 $ kumar $ korvai adi
    [ s $ r2 sarva1 . sarva_ . sarva1 . d_nd_ . r3 p5 . ou.__4 -- 0:39
    , s $ n5 $ sarva5 . sarva5_ . sarva5 . r3 p5 . ou.__5 -- 0:50
    , s $ sarva5 . sarva5_ . d_nd_ . r3 p5 . ou.__4
    , s $ n5 $ sarva5.sarva5_.sarva5 . r3 p5 . ou.__5 -- 1:15
    , s $ r2 sarva1 . ou.__4 . n5 (sarva1.d_nd_.p5.ou.__5) -- 1:27
        . sarva1.d_nd_.p5.ou.__4
        . n5 (sarva1.d_nd_.p5.ou.__5)
        . sarva1.d_nd_.p5 . n5 (r3 d_nd_ . p5) . r3 n_nd_ . p5
        . n5 (r3 n_nd_ . p5)
    ]
    where
    n5 = nadai 5
    sarva1 = "D_ND_N,ND_"
    sarva_ = "__ND_N,ND_"
    sarva5 = r3 "D_ND_" . "N,ND_"
    sarva5_ = "__ND_" . r2 "D_ND_" . "N,ND_"
    n_nd_ = "N,ND_"
    d_nd_ = "D_ND_"

kumar4_kanda :: Korvai
kumar4_kanda = date 2026 6 12 $ kumar $ korvai adi $ map (fmap (nadai 5))
    -- 1:57
    [ x2 $ s $ r3 (sarva1 . sarva_) . "D_D_" . tri "D_" takadinna
    -- 2:20
    , x2 $ s $ sarva1 . sarva_ . sarva1 . "kookD_kook"
    -- 2:31
    , s $ trin "D___" (tri "D_" takadinna) (tri "D_" (r2 takadinna))
        (tri "D_" (r3 takadinna)) . "D___" . tri2 "D_k_" "D_k_k_" (r3 p6)
    -- 2:57
    , s $ sarva1 . sarva_ . "D,ND," . r3 "N,ND,"
        . sarva1 . sarva_ . "D,ND," . r2 "N,ND," . od.__5
    -- 3:08
    , s $ r2 (r3 n_nd_ . p5.pu.__5)
        . r2 (r2 n_nd_ . p5.pu.__5)
        . r2 (n_nd_ . p5.pu.__5)
    , s $ tri123 (pu.__5) p5
    -- 3:32, 2x speed
    , s $ su $ r2 (r3 "ktkd_" . p5.u.__5) . r2 (r2 "ktkd_" . p5.u.__5)
        . r2 ("ktkd_".p5.u.__5) . tri123 "k_D_o" p5
    -- 3:45
    , s $ su $
        r3 "ktkd_" . p5.u.__5 . (d_nd_.n_nd_.n_nd_) . p5.u.__5
        . r2 (d_nd_.n_nd_ . p5.u.__5)
        . r2 (n_nd_.p5.u.__5) . tri123 (u.__5) p5
    , s $ su $
        r2 (d_nd_ . r2 n_nd_ . p5.u.__5)
        . r2 (d_nd_.n_nd_ . p5.u.__5)
        . r2 (n_nd_.p5.u.__5) . tri123 "k_D_o" p5
        . tri123 "k_D_o" p5 . tri123 "i_i__" p5
    ]
    where
    sarva1 = "D_ND_N,ND_"
    sarva_ = "__ND_N,ND_"
    n_nd_ = "N,ND_"
    d_nd_ = "D_ND_"
    takadinna = "kook"

kumar5 :: Korvai
kumar5 = date 2026 7 9 $ kumar $ korvaiV adi [sequence, nadai 6 sequence]
    where
    sequence = with (r4 sarva) . with (r3 sarva) . with (r2 sarva) . with sarva
        . mconcat expand . mconcatMap r2 expand . mconcatMap r3 expand
    with sarva = suffixes sarva expand
    expand = [p6, "k_".p6, "k_t_".p6]
    sarva = takadimi.takajonu
    takadimi = "nkdd"
    takajonu = "nkdd"

takatari :: Sequence
takatari = nakatiku
