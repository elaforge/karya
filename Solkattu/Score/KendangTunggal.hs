-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE NoMonomorphismRestriction #-}
module Solkattu.Score.KendangTunggal where
import           Prelude hiding ((.), repeat)

import qualified Solkattu.Score.Mohra as Mohra
import qualified Solkattu.Tala as Tala

import           Solkattu.Dsl.Kendang


farans :: Korvai
farans = faran $ korvaiV adi $ map su $ concat
    [ map (make (t.k.p.k) (t.k.p.k . p.k)) -- (p.n.p.k) (p.n.p.k . t.k)
        [ k.p.k.t.p.k.o.k -- k.t.k.n.p.k.t.k
        , o.o.k.t.p.k.o.k -- o.o.k.n.p.k.t.k
        , o.o.t.t.p.k.o.k -- o.o.n.n.p.k.t.k
        , o.p.k.t.p.k.o.k -- o.t.k.n.p.k.t.k
        , i.__.i.t.p.k.o.k -- od.__.od.n.p.k.t.k
        , i.t.i.t.p.k.o.k -- o.d.o.n.p.k.t.k
        , i.p.i.t.p.k.o.k -- o.k.o.n.p.k.t.k
        , pk.p.o.t.p.k.o.k -- o&t.k.o.n.p.k.t.k
        , p.y.__.t.p.k.o.k -- p.u.__.n.p.k.t.k
        , i.y.__.t.p.k.o.k -- o.u.__.n.p.k.t.k
        ]
    , map (make (i.y.__.k) (i.y.__.k.p.k)) -- (o.y.__.k) (o.y.__.k . t.k)
        [ i.y.__.p.p.o.i.k
        , i.y.k.p.p.o.i.k
        , o.p.i.y.__.k.o.k
        , o.p.i.y.p.k.o.k
        ]
    , map (make (o.__.p.__) (o.t.p.k.o.k)) --  (o.__.k.__) (o.k.p.k . t.k)
        [ o.p.o.d.p.o.d.p -- o.k.o.o.k.o.o.k
        , o.__.p.o.p.o.t.p -- o.__.k.o.k.o.o&t.k
        , o.o.p.o.p.o.t.p -- o.o.k.o.k.o.o&t.k
        , o.__.p.k.p.o.t.p -- o.__.k.t.k.o.o&t.k
        , o.o.p.k.p.o.t.p -- o.o.k.t.k.o.o&t.k
        , p.__.p.t.p.o.t.p -- k.__.k.t.k.o.o&t.k
        , p.k.p.t.p.o.t.p -- k.p.k.t.k.o.o&t.k
        , t.p.k.k.p.o.i.p -- n.k.p.p.k.o.o.k
        ]
    -- , [ make (i.i.k.t) (p.k.p.k . t.k) (p.k.i.i.k.t.p.k)
    , [ make (o.o.p.k) (p.k.p.k.t.k) (p.k.o.o.p.k.t.k)
      -- , make (n.i.i&k.__) (i&k.__.y.__ . p.k) (n.i.i&k.__.y.__.p.k)
      , make (t.i.pk.__) (pk.__.y.__.p.k) (t.i.pk.__.y.__.p.k)
      ]
    ]
    where
    make fill1 fill2 pattern =
        long . long
        . group pattern . group pattern . long
        . r2 short . fill1 . long
        . r3 short . fill2 . nakatiku
        where
        long = group pattern . nakatiku
        short = takeM 6 pattern

exercise1 :: Korvai
exercise1 = exercise $ date 2026 1 25 $ korvaiV Tala.any_beats
    [ r4 "kipp" -- TODO kipp or ippk?  Last does seem to start on i.
    , r4 "kipkpp"
    , r4 "kipkppkp"
    , "ipkppkpkipod_kpi_kpod_kpkpkpkppk"
    ]
    -- Should this be i for kum?

korvais :: Korvai
korvais = korvaiV adi
    [ tri "o_pk" nakatiku
    , reduce3 2 p5 "p_p_yodyo__" . trin "o__" p5 ("pk".p5) ("pktk".p5)
    ,    tri "d_" (su $ reduce3 2 ø "p_p_okpktiyod_" . "kpkp")
    ]

karaikudi_korvai :: Korvai
karaikudi_korvai = date 2026 4 28 $ source "Karaikudi Mani" $ korvaiS1 adi $
    su $ reduce3x 4 2 theme . tri "__" (r3 "po_" . r3 p7)
    where theme = "p_p_okpktiyod_"
    -- where theme = "p_p_okpktoyod_"

reduce3x :: Pretty sollu => FMatra -> FMatra -> SequenceT sollu
    -> SequenceT sollu
reduce3x to by seq = mconcatMap r3 (reduceToL to by seq)

sarva :: Korvai
sarva = sarvalaghu $ korvaiV adi
    [ "i_t_t_ipktt_t_i_" . "i_t_t_i_it_tt_to" -- nddn
    , r4 "o_po_itp" -- d_nd_dn_
    , r4 "o__tp_tp" -- o__nd_Nd
    ]

mohra_sequence :: Korvai
mohra_sequence = korvaiV adi $ map su
    -- tang gu, tarikitataka
    [ let trkttk = "tkttkt" in
        r2 (sd "d__" . trkttk) . sd "d_p_" . r2 (sd "i__" . trkttk) . nakatiku
    -- thom takadit thom kitataka
    , r2 "t_okp_d_pkpk" . nakatiku . r2 ("d_pk".nakatiku) . nakatiku
    -- takita takita
    , r2 "tkppkppkppkt" . nakatiku . r2 "pkppkttkppkp" . nakatiku
    -- faran
    , r2 ("pkuopkpk" . nakatiku) . r3 "pkoupkpk" . nakatiku
    , r5 "pkuo" . "pkpk" . nakatiku . r5 "pkuo" . "P_pk".nakatiku
    , r3 (r2 ("d_pk".nakatiku) . nakatiku) . tri "d___" nakatiku
    ]

k_mohra :: Korvai
k_mohra = mohra $ korvaiS1 adi $ Mohra.make su Mohra.A1
    ( "P___" . "y_pk".nakatiku
    , "tkp_y_pk".nakatiku
    , "P___" . "y_pk".nakatiku
    )
    ( "iy_pd_i_d_i_d___"
    , "iy_pd___"
    , tri "d___" "iy_pd_i_"
    )

-- * layamrutha varshini

kumar :: Korvai -> Korvai
kumar = source "Patri Satish Kumar"

kumar1 :: Korvai
kumar1 = date 2026 6 12 $ kumar $ korvai adi
    [ x3 $ s $ r3 tang_kitataka . end . "d_"
    , s $ r3 (r2 tang_kitataka . end . "d_")
        . tri "d_" (tang_kitataka . end) . end . end . d
    , s $ __D 5 . r4 "y__"
    , x2 $ s $ ndnd . ndnd `replaceEnd` "_y_y___"
    , x3 $ s $ ndnd . ndnd . __.__ . t9.t8.t7.t6
    , x2 $ s $ ndnd . ndnd . "d__" . join __ [t8, t7, t6, p5]
    , s $ ndnd . ndnd . "y___" . join "d_" [t7, t6, p5, "pd".su "pktt"]
        . ndnd . ndnd . "y___" . join "d_" [t7, t6, p5, i.su "tkttkt"]
    -- -- playing: "A_A__" Pi_xxo, ktkxo
    , x2 $ s $ ndnd . ndnd . "y_y__" . join "d__" [t6, p5, "poip", su p6]
    , s $ ndnd . ndnd . join __
        ["pktd_".p5, t9, t8, t7, t6, p5, "poip", "ttp", "tp", "t"]
    , s $ "t_po" `replaceStart` ndnd . ndnd . ndnd
        . join (r3 "pd__")
            (map r3 [t9, t8, t7, t6, p5, "poip", "ttp", "tp", "t"])
        . r3 "pd__"
    ]
    where
    tang_kitataka = su $ "d_pk".takatari
    end = g "d_pd_p_d_p_o"
    ndnd = sd "titi". "tt" . sd "itd"
    t9 = kpnp.p5
    t8 = "pd_".p5
    -- -- alternate: A_i_kno
    t7 = "P_i_kto"
    t6 = "Pi_kto"

kpnp :: Sequence
kpnp = p.k.t.k

kp :: Sequence
kp = p.k

kumar2 :: Korvai
kumar2 = date 2026 6 12 $ kumar $ korvai adi
    [ x2 $ s $ sarva . sarva2 . tadin.p5
    , s $ sarva . tadin.p5 . rs 2 tadin p5 . rs 3 tadin p5
    , s $ sarva . sarva . sarva1 . tadin.p5
        . sarva . sarva1 . rs 2 tadin p5
        . sarva . sarva1 . rs 3 tadin p5
        . sarva . tadin.p5 . sarva . rs 2 tadin p5 . sarva . rs 3 tadin p5
        . sarva1 . tadin.p5 . sarva1 . rs 2 tadin p5 . sarva1 . rs 3 tadin p5
        . tadin.p5 . rs 2 tadin p5 . r3 tadin
        . trin __ (r3 p5) (r3 (g (su "p_t_p_kpd_"))) (r3 (g (su "p_t_pkppd_")))
    , s $ sarva . sarva1 . tadin.p5
    , s $ sarva1 . tadin.p5 . rs 2 tadin p6 . rs 3 tadin p7
    , s $ sarva . sarva1 . tadin.p5
        . sarva . sarva1 . rs 2 tadin p6
        . sarva . sarva1 . rs 3 tadin p7
    , s $ sarva . tadin.p5 . sarva . rs 2 tadin p6 . sarva . rs 3 tadin p7
        . sarva1 . tadin.p5 . sarva1 . rs 2 tadin p6 . sarva1 . rs 3 tadin p7
        . tadin.p5 . rs 2 tadin p6
        . r3 tadin . trin "d__" (r3 p7)
            (r3 (g ("p_t_" . su "p_kpo_")))
            (r3 (g ("p_t_". su "pkpko_")))
    , s $ sarva.sarva1 . tadin.p5
        . sarva.sarva1 . r2 (tadin.p5) . rs 2 tadin (kp.p5)
            . rs 3 tadin (kpnp.p5)
    , s $ sarva . sarva . sarva1 . tadin.p5
        . sarva . sarva1 . rs 2 tadin (kp.p5)
        . sarva . sarva1 . rs 3 tadin (kpnp.p5)
        . sarva . tadin.p5 . sarva . rs 2 tadin (kp.p5)
            . sarva . rs 3 tadin (kpnp.p5)
        . sarva1 . tadin.p5 . sarva1 . rs 2 tadin (kp.p5)
            . sarva1 . rs 3 tadin (kpnp.p5)
    , s $ tadin.p5 . rs 2 tadin (kp.p5) . r3 tadin
        . trin "y_y__" (r3 (kpnp.p5)) (r3 (kpnp.p5))
            ("i_p_k_t_o" . r3 (nadai 6 "y_y_y_y_p"))
    ]
    where
    sarva = sarva1 . sarva2
    sarva1 = "tktktto_"
    sarva2 = "__tktto_"
    tadin = "pd_"
    p6 = pattern "pi_kto"
    p7 = pattern "P_i_kto"
    rs n a b = repeat n a . repeat n b

{-
kumar3 :: Korvai
kumar3 = date 2026 6 12 $ kumar $ korvai adi
    [ s $ r2 sarvaA . noThom sarvaA . "__U__U__"
        . r3 sarvaA . "__UUU___"
    , s $ r2 sarvaA . sarva . sarva . noThom sarva
    -- 0:32
    , s $ sarva . "ND,N,D" . su ("N_pk".takatari.takatari)
    , s $ sarva . "ND," . su ("N_ktpk" . tang_kita.takatari)
    -- 0:44 -- ktok instead of ktpk
    , x2 $ s $ sarva . noThom sarva . sarva . su tang_kita2
    , s $ r2 $ sarva . sarva . su tang_kita2
    , x2 $ s $ sarva . su tang_kita2
    -- 1:38
    , s $ su $ tang_kita2 . noThom tang_kita2
        . r2 (tang_kita.takatari . "n_pk".takatari)
        . takatari . r2 tang_kita . takatari . r2 tang_kita
    -- 1:56
    , s $ sarva . sarva
    , s $ suffixes (su tang_kita) tadin123 . tri2 "kD_kD_" "kD_" (r3 p5)
    -- 2:20
    , s $ suffixes (su (tang_kita.takatari)) tadin123
        . tri2 "_kD_kD_" "_kD_" (r3 (su oknp.p5))
    -- 2:44
    , s $ suffixes (su (tang_kita.takatari.takatari)) tadin123
        . suffixes (r3 (su ktktoknp.p5)) ["D_kD_kD_", "D_kD_"]
        . su ktktoknp.p5. r3 (nadai 6 "D_D_D_D_k")
    -- 3:14
    , s $ sd (sd (r8 "U"))
    -- 3:20 (3:42)
    , s $ su $ r3 tang_kita . sd (r4 "U__" . "i__")
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
kumar4_transition = date 2026 6 12 $ kumar $ korvai adi -- TODO some Ds missing
    -- 0:39
    [ s $ r2 sarva1 . sarva_ . sarva1 . d_nd_ . r3 p5 . ou.__4
    -- 0:50
    , s $ n5 $ sarva5 . sarva5_ . sarva5 . r3 p5 . ou.__5
    , s $ sarva5 . sarva5_ . d_nd_ . r3 p5 . ou.__4
    -- 1:15
    , s $ n5 $ sarva5.sarva5_.sarva5 . r3 p5 . ou.__5
    -- 1:27
    , s $ r2 sarva1 . ou.__4 . n5 (sarva1.d_nd_.p5.ou.__5)
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
-}

takatari :: Sequence
takatari = nakatiku
