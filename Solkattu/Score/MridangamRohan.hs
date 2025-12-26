-- Copyright 2024 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Solkattu.Score.MridangamRohan where
import           Prelude hiding ((.), repeat)
import           GHC.Stack (HasCallStack)

import qualified Solkattu.S as S
import qualified Solkattu.Score.Mohra as Mohra
import qualified Solkattu.Tala as Tala

import           Global
import           Solkattu.Dsl.Mridangam


-- * kanda tani

kanda_tani :: Score
kanda_tani = tani
    [ K kanda1
    , K kanda2
    , K kanda3
    , Comment "tisram"
    , K kanda3_tisram
    , K kanda_korvai3
    , Comment "farans"
    , K kanda4_farans
    , Comment "mohra"
    , K kanda5_mohra
    , K kanda_korvai4
    ]

kanda1 :: Korvai
kanda1 = korvaiS Tala.kanda_chapu
    [ "A___ ____ k_o_ D___ k_o_" . "D___ D___ k_o_ D___ ktpk"
    . "o_k_ o_o_ k_o_ D___ k_o_" . "D___ __N_ __N_ __u_ ____"
    , "D___ D___ k_o_ D___ ktpk" . "o_k_ o_o_ k_o_ D___ k_o_"
    . "D___ __N_ __N_ __u_ ____"
    , "__n_ p_k_ k_o_ D___ k_o_" . "D___ D___ k_o_ D___ ktpk"
    . "okk_ o_o_ k_o_ D___ k_o_" . "D___ __N_ __N_ __D_ ____"
    , "__n_ p_k_ k_o_ D___ k_o_" . "D___ D___ k_o_ D___ ktpk"
    . "okk_ o_o_ k_o_ D___ k_o_" . "D___ __N_ __N_ __u_ ____"
    , "A___ n_k_ k_o_ D___ k_o_" . "D___ D___ k_o_ D___ ktpk"
    . "o_k_ o_o_ k_o_ D___ n___" . "A___" . ktkno
    , "D___ __k_ k_o_ D___ n___" . "D___ D___ k_o_ D___ ktpk"
    . "okk_ o_o_ k_o_ D___ n___" . "A___" . ktkno
    , "o_k_ o_o_ k_o_ D___ n___" . "A___" . ktkno
    , "o_k_ o_o_ k_o_ D___".ktkno. on.__8
    . ktkno. "N_pk" . nakatiku . "N_pk npk_ ____"
    , "N___ o___ n_o_ D___ n_o_" . "D___ o_d_ D___ D___ n___"
    , trin (k.__8.i.__8)
        (g (u.__4 . ktkno . "k_kt_kno"))
        (g (u.__4 . ktkno . r2 "k_kt_kno"))
        (g (u.__4 . ktkno . r3 "k__k_kno"))
    , "N___ ____ n_o_ D___ n_o_" . on.__8.expand3 (on.__4) 2 "k_t_kt_kno"
    ]
    where
    ktkno = "ktknoktk".nakatiku

expand3 :: Sequence -> S.FMatra -> Sequence -> Sequence
expand3 sep dur pat = join sep (expands 3 dur pat)

kanda2 :: Korvai
kanda2 = korvaiS Tala.kanda_chapu $ map sd
    [ k.__8 . su "kook" . "D_odDodDod" -- delay on second o
    , "d_d,n,nnd," & "/_o_/__o/" . rh_&o . krh
    , rh & "__/_/__o/"  . rh_ & "/_o_/__o/"
    , "n,tkn"&"/_/".ktok."ND," . rh_ & "/"
    , rh & "____/__o/" . rh & "/_o_/_o"
    , n.su (r4 "ktkp").k . krh & "__/_/__o/"
    , "n,tkn,nnd," & "o_o_/__o/" . rh_ & "/_o"
    , n.su (r4 "ktkp").k . krh & "__/_/__o/"
    , n.su (r4 "ktkp").k . "k_d,n".su "ktkp".k."k__n,kk__n,k"
        . krh & "__/_/__o/"
    , "N,D,N".ktok."ND," . rh&"o_o_o__oo" . "n,d,"&"_oo".su "U__U__"."U__"
    ]
    where
    rh = "n,d,n,nnd,"
    rh_ = "n_d,n,nnd,"
    krh = "k_d,n,nnd,"
    ktok = su "ktok"

kanda3 :: Korvai
kanda3 = korvaiS Tala.kanda_chapu
    [ sd $ "N_D_kd_n_k d_n_kd_n_k" . "D_N_kd_n_kA_A_kd_n_k"
    , sd "D_N_kd_n_k" . "o_o" & r4 "d_n_k"
    , sd (p&"d_n_kd_n_k") . "o_o" & r4 "d_n_k"
    , sd "D_N_kd_n_k" . p & r4 "d_n_k"
    , "D_N_kd_n_k" . sd "d_n_k" . p&"d_n_kd_n_k" . sd "d_n_k"
    , "D_N_kD_N_k" . sd "D_n_k" . p&"d_n_kd_n_k" . sd "d_n_k"
    , "o_o__o_o" & r4 "d_n_k" . p & r4 "d_n_k"
    ]

kanda3_tisram :: Korvai
kanda3_tisram = korvaiS Tala.kanda_chapu $ map (nadai 3) $
    [ "o_o" & r3 rh . p & r3 rh . r3 p5
    , "U__N_kD_kN_kDok"
    , r2 "D_kn_kd_kn_kDok" . "D_kD_kN_kD_kDok"
    , o & "d_kn_kd_kn_kd_k" . "d__k__ ktknpuook"
    , "D_kN_kD_kDokD_kd_kd_k" . "npkk_kn_k" . "d__k__ ktknpuook"
    , "D_kN_kD_kD_kDok" . "D__k__ ktknpuook"
    ]
    where
    rh = "d_n_k"

kanda_korvai3 :: Korvai
kanda_korvai3 = korvaiS Tala.kanda_chapu $ map (nadai 3)
    [ tri (g "k__ ktknpuookookD__") . r3 (p5.p7)
    , tri122
        (g "k__ktknpuookoo_oo_")
        (g "on_ktknpuookoo_oo_") . r3 (p5.p7)
    , tri122
        (g "k__ktknpuookookook")
        (g "on_ktknpuookookook") . r3 (p5.p7)
    ]

kanda4_farans :: Korvai
kanda4_farans = korvaiS Tala.kanda_chapu
    [ sd "u__N_N_N_k" . sd "d__n_n_n_k"
    . "u___ __ktktpk" . nakatiku . "d___ __npktpk" . nakatiku
    , "d___ __ktktpkd___ __ktktpkd___ __ktktpkN_k_"
    . "d___ __ktktpkd___ __ktktpkd___ N_pk" . nakatiku
    , "N___ t_o_ktpk" . nakatiku . "N_N_ pu_o ktpk" . nakatiku
    . "N_N_ pko_N_pk" . nakatiku . "N_N_ N_pk nook" . nakatiku
    , "N_pk t_o_N_pk t_o_ktpk" . "N_pk". r2 nakatiku
    , "pu_ko_k_pu_ko_ktktpk" . "N_N_pu_ko_pu_ko_pu_k"
    ]

kanda5_mohra :: Korvai
kanda5_mohra = mohra $ korvaiS1 Tala.kanda_chapu $ Mohra.make id Mohra.A1
    ( sd "ktknok_N_" . "ktktpk"
    , "t_o_ktpk".nakatiku
    , sd "oktknokN_" . "ktktpk"
    )
    ( "N_k_o__kN_k_o___"
    , sd "oko_"
    , tri_ "N___" "pu_ko_k_"
    )

kanda5_mohra_raw :: Korvai
kanda5_mohra_raw = korvaiS Tala.kanda_chapu
    [ "k_t_k_n_ o_k___N_ __kt" . "ktpkt_o_ktpk" . nakatiku
    . "o_k_t_k_ n_o_k_N_ __kt" . "ktpkN_k_o__k N_k_o___"
    , "k_t_k_n_ o_k___N_ __kt" . "ktpkt_o_ktpk" . nakatiku
    . "o_k_t_k_ n_o_k_N_ __kt" . "ktpkN_k_o__k N_k_o___"
    , "k_t_k_n_ o_k___N_ __kt" . "ktpkt_o_ktpk" . nakatiku
    . "o_k_t_k_ n_o_k_N_ __kt" . "ktpko_k_o___ k_t_k_n_"
    . "o_k___N_ __ktktpk o_k_" . "o___k_t_k_n_ o_k___N_"
    . "__ktktpk pu_ko_k_ N___" . "pu_ko_k_N___ pu_ko_k_"
    ]

kanda_korvai4 :: Korvai
kanda_korvai4 = korvai Tala.kanda_chapu
    [ x3 $ s $ reduceTo3 4 2 "p_k_t_k_kto_" . r2 (r2 "D___" . r3 "kto_")
    , s $ "N___ ____ n_o_ D___ n_o_"
    . r3 (g "k_kto_") . "N___" . r3 (g "k_k_kto_") . "N___"
    . r3 (g (o.k.nakatiku))
    ]

tri122 :: Sequence -> Sequence -> Sequence
tri122 a b = a.b.b

reduceTo3 :: (HasCallStack, Pretty sollu) => S.FMatra -> S.FMatra
    -> SequenceT sollu -> SequenceT sollu
reduceTo3 to by seq = mconcatMap r3 (reduceToL to by seq)

-- * misra tani

misra_tani :: Score
misra_tani = tani
    [ K misra1
    , K misra_korvai1
    , K misra2_tisram
    , K misra_korvai3
    , K misra_farans
    , K misra_mohra
    , K misra_mohra_korvai
    ]

misra1 :: Korvai
misra1 = korvaiS Tala.misra_chapu
    [ sd $ "k_D_ __ N_ N_ d_ nk" . "koD_ __ N_ N_ u_ __"
    . "koD_ __ N_ N_ u_ __" . "kkoD __ N_ N_ D_ nk"
    . "koD_ __ N_N_u___" . su "okko" . "o_ __ N_ N_ D_ nk"
    . "kookokookokook" . "k__D__N_N_D_nk"
    . "kookokookokook" . "kkoD__NkNkD_nk"
    . "kookokookokook"
    . tri_ o (su "kookkook")
    . tri123 o (su "kook")
    , sd $ (u.__4) . r6 (u.__3).u.__ . su ("n_pk" . su nakatiku)
    . "D_nnd___d_nnd_" . "u_nnd_n_d_nnd_"
    . "N_NND___D_NND_" . "u_nnd_nddDnDDd"
    . "ndd ndd ndd DnDDd"
    , "d___n_ktpkn_ d_k_n_kt pkn_d_k_" --
    . "D___n_ktpkn_ d_k_n_kt pkn_d_k_"
    . "t_k_n_ktpkn_ d_k_n_kt pkn_d_k_"
    . "p___k___k_o_ o_k_N___ p_k_kook"
    . "D___N_ktpkN_ D_k_N_kt pk_ND_k_"
    . "p___k___k_o_ o_k_N___ p_k_kook"
    ]

misra_korvai1 :: Korvai
misra_korvai1 = korvaiS Tala.misra_chapu
    [ utarangam . spread 6 ktkno . r2 (spread 4 ktkno) . r3 (g (spread 2 ktkno))
    , utarangam . spread 6 ktkno . r2 (spread 4 ktkno) . r3 (g "ktktpkpto_")
    , utarangam . spread 6 ktkno . r2 (spread 4 ktkno) . r3 (g "oktpupkto_")
    ]
    where
    utarangam =
          sd theme . theme . dropM 2 theme . dropM 4 theme
        . sd (dropM 2 theme) . dropM 2 theme . dropM 4 theme
        . sd (dropM 4 theme) . dropM 4 theme
    theme = g "p_k_kook"

misra2_tisram :: Korvai
misra2_tisram = korvaiS Tala.misra_chapu $
    [ "N___ __k_ k_o_ o_k_ k_o_ o_k_ ktoo" --
    . "k___ N___ ktoo k___ N_k_ __N_ k___"
    , "N_d_ __k_ n_k_ n_d_ __k_ n_o_ ktpk" --
    . "N_d_ __k_ n_k_ n_d_ __k_ noo_ ktpk"
    . "N_d_ __k_ n_k_ n_d_ __k_ tkoo kook"
    . "N_d_ __k_ n_k_ n_d_ __k_ okoo kook"
    . "N_d_ __k_ n_k_ n_d_" . nadai 3 "nkd nkd nkd"
    . "N_d_ __k_ n_k_ n_d_" . nadai 3 "nkd nkd nkd"
    ] ++ map (nadai 3)
    [ "Nkd nkd nkd nkd nkd nkd nkd" --
    . "Nkd nkd nkd".p&n."kd nkd ".p&n."kd nkd"
    . "Nkd Nkd nkd ".p&n."kd nkd ".p&n."kd nkd"
    . "k_t _kn ok_ t_k nok _t_ kno"
    . "Nkd Nkd nkd ".p&n."kd nkd ".p&n."kd nkd"
    . "k_t _kn ok_ t_k nok _t_ kno"
    . "P_D _kn ok_ D_k nok _D_ kno"
    . "P_D _".su "ktkt" . "ok_ D_".su "kt kt"."ok _D_". su "ktkt".o
    , "d__ ktk d__ u__ ktk d__ ktk" --
    . "d__ ktk d__ d__ ktk d__ kpk"
    . "o__ ktk d__ u__ ook ook ook"
    . "d__ ktk d__ k_p ktk" . su ("n_pk".nakatiku)
    . "d__ ktk d__ k__ ktk npu ook"
    . "ook ook d__ k__ ktk npu ook"
    ]

misra_korvai3 :: Korvai
misra_korvai3 = korvaiS Tala.misra_chapu $ map (nadai 3) $
    [ r3 theme . tri_ (od.__6) p6
    , tri122 theme (ton theme) . tri_ (u.__6) p6 -- p6 = kt_k_kto
    , tri122 theme2 (ton theme2) . tri_ (u.__6) p6
    ]
    where
    theme  = g "k__ ktk npu ook ook N__"
    theme2 = g "k__ ktk npu ook ook ook"
    ton = replaceStart "on"

misra_farans :: Korvai
misra_farans = korvaiS Tala.misra_chapu $
    [ "u___ __k_ p_k_ t_k_ N_pk".nakatiku
    . "N_kd _kN_ kd_k N_kd _ktk tkoo".ktok."Tk"
    . "N_kd _kn_ kd_k n_kd _ktk tkoo".ktok."Tk"
    . "onkd _kN_ kd_k N_kd _ktk tkoo".ktok."Tk"
    . "tkoo" . ktok."Tk tkoo" . ktok."Tk tkoo".ktok."Tk tkoo"
    . ktok."Tk N___ tkoo".ktok."Tk N___ tkoo".ktok."Tk"
    , on.__6 . r3 (ktktpk.d.__6) . ktktpk.on.__4.k.__4
    , on.__6 . r3 (ktktpk.d.__6) . ktktpk.nakatiku
    -- Farans
    , "t_o_ktpk".nakatiku . "N_pk".nakatiku
    . "pu_oktpk".nakatiku . "N_pk".nakatiku
    . "N_pkt_o_ ktpkN_pkt_o_" . nakatiku
    . r3 "pu_ko_" . o.k.nakatiku
    ]
    where
    ktok = su "ktok"
    ktktpk = "ktktpk"

misra_mohra :: Korvai
misra_mohra = mohra $ korvaiS1 Tala.misra_chapu $ Mohra.make id Mohra.A2
    ( "upkto_k_ __N_ __npktpk".nakatiku
    , "tpupkt_o k_N_ __npktpk".nakatiku
    , "upkto_k_ __N_ __npktpk".nakatiku
    )
    ( r2 "pu_ko_k_ N_ ____"
    , "pu_ko_k_ N_____"
    , "pu_ko_k_ pu_ko_k_N_k_ pu_ko_k_"
    . "pu_ko_k_ N_pkpu_ko_k_ pu_ko_k_"
    )

misra_mohra_korvai :: Korvai
misra_mohra_korvai = korvai Tala.misra_chapu $
    [ x2 $ s $ purvangam . sd (sd (sd p5)) . sd (sd p5) . sd p5
    , s $ purvangam . sd (sd (sd p5)) . sd (r3 p5)
    , s $ sd $ od.__8 . "kkkook" . "D_" . r4 "ND_"
    , s $ nadai 3 $ tri123 "D__" "npuook"
    ]
    where purvangam = sd $ "k_D_kD_koD_kD_okD_kD_"


-- * rupaka tani

rupaka_tani :: Score
rupaka_tani = tani
    [ K rupaka1
    , K rupaka_korvai1
    , K rupaka2
    , K rupaka_korvai3
    , K rupaka_farans
    , K rupaka_mohra
    , K rupaka_mohra_korvai
    ]

rupaka1 :: Korvai
rupaka1 = korvaiS Tala.rupaka_tala $
    [ __D 3 . su (r4 "upkto_")
    . "A_ok koNN _oNk D_D_ noD_ nook"
    . "A_ok kook okk_ D_D_ noD_ nook"
    . "u_oo koD_" . su "ktokotok" . "o_o_ koD_" . su "ktokotok"
    . "N_o_ koD_" . su "ktokotokN___ktokotokN_otokN_otok"
    , "A_kk koD_ D_N_ d_d_ nnd_ d_n_" --
    . "__u_ __u_ __u_ __u_ __u_ _ook"
    . "A_kk koD_ D_N_ D___ __" . k_t_k_kt_kno
    . "D___ __" . k_t_k_kt_kno . "uu__ __" . k_t_k_kt_kno
    . "u___ __" . r3 k_t_k_kt_kno
    ]
    where
    k_t_k_kt_kno = su (g "k_t_k_kt_kno")

rupaka_korvai1 :: Korvai
rupaka_korvai1 = korvaiS Tala.rupaka_tala
    [ reduce theme . tri123 "D__" (su p6)
    , reduce theme2 . tri123 "DD_" (su p6)
    , reduce theme3 . trin "DDD" (su p6) (su (p6.p6)) (su (r6 (g "kto")))
    ]
    where
    reduce = reduce3 1 ø
    theme  = "ktk".su p6.od.__3
    theme2 = "PXP".su p6.od.od.__
    theme3 = "PXP".su p6.od.od.od

rupaka2 :: Korvai
rupaka2 = korvaiS Tala.rupaka_tala $
    [ "k___ ____" . su "nkook___ N___nkoo" . "k_Nk _Nk_" --
    , "N_D_ nkn_ d___ __d_ nkn_ d___" --
    . "N_D_ nkn_ d___ __".p&d."_ nkn_ d_nk"
    . "NkD_ nkn_ d___ __".p&d."_ nkn_ d_nk"
    . su "on_kD_k_" . "NkN_ D___ __D_ NkNo kNok"
    , "N_nn d.n. nnd. __nn d.n. nnd." --
    . "N_NN D.N. NND. __NN D.N. NND."
    , su "n_ktpkn_" . "d.n. nnd. __nn d.n. nnd." --
    . su "npktpkn_" . "d.n. nnd." . su "npnd_pnd _pnd_pnd _pnd_oN_"
    . "u_u_ d.n. nnd." . su "npnd_pnd _pnd_pnd _pnd_oN_"
    . "u_u_ d.n." . su "npnd_oN_ u___npnd _oN_u__k" . "doDk"
    , "N_kD _kn_ kd_k n_kd _kn_ kd_k" --
    . "N_kD _kn_ kd_k NokN okNo kNok"
    . "N_kD _kn_ kd_k" . r4 (su "o_ktpk")
    , r2 $ "N_kD_kn_kD_k" . nadai 6 (r3 "N_kD_k") --
    ] ++ map (nadai 6)
    [ "N_kD_k n_kd_k n_kd_k N_kD_k n_kd_k n_kd_k" --
    . "ookD_k n_kd_k n_kd_k ppkd_k n_kd_k n_kd_k"
    . "ookD_k n_kd_k ppkd_k n_kd_k ookD_k n_kd_k"
    . "ookD_k ppkd_k ookD_k ppkd_k ookppk ookppk"
    . r2 (su (r3 "N_ktoko_t_k_ n_ktoko_t_k_"))
    . su (r3 "o_t_k_N_ktok p_t_k_n_ktok")
    ]

rupaka_korvai3 :: Korvai
rupaka_korvai3 = korvaiS Tala.rupaka_tala $ map (nadai 6) $
    [ r3 theme . r3 p5 . r3 theme . r3 p6 . r3 theme . r3 p7 ]
    where theme = g "k__ktknpuookooko__"

rupaka_farans :: Korvai
rupaka_farans = korvaiS Tala.rupaka_tala $ map su
    [ "d_____n_ ktpkd___ __n_ktpk d_____n_ ktpkD_D_ __N_ktpk" --
    . "D_____np ktpkd___ __npktpk d_____np ktpkD_D_ __N_ktpk"
    . r3 (g "Tkoktknpktpk") . "N_pk".nakatiku
    -- Farans
    , "t_o_ktpk" . nakatiku.nakatiku. "pu_oktpk" . nakatiku.nakatiku --
    . "pko_N_pk" . nakatiku.nakatiku. "N_pknook" . nakatiku.nakatiku
    . r2 "N_pkt_o_ktpk" . r2 ("N_pk".nakatiku)
    . r2 "pu_ko_ktktpk" . r4 "pu_ko_"
    ]

rupaka_mohra :: Korvai
rupaka_mohra = mohra $ korvaiS1 Tala.rupaka_tala $ Mohra.make su Mohra.A3
    ("N_pk".nakatiku, "oktpu___", "N_pk".nakatiku)
    ( "o_k_N__ko_k_N___", "pu_ko___", r3 "pu_ko_k_")

rupaka_mohra_korvai :: Korvai
rupaka_mohra_korvai = korvai Tala.rupaka_tala $
    [ x2 $ s $ reduce3 2 ø theme . r3 ("PPP_".p5)
    , s $ reduce3 2 ø theme . r3 (su "oktpu___".p5)
    , s $ r2 "NkNNdk" . r4 "Nd_"
    , s $ su $ tri_ "N___" nakatiku . tri_ "N_pk" nakatiku
        . nakatiku . r3 "N_ktpk" . "N_o_k_D"
    ]
    where theme = "k_t_".su "ko"."D_N_"

-- * featuring fives

-- replacements:
-- ktkno -> p5
-- ktktpkpko -> ktktpktko
-- tpupktpk -> npupktpk

featuringFives :: Score
featuringFives = tani
    [ K featuringFives_start
    , K featuringFives_kanda
    , K featuringFives_farans
    , K featuringFives_mohra
    , K featuringFives_mohra_korvai
    ]

featuringFives_start :: Korvai
featuringFives_start = korvaiS adi $
    [ "ktkn o_kt knok tkno _ktk nokt knok tkno"
    . su "ktktpkpk o___ktkt pkpko_kt ktpkpko_ __ktktpk pko_ktkt pkpko_kt ktpkpko_"
    . su "oktpupkt o___oktp upkto_ok tpupkto_ __oktpup kto_oktp upkto_ok tpupkto_"
    , "Nd_k nd_k nd_k nd_k Nd_k nd_k nd_k nu__"
    . "Nd_k nd_k nd_k nd_k Nd_k nd_k nd_k nD__"
    , su $ "N_d___k_ n_d___k_ n_d___k_ n_o_ktpk N_d___k_ n_d___k_ n_d___k_ n_o_ktpk"
      . "N_d___k_ n_d___k_ n_d___k_ noo_ktpk N_d___k_ n_d___k_ n_d___k_ noo_ktpk"
      . "N_d___k_ n_d___k_ n_d___k_ tkookook N_d___k_ n_d___k_ n_d___k_ tkookook"
      . "N_d___k_ n_d___k_ n_d___k_ okookook N_d___k_ n_d___k_ n_d___k_ okookook"
      . "N_d___k_ n_d___k_ n_d___k_ n_o_ktpk N_d___k_ n_d_k___ ktknoktk tpupktpk"
      . "N_d___k_ n_d_k___ ktknoktk tpupktpk N_d___k_ n_d_k___ ktknoktk tpupktpk"
    -- arudhi
    , su "k___ktkn oktktpup ktpkk_kt _knoN___ ______k_ __ktknok tktpupkt pkk_kt_k"
    . su "nok_kt_k noN_____ ____k___ ktknoktk tpupktpk k_kt_kno k_kt_kno k_kt_kno"
    , su "N___p_k_ n_o_o_k_ t_k_o_o_ n_o_o_k_ t_o_ktpk N___t_o_ ktpkN___ t_o_ktpk"
    , su "N_o_k_N_ k_N_o_k_ N_k_N_o_ k_N_ktpk N_o_k_N_ k_N_o_k_ N_k_N_o_ k_N_ktpk"
    . su "N_o_k_N_ k_N_o_k_ N_k_N_o_ k_N_ktpk N_o_k_N_ o_k_k___ t___k___ n___o___"
    . "NokN okk_ t_k_ n_o_ NokN okk_ t_k_ n_o_"
    -- arudhi
    , "k___ D___ k___ n___ o___ ktkn od_k __D_"
    . "_k__ n__o __kt knod _k_t _k_n _o_k tkno"
    , su "k_______ D_______ k_______ n_______ o_______ ktktpkpk o_d___k_ ____D___"
    . su "__k_____ n_____o_ ____ktkt pkpko_d_ __k___t_ __k___n_ __o___kt ktpkpko_"
    , su "k_______ D_______ k_______ n_______ o_______ oktpupkt o_d___k_ ____D___"
    . su "__k_____ n_____o_ ____oktp upkto_d_ __k___t_ __k___n_ __o___ok tpupkto_"
    , su "N___p_k_ n_o_o_k_ t_k_o_o_ n_o_o_k_ ktook___ D___ktoo k___D___ ktook___"
    , "N__p uook D__p uook D__p uook D__p uook"
    . "Noop uook Noop uook Noop uook" . nadai 5 "NokNk Tknpk"
    , "N__k dook" . nadai 5 "NokNk Tknpk" . "N__k dook" . nadai 5 "NokNk Tknpk"
    ]

featuringFives_kanda :: Korvai
featuringFives_kanda = korvaiS adi $ map (nadai 5)
    [ "NokNk Tknpk npknk Tknpk NokNk Tknpk npknk Tknpk"
    . "N_N_k d_n_k d_n_k d_n_k N_N_k d_n_k d_n_k d_n_k"
    . "N_N_k d_n_k G_G_k d_n_k N_N_k d_n_k G_G_k d_n_k"
    . "N_N_k G_G_k N_N_k G_G_k N_N_k G_G_k N_N_k ktkno"
    . "N_N_k G_G_k N_N_k ktkno N_N_k G_G_k N_N_k ktkno"
    , r3 "ktkno d____ ktkno ktkno d____ ktkno ktkno ktkno"
    , "N_N_k G_G_k N_N_k G_G_k N_N_k G_G_k N_N_k G_G_k"
    -- korvai
    , r3 "p_k_N __ktk nok_N __ktk noN__ ktkno ktkno ktkno"
    ]

featuringFives_farans :: Korvai
featuringFives_farans = korvaiS adi $
    [ su "N___p_k_ n_o_o_k_ t_k_o_o_ n_o_o_k_ __okk___ d_____ok k___d___ __okk___"
    --
    , "ooou _oou ooou _oou pppu _ppu pppu _ppu"
    , "ooou _ppu ooou _ppu ooou _ppu ooou _ppu"
    --
    , su "N_pkd_pk nookd_pk N_pkd_pk nookd_pk N_pkd_pk kookd_pk N_pkd_pk kookd_pk"
    , su "N_pkkook N_pkkook N_pkkook N_pkkook kookkook N___kook kookN___ kookkook"

    , su "N___ktpk n_o_ktpk N___ktpk n_o_ktpk N___ktpk n_o_ktpk N___ktpk tpupktpk"
    , su "N___ktpk n_o_ktpk N___ktpk tpupktpk N___ktpk n_o_ktpk N___ktpk tpupktpk"
    , su "N___ktpk tpupktpk N___ktpk tpupktpk tpupktpk N___tpup ktpkN___ tpupktpk"
    --
    , su "N_____kt ktpkd___ __ktkttk N___k___ N_____kt ktpkd___ __ktkttk N___k___"
    , su "N_____kt ktpkd___ __ktkttk tpupktpk N_____kt ktpkd___ __ktkttk tpupktpk"

    -- farans
    , su "t_o_ktpk tpupktpk pu_oktpk tpupktpk pko_N_pk tpupktpk N_pknook tpupktpk"
    , su "N_pkt_o_ ktpkN_ pkt_o_ktpk tpupktpk N_pktpup ktpkN_pk tpupktpk tpupktpk"
    , su "pu_ko_kt ktpkpu_k o_ktktpk tpupktpk pu_ko_pu _ko_pu_k o_ktktpk tpupktpk"
    , su "npktpknp ktpknpkt pknpktpk tpupktpk n_ktpkn_ ktpkn_kt pkn_ktpk tpupktpk"
    , su "tpktpktp ktpktpkt pktpktpk tpupktpk N_ktpkN_ ktpkN_kt pkN_ktpk tpupktpk"
    , su "N_N_ktpk tpupktpk N_k_N___ N_N_ktpk tpupktpk N_k_N___ N_N_ktpk tpupktpk"
    ]

featuringFives_mohra :: Korvai
featuringFives_mohra = korvaiS adi $
    [ su "p___k___ N_pktpup ktpkk___ N_pktpup ktpkN_pk tpupktpk pu_ko_k_ o_k_o___"
    , su "p___k___ N_pktpup ktpkk___ N_pktpup ktpkN_pk tpupktpk pu_ko_k_ o_k_o___"
    , su "p___k___ N_pktpup ktpkk___ N_pktpup ktpkN_pk tpupktpk pu_ko___ k___N_pk"
    , su "tpupktpk pu_ko___ k___N_pk tpupktpk pu_ko_k_ N___pu_k o_k_N___ pu_ko_k_"
    ]

featuringFives_mohra_korvai :: Korvai
featuringFives_mohra_korvai = korvaiS adi $
    [ "p_k_ kook N__k _t_k _n_o k_ko okN_ _k_t"
    , "_k_n _oko okN_ _k_t _k_n _oD_ __k_ t_k_"
    , "n_oD ___k _t_k _n_o p_k_ kook N__k _t_k"
    , "_n_o k_ko okN_ _k_t _k_n _oko okN_ _k_t"
    , "_k_n _oD_ __k_ t_k_ n_oD ___k _t_k _n_o"

    , nadai 5 $ "p_k_k ookN_ _k_t_ k_n_o k_koo kN__k _t_k_ n_oko"
        . "okN__ k_t_k _n_oD ___k_ t_k_n _oD__ _k_t_ k_n_o"
    -- ending
    , su "N___p_k_ n_o_o_k_ t_k_o_o_ n_o_o_k_ tpupktpk N___tpup ktpkN___ tpupktpk"
    , su "tpupktpk N_pktpup ktpkN_pk tpupktpk tpupktpk N_ktpkN_ ktpkN_kt pkN_o_k_"
    ]
