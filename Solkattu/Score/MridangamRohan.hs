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
    , Comment "tisram"
    , K kanda2_tisram
    , K kanda_korvai3
    , Comment "farans"
    , K kanda3_farans
    , Comment "mohra"
    , K kanda4_mohra
    , K kanda_korvai4
    ]

kanda1 :: Korvai
kanda1 = korvaiS Tala.kanda_chapu $
    [ "A___ ____ k_o_ D___ k_o_" --
    . "D___ D___ k_o_ D___ ktpk"
    . "o_k_ o_o_ k_o_ D___ k_o_"
    . "D___ __N_ __N_ __u_ ____"
    , "D___ D___ k_o_ D___ ktpk" --
    . "o_k_ o_o_ k_o_ D___ k_o_"
    . "D___ __N_ __N_ __u_ ____"
    , "__n_ p_k_ k_o_ D___ k_o_" --
    . "D___ D___ k_o_ D___ ktpk"
    . "okk_ o_o_ k_o_ D___ k_o_"
    . "D___ __N_ __N_ __D_ ____"
    , "__n_ p_k_ k_o_ D___ k_o_" --
    . "D___ D___ k_o_ D___ ktpk"
    . "okk_ o_o_ k_o_ D___ k_o_"
    . "D___ __N_ __N_ __u_ ____"
    , "A___ n_k_ k_o_ D___ k_o_" --
    . "D___ D___ k_o_ D___ ktpk"
    . "o_k_ o_o_ k_o_ D___ n___"
    . "A___ ktkn oktk" . nakatiku
    , "D___ __k_ k_o_ D___ n___" --
    . "D___ D___ k_o_ D___ ktpk"
    . "okk_ o_o_ k_o_ D___ n___"
    . "A___ ktkn oktk" . nakatiku
    , "o_k_ o_o_ k_o_ D___ n___" --
    . "A___ ktkn oktk" . nakatiku
    , "o_k_ o_o_ k_o_ D___ ktkn" --
    . "oktk" . nakatiku . "N___ ____"
    . "ktkn oktk" . nakatiku . "N_pk"
    . nakatiku . "N_pk npk_ ____"
    , "N___ o___ n_o_ D___ n_o_" --
    . "D___ o_d_ D___ D___ n___"
    , "A___ ktkn oktk" . nakatiku --
    . "k_kt _kno k___ ____ D___"
    , "ktpk k_kt _kno k_kt _kno" --
    . "k___ ____ D___ ____ A___"
    . "ktkn oktk" . nakatiku . "k__k"
    . "_kno k__k _kno k__k _kno"
    , "N___ ____ n_o_ D___ n_o_" --
    . "N___ ____ kt_k noN_ __k_"
    . "kt_k noN_ __k_ t_kt _kno"
    , "k___ d_d_ n_k_ n_n_ d_d_" --
    . "n_k_ t_k_ n_k_ n_n_ d_d_"
    . "n___ d_d_ n_k_ d_n_ d___"
    . "n_kt kpkt kpkt kpkt kpk_"
    , "k___ d_d_ n_k_ d_n_ d___" --
    . "nkt_ kpkt kpkt kpkt kpk_"
    . "k___ d_d_ n_kt kpk_ k___"
    . "____ __k_ k___ ____ __k_"
    , "k___ d_d_ n_k_ n_n_ d_d_" --
    . "k___ d_k_ n_kp pk_n d_k_"
    . "n_k_ d_k_ n_k_ n_n_ d_k_"
    . "n_k_ d_k_ U_U_ __U_ ____"
    , "D___ N___ k_d_ __n_ __k_" --
    . "d___ n___ k_d_ __n_ __k_"
    . "D___ N___ k_d_ __n_ __k_"
    . p&d.__4.p&d.__4 . "k_d_ __n_ __k_"
    . "D___ N___ k_d_ __n_ __k_"
    . "D_N_ kd_n _kd_ n_kd _n_k"
    , "D___ N___ k_d_ __n_ __k_" --
    . "D_N_ kd_n _kd_ n_kd _n_k"
    , "D___ N___ k_d_ __n_ __k_" --
    . "D_N_ kd_n _kd_ n_kd _n_k"
    , "D_N_ kD_N _kd_ __n_ __k_" --
    . "D_N_ kD_N _kd_ __n_ __k_"
    . "D_N_ kD_N _kd_ __n_ __k_"
    . "D_N_ kD_N _kd_ __n_ __k_"
    , "D_N_ kD_N _kD_ N_kD _N_k" --
    . "d_n_ kd_n _kd_ n_kd _n_k"
    ]

kanda2_tisram :: Korvai
kanda2_tisram = korvaiS Tala.kanda_chapu $ map (nadai 3) $
    [ "D_n _kd _n_ kd_ n_k" . "D_n _kd _n_ kd_ n_k" . r3 p5
    , "u__ D_kN_k D_kNok" . "d__ n_kd_k n_kNok"
    . "D__ D_kN_k D_kook" . "d__ n_kd_k n_kNok"
    . "d__ n_kd_k n_kNok" . "N__ k__ktk npuook"
    , "D__ D_kN_k D_kNok" . "N__ k__ktk npuook"
    , "d__ d_kn_k d_kNok" . "N__ k__ktk npuook"
    ]

kanda_korvai3 :: Korvai
kanda_korvai3 = korvaiS Tala.kanda_chapu $ map (nadai 3)
    [ tri (g "k__ ktk npu ook ook N__") . r3 (p5.p7)
    , tri122
        (g "k__ ktk npu ook oo_ oo_")
        (g "on_ ktk npu ook oo_ oo_") . r3 (p5.p7)
    , tri122
        (g "k__ ktk npu ook ook ook")
        (g "on_ ktk npu ook ook ook") . r3 (p5.p7)
    ]

kanda3_farans :: Korvai
kanda3_farans = korvaiS Tala.kanda_chapu $
    [ sd "u__N_N_N_k" . sd "d__n_n_n_k"
    . "u___ __ktktpk" . nakatiku . "d___ __npktpk" . nakatiku
    , "d___ __ktktpkd___ __ktktpkd___ __ktktpkN_k_"
    . "d___ __ktktpkd___ __ktktpkd___ N_pk" . nakatiku
    , "N___ t_o_ktpk" . nakatiku . "N_N_ pu_o ktpk" . nakatiku
    . "N_N_ pko_N_pk" . nakatiku . "N_N_ N_pk nook" . nakatiku
    , "N_pk t_o_N_pk t_o_ktpk" . "N_pk". r2 nakatiku
    , "pu_ko_k_pu_ko_ktktpk" . "N_N_pu_ko_pu_ko_pu_k"
    ]

kanda4_mohra :: Korvai
kanda4_mohra = mohra $ korvaiS1 Tala.kanda_chapu $ Mohra.make id
    ( sd "ktknok_N_" . "ktktpk"
    , "t_o_ktpk".nakatiku
    , sd "oktknokN_" . "ktktpk"
    )
    ( "N_k_o__kN_k_o___"
    , sd "oko_"
    , tri_ "N___" "pu_ko_k_"
    )
    -- a123.b1 . a123.b1 . a123.b2 . a1.b2 . a3.b3
    -- But he ends:                  a1.b2 . a1.b3

kanda4_mohra_raw :: Korvai
kanda4_mohra_raw = korvaiS Tala.kanda_chapu $
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
kanda_korvai4 = korvai Tala.kanda_chapu $
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
    [ "u___ __k_ p_k_ t_k_ N_pk" . nakatiku
    . "N_kd _kN_ kd_k N_kd _ktk tkoo".ktok."Tk"
    . "N_kd _kn_ kd_k n_kd _ktk tkoo".ktok."Tk"
    . "onkd _kN_ kd_k N_kd _ktk tkoo".ktok."Tk"
    . "tkoo" . ktok."Tk tkoo" . ktok."Tk tkoo" . ktok."Tk tkoo"
    . ktok ."Tk N___ tkoo" . ktok."Tk N___ tkoo" . ktok."Tk"
    , "N___ __kt ktpk d___ __kt ktpk d___" --
    . "__kt ktpk d___ __kt ktpk N___ k___"
    , "N___ __kt ktpk d___ __kt ktpk d___" --
    . "__kt ktpk d___ __kt ktpk" . nakatiku
    -- Farans
    , "t_o_ktpk" . nakatiku . "N_pk" . nakatiku
    . "pu_oktpk" . nakatiku . "N_pk" . nakatiku
    . "N_pkt_o_ ktpkN_pkt_o_" . nakatiku
    . "pu_ko_pu _ko_pu_ko_ok" . nakatiku
    ]
    where ktok = su "ktok"

misra_mohra :: Korvai
misra_mohra = mohra $ korvaiS1 Tala.misra_chapu $ Mohra.make id
    ( "upkto_k_ __N_ __npktpk".nakatiku
    , "tpupkt_o k_N_ __npktpk".nakatiku
    , "upkto_k_ __N_ __npktpk".nakatiku
    )
    ( "pu_ko_k_ N_____pu _ko_ k_N_ ____"
    , "pu_ko_k_ N_____"
    , "pu_ko_k_ pu_ko_k_N_k_ pu_ko_k_"
    . "pu_ko_k_ N_pkpu_ko_k_ pu_ko_k_"
    )
    -- a123.b1 . a123.b1 . a123.b2 . a1.b2 . a3.b3
    -- But he has:                   a1.b2 . a2.b3

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
    . "N_o_ koD_" . su "ktokotok N____ktok otokN_ot okN_otok"
    , "A_kk koD_ D_N_ d_d_ nnd_ d_n_" --
    . "__u_ __u_ __u_ __u_ __u_ _ook"
    . "A_kk koD_ D_N_ D___ __kt" . su "k_kt_kno"
    . "D___ __kt" . su "k_kt_kno" . "uu__ __kt " . su "k_kt_kno"
    . "u___ __kt" . su "k_kt_kno k_t_k_kt _knok_t_ k_kt_kno"
    ]

rupaka_korvai1 :: Korvai
rupaka_korvai1 = korvaiS Tala.rupaka_tala
    [ reduce theme . tri123 "D__" (su p6)
    , reduce theme2 . tri123 "DD_" (su p6)
    , reduce theme3 . trin "DDD" (su p6) (su (p6.p6)) (su (r6 "kto"))
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
    , "N_nn dknk nndk __nn dknk nndk" --
    . "N_NN DkNk NNDk __NN DkNk NNDk"
    , su "n_ktpkn_" . "dknk nndk __nn dknk nndk" --
    . su "npktpkn_" . "dknk nndk" . su "npnd_pnd _pnd_pnd _pnd_oN_"
    . "u_u_ dknk nndk" . su "npnd_pnd _pnd_pnd _pnd_oN_"
    . "u_u_ dknk" . su "npnd_oN_ u___npnd _oN_u__k" . "doDk"
    , "N_kD _kn_ kd_k n_kd _kn_ kd_k" --
    . "N_kD _kn_ kd_k NokN okNo kNok"
    . "N_kD _kn_ kd_k" . su "o_ktpko_ ktpko_kt pko_ktpk"
    , r2 $ "N_kD _kn_ kD_k" . nadai 6 "N_kD_k N_kD_k N_kD_k" --
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
rupaka_mohra = mohra $ korvaiS1 Tala.rupaka_tala $ Mohra.make su
    ("N_pk".nakatiku, "oktpu___", "N_pk".nakatiku)
    ( "o_k_N__ko_k_N___", "pu_ko___", r3 "pu_ko_k_")
    -- a123.b1 . a123.b1 . a123.b2 . a1.b2 . a3.b3

rupaka_mohra_korvai :: Korvai
rupaka_mohra_korvai = korvai Tala.rupaka_tala $
    [ x2 $ s $ reduce3 2 ø theme . r3 ("PPP_".p5)
    , s $ reduce3 2 ø theme . r3 (su "oktku___".p5)
    , s $ r2 "NkNNdk" . r4 "Nd_"
    , s $ su $ tri_ "N___" nakatiku . tri_ "N_pk" nakatiku
        . nakatiku . r3 "N_ktpk" . "N_o_k_D"
    ]
    where theme = "k_t_".su "ko"."D_N_"
