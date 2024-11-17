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

kanda_korvai4 :: Korvai
kanda_korvai4 = korvai Tala.kanda_chapu $
    [ x3 $ s $ reduceTo3 4 2 "p_k_t_k_kto_" . r2 (r2 "D___" . r3 "kto_")
    , s $ "N___ ____ n_o_ D___ n_o_"
    . r3 (g "k_kto_") . "N___" . r3 (g "k_k_kto_") . "N___"
    . r3 (g ("ok".nakatiku))
    ]

tri122 :: Sequence -> Sequence -> Sequence
tri122 a b = a.b.b

reduceTo3 :: (HasCallStack, Pretty sollu) => S.FMatra -> S.FMatra
    -> SequenceT sollu -> SequenceT sollu
reduceTo3 to by seq = mconcatMap r3 (reduceToL to by seq)

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
    . "A___ ktkn oktk tpup ktpk"
    , "D___ __k_ k_o_ D___ n___" --
    . "D___ D___ k_o_ D___ ktpk"
    . "okk_ o_o_ k_o_ D___ n___"
    . "A___ ktkn oktk tpup ktpk"
    , "o_k_ o_o_ k_o_ D___ n___" --
    . "A___ ktkn oktk tpup ktpk"
    , "o_k_ o_o_ k_o_ D___ ktkn" --
    . "oktk tpup ktpk N___ ____"
    . "ktkn oktk tpup ktpk N_pk"
    . "tpup ktpk N_pk npk_ ____"
    , "N___ o___ n_o_ D___ n_o_" --
    . "D___ o_d_ D___ D___ n___"
    , "A___ ktkn oktk tpup ktpk" --
    . "k_kt _kno k___ ____ D___"
    , "ktpk k_kt _kno k_kt _kno" --
    . "k___ ____ D___ ____ A___"
    . "ktkn oktk tpup ktpk k__k"
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
    [ "D_n _kd _n_ kd_ n_k" --
    . "D_n _kd _n_ kd_ n_k"
    . r3 p5
    , "u__ D_k N_k D_k Nok" --
    . "d__ n_k d_k n_k Nok"
    . "D__ D_k N_k D_k ook"
    . "d__ n_k d_k n_k Nok"
    . "d__ n_k d_k n_k Nok"
    . "N__ k__ ktk npu ook"
    , "D__ D_k N_k D_k Nok" --
    . "N__ k__ ktk npu ook"
    , "d__ d_k n_k d_k Nok" --
    . "N__ k__ ktk npu ook"
    ]

kanda3_farans :: Korvai
kanda3_farans = korvaiS Tala.kanda_chapu $
    [ "u___ __N_ __N_ __N_ __k_" --
    . "d___ __n_ __n_ __n_ __k_"
    . "u___ __kt ktpk" . nakatiku
    . "d___ __np ktpk" . nakatiku
    , "d___ __kt ktpk d___ __kt" --
    . "ktpk d___ __kt ktpk N_k_"
    . "d___ __kt ktpk d___ __kt"
    . "ktpk d___ N_pk" . nakatiku
    -- Farans
    , "N___ t_o_ ktpk" . nakatiku --
    . "N_N_ pu_o ktpk" . nakatiku
    . "N_N_ pko_ N_pk" . nakatiku
    . "N_N_ N_pk nook" . nakatiku
    , "N_pk t_o_ N_pk t_o_ ktpk" --
    . "N_pk". r2 nakatiku
    , "pu_k o_k_ pu_k o_kt ktpk" --
    . "N_N_ pu_k o_pu _ko_ pu_k"
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
    -- But he ends: a1.b2 . a1.b3

kanda4_mohra_raw :: Korvai
kanda4_mohra_raw = korvaiS Tala.kanda_chapu $
    [ "k_t_ k_n_ o_k_ __N_ __kt" . "ktpk t_o_ ktpk" . nakatiku
    . "o_k_ t_k_ n_o_ k_N_ __kt" . "ktpk N_k_ o__k N_k_ o___"
    , "k_t_ k_n_ o_k_ __N_ __kt" . "ktpk t_o_ ktpk" . nakatiku
    . "o_k_ t_k_ n_o_ k_N_ __kt" . "ktpk N_k_ o__k N_k_ o___"
    , "k_t_ k_n_ o_k_ __N_ __kt" . "ktpk t_o_ ktpk" . nakatiku
    . "o_k_ t_k_ n_o_ k_N_ __kt" . "ktpk o_k_ o___ k_t_ k_n_"
    . "o_k_ __N_ __kt ktpk o_k_" . "o___ k_t_ k_n_ o_k_ __N_"
    . "__kt ktpk pu_k o_k_ N___" . "pu_k o_k_ N___ pu_k o_k_"
    ]

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
    [ "k___ D___ ____ N___ N___ d___ n_k_" --
    . "k_o_ D___ ____ N___ N___ u___ ____"
    , "k_o_ D___ ____ N___ N___ u___ ____" --
    . "k_k_ o_D_ ____ N___ N___ D___ n_k_"
    . "k_o_ D___ ____ N___ N___ u___ ____"
    . "okko o___ ____ N___ N___ D___ n_k_"
    . "k_o_ o_k_ o_k_ o_o_ k_o_ k_o_ o_k_"
    . "k___ __D_ ____ N___ N___ D___ n_k_"
    . "k_o_ o_k_ o_k_ o_o_ k_o_ k_o_ o_k_"
    . "k_k_ o_D_ ____ N_k_ N_k_ D___ n_k_"
    . "k_o_ o_k_ o_k_ o_o_ k_o_ k_o_ o_k_"
    . "kook kook o_ko okko oko_ kook kook"
    . "kook o_ko okko oko_ kook kook kook"
    , "u___ ____ u___ __u_ ____ u___ __u_" --
    . "____ u___ __u_ ____ u___ n_pk" . su nakatiku
    , "D___ n_n_ d___ ____ d___ n_n_ d___" --
    . "u___ n_n_ d___ n___ d___ n_n_ d___"
    . "N___ N_N_ D___ ____ D___ N_N_ D___"
    . "u___ n_n_ d___ n_d_ d_D_ n_D_ D_d_"
    . "n_d_ d_n_ d_d_ n_d_ d_D_ n_D_ D_d_"
    , "d___ n_kt pkn_ d_k_ n_kt pkn_ d_k_" --
    . "D___ n_kt pkn_ d_k_ n_kt pkn_ d_k_"
    . "t_k_ n_kt pkn_ d_k_ n_kt pkn_ d_k_"
    . "p___ k___ k_o_ o_k_ N___ p_k_ kook"
    . "D___ N_kt pkN_ D_k_ N_kt pk_N D_k_"
    . "p___ k___ k_o_ o_k_ N___ p_k_ kook"
    ]

misra_korvai1 :: Korvai
misra_korvai1 = korvaiS Tala.misra_chapu
    [ "p___ k___ k_o_ o_k_ p_k_ kook k_ko" --
    . "okko okk_ __k_ o_o_ k_k_ kook kook"
    . "k_o_ o_k_ kook k___ __D_ ____ k___"
    . "__n_ ____ o___ __k_ __D_ __k_ __n_"
    . "__o_ __k_ __D_ __k_ __n_ __o_ __k_"
    . "t_k_ n_o_ k_t_ k_n_ o_k_ t_k_ n_o_"

    , "p___ k___ k_o_ o_k_ p_k_ kook k_ko" --
    . "okko okk_ __k_ o_o_ k_k_ kook kook"
    . "k_o_ o_k_ kook k___ __D_ ____ k___"
    . "__n_ ____ o___ __k_ __D_ __k_ __n_"
    . "__o_ __k_ __D_ __k_ __n_ __o_ __kt"
    . "ktpk pko_ ktkt pkpk o_kt ktpk pko_"
    , "p___ k___ k_o_ o_k_ p_k_ kook k_ko" --
    . "okko okk_ __k_ o_o_ k_k_ kook kook"
    . "k_o_ o_k_ kook k___ __D_ ____ k___"
    . "__n_ ____ o___ __k_ __D_ __k_ __n_"
    . "__o_ __k_ __D_ __k_ __n_ __o_ __ok"
    . "tpup kto_ oktp upkt o_ok tpup kto_"
    ]

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
    . "d__ ktk d__ k_p ktk" . su "n_pktp upktpk"
    . "d__ ktk d__ k__ ktk npu ook"
    . "ook ook d__ k__ ktk npu ook"
    ]

misra_korvai3 :: Korvai
misra_korvai3 = korvaiS Tala.misra_chapu $ map (nadai 3) $
    [ "k__ ktk npu ook ook N__ k__" --
    . "ktk npu ook ook N__ k__ ktk"
    . "npu ook ook N__ kt_ kno N__"
    . "___ kt_ kno N__ ___ kt_ kno"
    , "k__ ktk npu ook ook N__ on_" --
    . "ktk npu ook ook N__ on_ ktk"
    . "npu ook ook N__ kt_". su "k_kt"."o u__"
    . "___ kt_" . su "k_kto_" . "u__ ___ kt_" . su "k_kt".o
    , "k__ ktk npu ook ook ook on_" --
    . "ktk npu ook ook ook on_ ktk"
    . "npu ook ook ook kt_" . su "k_kt"."o u__"
    . "___" . r3 ("kt_" . su "k_kt".o)
    ]

misra_farans :: Korvai
misra_farans = korvaiS Tala.misra_chapu $
    [ "u___ __k_ p_k_ t_k_ N_pk tpup ktpk" --
    . "N_kd _kN_ kd_k N_kd _ktk tkoo" . su "ktok"."Tk"
    . "N_kd _kn_ kd_k n_kd _ktk tkoo" . su "ktok"."Tk"
    . "onkd _kN_ kd_k N_kd _ktk tkoo" . su "ktok"."Tk"
    . "tkoo" . su "ktok"."Tk tkoo" . su "ktok"."Tk tkoo" . su "ktok"."Tk tkoo"
    . su "ktok" ."Tk N___ tkoo" . su "ktok"."Tk N___ tkoo" . su "ktok"."Tk"
    , "N___ __kt ktpk d___ __kt ktpk d___" --
    . "__kt ktpk d___ __kt ktpk N___ k___"
    , "N___ __kt ktpk d___ __kt ktpk d___" --
    . "__kt ktpk d___ __kt ktpk tpup ktpk"
    -- Farans
    , "t_o_ ktpk tpup ktpk N_pk tpup ktpk" --
    . "pu_o ktpk tpup ktpk N_pk tpup ktpk"
    . "N_pk t_o_ ktpk N_pk t_o_ tpup ktpk"
    . "pu_k o_pu _ko_ pu_k o_ok tpup ktpk"
    ]

misra_mohra :: Korvai
misra_mohra = korvaiS Tala.misra_chapu
    [ "upkt o_k_ __N_ __np ktpk tpup ktpk" --
    . "tpup kt_o k_N_ __np ktpk tpup ktpk"
    . "upkt o_k_ __N_ __np ktpk tpup ktpk"
    . "pu_k o_k_ N___ __pu _ko_ k_N_ ____"
    , "upkt o_k_ __N_ __np ktpk tpup ktpk" --
    . "tpup kt_o k_N_ __np ktpk tpup ktpk"
    . "upkt o_k_ __N_ __np ktpk tpup ktpk"
    . "pu_k o_k_ N___ __pu _ko_ k_N_ ____"
    , "upkt o_k_ __N_ __np ktpk tpup ktpk" --
    . "tpup kt_o k_N_ __np ktpk tpup ktpk"
    . "upkt o_k_ __N_ __np ktpk tpup ktpk"
    . "pu_k o_k_ N___ __up kto_ k___ N___"
    . "npkt pktp upkt pkpu _ko_ k_N_ ____"
    . "tpup kto_ k_N_ __np ktpk tpup ktpk"
    , "pu_k o_k_ pu_k o_k_ N_k_ pu_k o_k_" --
    . "pu_k o_k_ N_pk pu_k o_k_ pu_k o_k_"
    ]

misra_mohra_korvai :: Korvai
misra_mohra_korvai = korvaiS Tala.misra_chapu $
    [ "k___ D___ k_D_ __k_ o_D_ __k_ D___" --
    . "o_k_ D___ k_D_ __k_ ____ __D_ ____"
    . "__k_ ____ __n_ ____ __o_ ____ __k_"
    . "__D_ __k_ __n_ __o_ __k_ t_k_ n_o_"
    , "k___ D___ k_D_ __k_ o_D_ __k_ D___" --
    . "o_k_ D___ k_D_ __k_ ____ __D_ ____"
    . "__k_ ____ __n_ ____ __o_ ____ __k_"
    . "__D_ __k_ __n_ __o_ __k_ t_k_ n_o_"
    , "k___ D___ k_D_ __k_ o_D_ __k_ D___" --
    . "o_k_ D___ k_D_ __k_ ____ __D_ ____"
    . "__k_ ____ __n_ ____ __o_ ____ __k_"
    . "t_k_ n_o_ k_t_ k_n_ o_k_ t_k_ n_o_"
    , "D___ ____ ____ ____ k_k_ k_o_ o_k_" --
    . "D___ N_D_ __N_ D___ N_D_ __N_ D___"
    ] ++ map (nadai 3)
    [ "npu ook D__ npu ook npu ook" --
    . "D__ npu ook npu ook ook ookD"
    ]


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
    [ "____ ____ ____ ". su "upkto_up kto_upkt o_upkto_"
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
rupaka_korvai1 = korvaiS Tala.rupaka_tala $
    [ su "k_t_k_kt _knoD___ __k_k_kt _knoD___ __k_kt_k noD_____" --
    . su "kt_knoD_ ____kt_k nokt_kno D_____kt _knokt_k nokt_kno"
    , su "P_X_P_kt _knoD_D_ __P_P_kt _knoD_D_ __P_kt_k noD_D___" --
    . su "kt_knoD_ D___kt_k nokt_kno D_D___kt _knokt_k nokt_kno"
    , su "P_X_P_kt _knoD_D_ D_P_P_kt _knoD_D_ D_P_kt_k noD_D_D_" --
    . su "kt_knoD_ D_D_kt_k nokt_kno D_D_D_kt oktoktok toktokto"
    ]

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
    , "N_kD _kn_ kD_k" . nadai 6 "N_kD_k N_kD_k N_kD_k" --
    , "N_kD _kn_ kD_k" . nadai 6 "N_kD_k N_kD_k N_kD_k" --
    ] ++ map (nadai 6)
    [ "N_kD_k n_kd_k n_kd_k N_kD_k n_kd_k n_kd_k" --
    . "ookD_k n_kd_k n_kd_k ppkd_k n_kd_k n_kd_k"
    . "ookD_k n_kd_k ppkd_k n_kd_k ookD_k n_kd_k"
    . "ookD_k ppkd_k ookD_k ppkd_k ookppk ookppk"
    . r2 (su "N_ktoko_t_k_ n_ktoko_t_k_ N_ktoko_t_k_ n_ktoko_t_k_"
                                      . su "N_ktoko_t_k_ n_ktoko_t_k_")
    . su "o_t_k_N_ktok p_t_k_n_ktok o_t_k_N_ktok p_t_k_n_ktok"
                              . su "o_t_k_N_ktok p_t_k_n_ktok"
    ]

rupaka_korvai3 :: Korvai
rupaka_korvai3 = korvaiS Tala.rupaka_tala $ map (nadai 6) $
    [ "k__ktk npuook ooko__ k__ktk npuook ooko__" --
    . "k__ktk npuook ooko__ ktknok tknokt knok__"
    . "ktknpu ookook o__k__ ktknpu ookook o__k__"
    . "ktknpu ookook o__kt_ knokt_ knokt_ knok__"
    . "ktknpu ookook o__k__ ktknpu ookook o__k__"
    . "ktknpu ookook o__k_t _knok_ t_knok _t_kno"
    ]

rupaka_farans :: Korvai
rupaka_farans = korvaiS Tala.rupaka_tala $ map su
    [ "d_____n_ ktpkd___ __n_ktpk d_____n_ ktpkD_D_ __N_ktpk" --
    . "D_____np ktpkd___ __npktpk d_____np ktpkD_D_ __N_ktpk"
    . "Tkoktknp ktpkTkok tknpktpk Tkoktknp ktpkN_pk".nakatiku
    -- Farans
    , "t_o_ktpk" . nakatiku.nakatiku. "pu_oktpk" . nakatiku.nakatiku --
    . "pko_N_pk" . nakatiku.nakatiku. "N_pknook" . nakatiku.nakatiku
    . "N_pkt_o_ ktpkN_pk t_o_ktpk N_pk".nakatiku."N_pk" . nakatiku
    . "pu_ko_kt ktpkpu_k o_ktktpk pu_ko_pu _ko_pu_k o_pu_ko_"
    ]

rupaka_mohra :: Korvai
rupaka_mohra = mohra $ korvaiS Tala.rupaka_tala $ map su
    [ "N_pktpup ktpkoktp u___N_pk tpupktpk o_k_N__k o_k_N___" --
    . "N_pktpup ktpkoktp u___N_pk tpupktpk o_k_N__k o_k_N___"
    . "N_pktpup ktpkoktp u___N_pk tpupktpk pu_ko___ N_pktpup"
    . "ktpkpu_k o___N_pk tpupktpk pu_ko_k_ pu_ko_k_ pu_ko_k_"
    ]

rupaka_mohra_korvai :: Korvai
rupaka_mohra_korvai = korvaiS Tala.rupaka_tala $
    [ r2 $ "k_t_" . su "koD___N_ __k___ko D___N___ koD___N_" . "_PPP" --
        . "_ktk noPP P_kt knoP PP_k tkno"
    , "k_t_" . su "koD___N_ __k___ko D___N___ koD___N_ __oktku_"
        . "_ktk" . su "n_o_oktp u___k_t_ k_n_o_ok tpu___k_ t_k_n_o_"

    , "NkNN dkNk NNdk Nd_N d_Nd _Nd_" --
    . su "tpupktpk N___tpup ktpkN___ tpupktpk tpupktpk N_pktpup"
    . su "ktpkN_pk tpupktpk pupktpk N__ktpkN_ ktpkN_kt pkN_o_k_" . od
    ]


{-
rupaka_tani_raw :: Korvai
rupaka_tani_raw = korvaiS Tala.rupaka_tala $
    [ "____ ____ ____ +upkto_up +kto_upkt +o_upkto_" --
    . "A_ok koNN _oNk D_D_ noD_ nook"
    . "A_ok kook okk_ D_D_ noD_ nook"
    . "u_oo koD_ +ktokotok o_o_ koD_ +ktokotok"
    . "N_o_ koD_ +ktokotok +N_ktok +otokN_ot +okN_otok"
    , "A_kk koD_ D_N_ d_d_ nnd_ d_n_" --
    . "__u_ __u_ __u_ __u_ __u_ _ook"
    . "A_kk koD_ D_N_ D___ __kt +kkt_kno"
    . "D___ __kt +kkt_kno uu__ __kt +kkt_kno"
    . "u___ __kt +kkt_kno +ktkkt +_knokt +kkt_kno"
    -- Korvai
    , "+ktkkt +_knoD_ +_kkkt +_knoD_ +_kkt_k +noD__" --
    . "+kt_knoD +__kt_k +nokt_kno +D__kt +_knokt_k +nokt_kno"
    , "+Pt̅Pkt +_knoDD +_PPkt +_knoDD +_Pkt_k +noDD_" --
    . "+kt_knoD +D_kt_k +nokt_kno +DD_kt +_knokt_k +nokt_kno"
    , "+Pt̅Pkt +_knoDD +DPPkt +_knoDD +DPkt_k +noDDD" --
    . "+kt_knoD +DDkt_k +nokt_kno +DDDkt +oktoktok +toktokto"
    , "k___ ____ +nkook_ +N_nkoo k_Nk _Nk_" --
    , "N_D_ nkn_ d___ __d_ nkn_ d___" --
    . "N_D_ nkn_ d___ __d̅_ nkn_ d_nk"
    . "NkD_ nkn_ d___ __d̅_ nkn_ d_nk"
    . "+on_kDk NkN_ D___ __D_ NkNo kNok"
    , "N_nn dknk nndk __nn dknk nndk" --
    . "N_NN DkNk NNDk __NN DkNk NNDk"
    , "+nktpkn dknk nndk __nn dknk nndk" --
    . "+npktpkn dknk nndk +npnd_pnd +_pnd_pnd +_pnd_oN_"
    . "u_u_ dknk nndk +npnd_pnd +_pnd_pnd +_pnd_oN_"
    . "u_u_ dknk +npnd_oN_ +u_npnd +_oN_u__k doDk"
    , "N_kD _kn_ kd_k n_kd _kn_ kd_k" --
    . "N_kD _kn_ kd_k NokN okNo kNok"
    . "N_kD _kn_ kd_k +o_ktpko_ +ktpko_kt +pko_ktpk"
    , "N_kD _kn_ kD_k +N_kD_k +N_kD_k +N_kD_k" --
    , "N_kD _kn_ kD_k +N_kD_k +N_kD_k +N_kD_k" --
    -- nadai 3
    , "+N_kD_k +n_kd_k +n_kd_k +N_kD_k +n_kd_k +n_kd_k" --
    . "+ookD_k +n_kd_k +n_kd_k +ppkd_k +n_kd_k +n_kd_k"
    . "+ookD_k +n_kd_k +ppkd_k +n_kd_k +ookD_k +n_kd_k"
    . "+ookD_k +ppkd_k +ookD_k +ppkd_k +ookppk +ookppk"
    . "+N_ktokotk +n_ktokotk +N_ktokotk +n_ktokotk +N_ktokotk +n_ktokotk"
    . "+N_ktokotk +n_ktokotk +N_ktokotk +n_ktokotk +N_ktokotk +n_ktokotk"
    . "+otkN_ktok +ptkn_ktok +otkN_ktok +ptkn_ktok +otkN_ktok +ptkn_ktok"
    -- Korvai
    , "+k__ktk +npuook +ooko__ +k__ktk +npuook +ooko__" --
    . "+k__ktk +npuook +ooko__ +ktknok +tknokt +knok__"
    . "+ktknpu +ookook +o__k__ +ktknpu +ookook +o__k__"
    . "+ktknpu +ookook +o__kt_ +knokt_ +knokt_ +knok__"
    . "+ktknpu +ookook +o__k__ +ktknpu +ookook +o__k__"
    . "+ktknpu +ookook +o__k_t +_knok_ +t_knok +_t_kno"
    -- nadai 4
    , "d__n +ktpkd_ +_nktpk d__n +ktpkDD +_Nktpk" --
    . "+D__np +ktpkd_ +_npktpk +d__np +ktpkDD +_Nktpk"
    . "+Tkoktknp +ktpkTkok +tknpktpk +Tkoktknp +ktpkN_pk +tpupktpk"
    -- Farans
    , "+t_o_ktpk +tpupktpk +tpupktpk +pu_oktpk +tpupktpk +tpupktpk" --
    . "+pko_N_pk +tpupktpk +tpupktpk +N_pknook +tpupktpk +tpupktpk"
    . "+N_pkt_o_ +ktpkN_pk +t_o_ktpk +N_pktpup +ktpkN_pk +tpupktpk"
    . "+pu_ko_kt +ktpkpu_k +o_ktktpk +pu_ko_pu +_ko_pu_k +o_pu_ko_"
    -- Mora
    , "+N_pktpup +ktpkoktp +u_N_pk +tpupktpk +okN__k okN_" --
    . "+N_pktpup +ktpkoktp +u_N_pk +tpupktpk +okN__k okN_"
    . "+N_pktpup +ktpkoktp +u_N_pk +tpupktpk +pu_ko_ +N_pktpup"
    . "+ktpkpu_k +o_N_pk +tpupktpk +pu_kok +pu_kok +pu_kok"
    -- Korvai
    , "k_t_ +koD_N +_k_ko D_N_ +koD_N _PPP" --
    . "_ktk noPP P_kt knoP PP_k tkno"
    , "k_t_ +koD_N +_k_ko D_N_ +koD_N _PPP" --
    . "_ktk noPP P_kt knoP PP_k tkno"
    , "k_t_ +koD_N +_k_ko D_N_ +koD_N +_oktpu" --
    . "_ktk +nooktp u_kt +knook +tpu_k tkno"
    , "NkNN dkNk NNdk Nd_N d_Nd _Nd_" --
    . "+tpupktpk +N_tpup +ktpkN_ +tpupktpk +tpupktpk +Npktpup"
    . "+ktpkNpk +tpupktpk +tpupktpk +N_ktpkN_ +ktpkN_kt +pkNok"
    . "N___ ____ ____ ____ ____ ____"
    ]

misra_tani_raw :: Korvai
misra_tani_raw = korvaiS Tala.misra_chapu $
    [ "k___ D___ ____ N___ N___ d___ n_k_" --
    . "k_o_ D___ ____ N___ N___ u___ ____"
    , "k_o_ D___ ____ N___ N___ u___ ____" --
    . "k_k_ o_D_ ____ N___ N___ D___ n_k_"
    . "k_o_ D___ ____ N___ N___ u___ ____"
    . "okko o___ ____ N___ N___ D___ n_k_"
    . "k_o_ o_k_ o_k_ o_o_ k_o_ k_o_ o_k_"
    . "k___ __D_ ____ N___ N___ D___ n_k_"
    . "k_o_ o_k_ o_k_ o_o_ k_o_ k_o_ o_k_"
    . "k_k_ o_D_ ____ N_k_ N_k_ D___ n_k_"
    . "k_o_ o_k_ o_k_ o_o_ k_o_ k_o_ o_k_"
    . "kook kook o_ko okko oko_ kook kook"
    . "kook o_ko okko oko_ kook kook kook"
    , "u___ ____ u___ __u_ ____ u___ __u_" --
    . "____ u___ __u_ ____ u___ n_pk" . su nakatiku
    , "D___ n_n_ d___ ____ d___ n_n_ d___" --
    . "u___ n_n_ d___ n___ d___ n_n_ d___"
    . "N___ N_N_ D___ ____ D___ N_N_ D___"
    . "u___ n_n_ d___ n_d_ d_D_ n_D_ D_d_"
    . "n_d_ d_n_ d_d_ n_d_ d_D_ n_D_ D_d_"
    , "d___ n_kt pkn_ d_k_ n_kt pkn_ d_k_" --
    . "D___ n_kt pkn_ d_k_ n_kt pkn_ d_k_"
    . "t_k_ n_kt pkn_ d_k_ n_kt pkn_ d_k_"
    . "p___ k___ k_o_ o_k_ N___ p_k_ kook"
    . "D___ N_kt pkN_ D_k_ N_kt pk_N D_k_"
    . "p___ k___ k_o_ o_k_ N___ p_k_ kook"
    -- Korvai
    , "p___ k___ k_o_ o_k_ p_k_ kook k_ko" --
    . "okko okk_ __k_ o_o_ k_k_ kook kook"
    . "k_o_ o_k_ kook k___ __D_ ____ k___"
    . "__n_ ____ o___ __k_ __D_ __k_ __n_"
    . "__o_ __k_ __D_ __k_ __n_ __o_ __k_"
    . "t_k_ n_o_ k_t_ k_n_ o_k_ t_k_ n_o_"

    , "p___ k___ k_o_ o_k_ p_k_ kook k_ko" --
    . "okko okk_ __k_ o_o_ k_k_ kook kook"
    . "k_o_ o_k_ kook k___ __D_ ____ k___"
    . "__n_ ____ o___ __k_ __D_ __k_ __n_"
    . "__o_ __k_ __D_ __k_ __n_ __o_ __kt"
    . "ktpk pko_ ktkt pkpk o_kt ktpk pko_"
    , "p___ k___ k_o_ o_k_ p_k_ kook k_ko" --
    . "okko okk_ __k_ o_o_ k_k_ kook kook"
    . "k_o_ o_k_ kook k___ __D_ ____ k___"
    . "__n_ ____ o___ __k_ __D_ __k_ __n_"
    . "__o_ __k_ __D_ __k_ __n_ __o_ __ok"
    . "tpup kto_ oktp upkt o_ok tpup kto_"

    , "N___ __k_ k_o_ o_k_ k_o_ o_k_ ktoo" --
    . "k___ N___ ktoo k___ N_k_ __N_ k___"
    , "N_d_ __k_ n_k_ n_d_ __k_ n_o_ ktpk" --
    . "N_d_ __k_ n_k_ n_d_ __k_ noo_ ktpk"
    . "N_d_ __k_ n_k_ n_d_ __k_ tkoo kook"
    . "N_d_ __k_ n_k_ n_d_ __k_ okoo kook"
    . "N_d_ __k_ n_k_ n_d_" . nadai 3 "nkd nkd nkd"
    . "N_d_ __k_ n_k_ n_d_" . nadai 3 "nkd nkd nkd"
    ] ++ map (nadai 3)
    [ "Nkd nkd nkd nkd nkd nkd nkd" --
    . "Nkd nkd nkd".p&n."kd nkd ".p&n."d nkd"
    . "Nkd Nkd nkd ".p&n."d nkd ".p&n."d nkd"
    . "k_t _kn ok_ t_k nok _t_ kno"
    . "Nkd Nkd nkd ".p&n."d nkd ".p&n."d nkd"
    . "k_t _kn ok_ t_k nok _t_ kno"
    . "P_D _kn ok_ D_k nok _D_ kno"
    . "P_D _".su "ktkt" . "ok_ D_".su "kt kt"."ok _D_". su "ktkt".o
    , "d__ ktk d__ u__ ktk d__ ktk" --
    . "d__ ktk d__ d__ ktk d__ kpk"
    . "o__ ktk d__ u__ ook ook ook"
    . "d__ ktk d__ k_p ktk" . su "n_pktp upktpk"
    . "d__ ktk d__ k__ ktk npu ook"
    . "ook ook d__ k__ ktk npu ook"
    -- Korvai
    , "k__ ktk npu ook ook N__ k__" --
    . "ktk npu ook ook N__ k__ ktk"
    . "npu ook ook N__ kt_ kno N__"
    . "___ kt_ kno N__ ___ kt_ kno"
    , "k__ ktk npu ook ook N__ on_" --
    . "ktk npu ook ook N__ on_ ktk"
    . "npu ook ook N__ kt_". su"k_kt"."o u__"
    . "___ kt_" . su "k_kto" . "u__ ___ kt_" . su "k_kt".o
    , "k__ ktk npu ook ook ook on_" --
    . "ktk npu ook ook ook on_ ktk"
    . "npu ook ook ook kt_" . su "k_kt"."o u__"
    . "___" . r3 ("kt_" . su "k_kt".o)
    ] ++
    [ "u___ __k_ p_k_ t_k_ N_pk tpup ktpk" --
    . "N_kd _kN_ kd_k N_kd _ktk tkoo" . su "ktok"."Tk"
    . "N_kd _kn_ kd_k n_kd _ktk tkoo" . su "ktok"."Tk"
    . "onkd _kN_ kd_k N_kd _ktk tkoo" . su "ktok"."Tk"
    . "tkoo" . su "ktok"."Tk tkoo" . su "ktok"."Tk tkoo" . su "ktok"."Tk tkoo"
    . su "ktok" ."Tk N___ tkoo" . su "ktok"."Tk N___ tkoo" . su "ktok"."Tk"
    , "N___ __kt ktpk d___ __kt ktpk d___" --
    . "__kt ktpk d___ __kt ktpk N___ k___"
    , "N___ __kt ktpk d___ __kt ktpk d___" --
    . "__kt ktpk d___ __kt ktpk tpup ktpk"
    -- Farans
    , "t_o_ ktpk tpup ktpk N_pk tpup ktpk" --
    . "pu_o ktpk tpup ktpk N_pk tpup ktpk"
    . "N_pk t_o_ ktpk N_pk t_o_ tpup ktpk"
    . "pu_k o_pu _ko_ pu_k o_ok tpup ktpk"
    -- Mora
    , "upkt o_k_ __N_ __np ktpk tpup ktpk" --
    . "tpup kt_o k_N_ __np ktpk tpup ktpk"
    . "upkt o_k_ __N_ __np ktpk tpup ktpk"
    . "pu_k o_k_ N___ __pu _ko_ k_N_ ____"
    , "upkt o_k_ __N_ __np ktpk tpup ktpk" --
    . "tpup kt_o k_N_ __np ktpk tpup ktpk"
    . "upkt o_k_ __N_ __np ktpk tpup ktpk"
    . "pu_k o_k_ N___ __pu _ko_ k_N_ ____"
    , "upkt o_k_ __N_ __np ktpk tpup ktpk" --
    . "tpup kt_o k_N_ __np ktpk tpup ktpk"
    . "upkt o_k_ __N_ __np ktpk tpup ktpk"
    . "pu_k o_k_ N___ __up kto_ k___ N___"
    . "npkt pktp upkt pkpu _ko_ k_N_ ____"
    . "tpup kto_ k_N_ __np ktpk tpup ktpk"
    , "pu_k o_k_ pu_k o_k_ N_k_ pu_k o_k_" --
    . "pu_k o_k_ N_pk pu_k o_k_ pu_k o_k_"
    -- Korvai
    , "k___ D___ k_D_ __k_ o_D_ __k_ D___" --
    . "o_k_ D___ k_D_ __k_ ____ __D_ ____"
    . "__k_ ____ __n_ ____ __o_ ____ __k_"
    . "__D_ __k_ __n_ __o_ __k_ t_k_ n_o_"
    , "k___ D___ k_D_ __k_ o_D_ __k_ D___" --
    . "o_k_ D___ k_D_ __k_ ____ __D_ ____"
    . "__k_ ____ __n_ ____ __o_ ____ __k_"
    . "__D_ __k_ __n_ __o_ __k_ t_k_ n_o_"
    , "k___ D___ k_D_ __k_ o_D_ __k_ D___" --
    . "o_k_ D___ k_D_ __k_ ____ __D_ ____"
    . "__k_ ____ __n_ ____ __o_ ____ __k_"
    . "t_k_ n_o_ k_t_ k_n_ o_k_ t_k_ n_o_"
    , "D___ ____ ____ ____ k_k_ k_o_ o_k_" --
    . "D___ N_D_ __N_ D___ N_D_ __N_ D___"
    ] ++ map (nadai 3)
    [ "npu ook D__ npu ook npu ook" --
    . "D__ npu ook npu ook ook ookD"
    ]

kanda_tani_raw :: Korvai
kanda_tani_raw = korvaiS Tala.kanda_chapu $
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
    . "A___ ktkn oktk tpup ktpk"
    , "D___ __k_ k_o_ D___ n___" --
    . "D___ D___ k_o_ D___ ktpk"
    . "okk_ o_o_ k_o_ D___ n___"
    . "A___ ktkn oktk tpup ktpk"
    , "o_k_ o_o_ k_o_ D___ n___" --
    . "A___ ktkn oktk tpup ktpk"
    , "o_k_ o_o_ k_o_ D___ ktkn" --
    . "oktk tpup ktpk N___ ____"
    . "ktkn oktk tpup ktpk N_pk"
    . "tpup ktpk N_pk npk_ ____"
    , "N___ o___ n_o_ D___ n_o_" --
    . "D___ o_d_ D___ D___ n___"
    , "A___ ktkn oktk tpup ktpk" --
    . "k_kt _kno k___ ____ D___"
    , "ktpk k_kt _kno k_kt _kno" --
    . "k___ ____ D___ ____ A___"
    . "ktkn oktk tpup ktpk k__k"
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
    -- , "d̅___ n̅___ k_d_ __n_ __k_"
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
    ] ++ map (nadai 3)
    [ "D_n _kd _n_ kd_ n_k" --
    . "D_n _kd _n_ kd_ n_k"
    . "ktk nok tkn okt kno"
    , "u__ D_k N_k D_k Nok" --
    . "d__ n_k d_k n_k Nok"
    . "D__ D_k N_k D_k ook"
    . "d__ n_k d_k n_k Nok"
    . "d__ n_k d_k n_k Nok"
    . "N__ k__ ktk npu ook"
    , "D__ D_k N_k D_k Nok" --
    . "N__ k__ ktk npu ook"
    , "d__ d_k n_k d_k Nok" --
    . "N__ k__ ktk npu ook"
    ] ++ map (nadai 3)
    -- Korvai
    [ "k__ ktk npu ook ook" --
    . "N__ k__ ktk npu ook"
    . "ook N__ k__ ktk npu"
    . "ook ook N__ ktk nok"
    . "_t_ kno ktk nok _t_"
    . "kno ktk nok _t_ kno"
    , "k__ ktk npu ook oo_" --
    . "oo_ on_ ktk npu ook"
    . "oo_ oo_ on_ ktk npu"
    . "ook oo_ oo_ ktk nok"
    . "_D_ kno ktk nok _D_"
    . "kno ktk nok _D_ kno"
    , "k__ ktk npu ook ook" --
    . "ook on_ ktk npu ook"
    . "ook ook on_ ktk npu"
    . "ook ook ook ktk nok"
    . "_D_ kno ktk nok _D_"
    . "kno ktk nok _D_ kno"
    ] ++
    [ "u___ __N_ __N_ __N_ __k_" --
    . "d___ __n_ __n_ __n_ __k_"
    . "u___ __kt ktpk tpup ktpk"
    . "d___ __np ktpk tpup ktpk"
    , "d___ __kt ktpk d___ __kt" --
    . "ktpk d___ __kt ktpk N_k_"
    . "d___ __kt ktpk d___ __kt"
    . "ktpk d___ N_pk tpup ktpk"
    -- Farans
    , "N___ t_o_ ktpk" . nakatiku --
    . "N_N_ pu_o ktpk" . nakatiku
    . "N_N_ pko_ N_pk" . nakatiku
    . "N_N_ N_pk nook" . nakatiku
    , "N_pk t_o_ N_pk t_o_ ktpk" --
    . "N_pk". r2 nakatiku
    , "pu_k o_k_ pu_k o_kt ktpk" --
    . "N_N_ pu_k o_pu _ko_ pu_k"
    -- Mora
    , "k_t_ k_n_ o_k_ __N_ __kt" --
    . "ktpk t_o_ ktpk tpup ktpk"
    . "o_k_ t_k_ n_o_ k_N_ __kt"
    . "ktpk N_k_ o__k N_k_ o___"
    , "k_t_ k_n_ o_k_ __N_ __kt" --
    . "ktpk t_o_ ktpk tpup ktpk"
    . "o_k_ t_k_ n_o_ k_N_ __kt"
    . "ktpk N_k_ o__k N_k_ o___"
    , "k_t_ k_n_ o_k_ __N_ __kt" --
    . "ktpk t_o_ ktpk tpup ktpk"
    . "o_k_ t_k_ n_o_ k_N_ __kt"
    . "ktpk o_k_ o___ k_t_ k_n_"
    . "o_k_ __N_ __kt ktpk o_k_"
    . "o___ k_t_ k_n_ o_k_ __N_"
    . "__kt ktpk pu_k o_k_ N___"
    . "pu_k o_k_ N___ pu_k o_k_"
    -- Korvai
    , "p_k_ t_k_ kto_ p_k_ t_k_" --
    . "kto_ p_k_ t_k_ kto_ k_t_"
    . "k_kt o_k_ t_k_ kto_ k_t_"
    . "k_kt o_k_ k_kt o_k_ k_kt"
    . "o_k_ k_kt o_k_ kto_ k_kt"
    . "o_k_ kto_ kto_ kto_ kto_"
    . "N___ N___ kto_ kto_ kto_"
    . "N___ N___ kto_ kto_ kto_"
    , "p_k_ t_k_ kto_ p_k_ t_k_" --
    . "kto_ p_k_ t_k_ kto_ k_t_"
    . "k_kt o_k_ t_k_ kto_ k_t_"
    . "k_kt o_k_ k_kt o_k_ k_kt"
    . "o_k_ k_kt o_k_ kto_ k_kt"
    . "o_k_ kto_ kto_ kto_ kto_"
    . "N___ N___ kto_ kto_ kto_"
    . "D___ D___ kto_ kto_ kto_"
    , "p_k_ t_k_ kto_ p_k_ t_k_" --
    . "kto_ p_k_ t_k_ kto_ k_t_"
    . "k_kt o_k_ t_k_ kto_ k_t_"
    . "k_kt o_k_ k_kt o_k_ k_kt"
    . "o_k_ k_kt o_k_ kto_ k_kt"
    . "o_k_ kto_ kto_ kto_ kto_"
    . "N_N_ N___ kto_ kto_ kto_"
    . "N_N_ N___ kto_ kto_ kto_"
    , "N___ ____ n_o_ D___ n_o_" --
    . "k_kt o_k_ kto_ k_kt o_N_"
    . "__k_ k_kt o_k_ k_kt o_k_"
    . "k_kt o_N_ __ok tpup kto_"
    . "oktp upkt o_ok tpup kto_"
    ]
-}
