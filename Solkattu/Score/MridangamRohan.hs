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

kanda_thani_raw :: Korvai
kanda_thani_raw = korvaiS Tala.kanda_chapu $
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
