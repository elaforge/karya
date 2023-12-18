-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Sarvalaghu.
module Solkattu.Score.MridangamSarva where
import           Prelude hiding ((.), repeat)

import qualified Solkattu.Tala as Tala

import           Global
import           Solkattu.Dsl.Mridangam


-- * kirkalam

-- TODO these don't need to be a full avartanam, only a binary factor of it

kir1 :: Korvai
kir1 = sarvalaghu $ sudhindra $ korvaiS adi $
    [ repeat 4 $ repeat 2 (n.l.d.d) & (o.__.o.o.__.o.o.__) -- takadimi takajonu
    ]

kir2 :: Korvai
kir2 = sarvalaghu $ sudhindra $ korvaiS adi $
    repeat 2 sarva : map pattern prefixes
    where
    pattern (prefix, end) =
        (repeat 2 $ prefix `replaceStart` sarva) `replaceEnd` end
    -- takatadin or nakanadin
    sarva = repeat 2 (n.k.n.d) & (o.__.o.o.__.__.o.o) . (on.k.n.d) . (n.k.n.d)
    prefixes = map (bimap su su)
        [ (takadinna, takadinna . repeat 3 (t.o.o.k))
        , (o.o.n.n . o.k, repeat 4 (o.o.n.n))
        , (o.o.k.t . p.k, repeat 4 (o.o.k.t))
        , (n.n.p.k, repeat 4 (n.n.p.k))
        , (p.u.__.k, repeat 4 (p.u.__.k))
        , (dinna_kitataka, repeat 2 dinna_kitataka . o.k.o.k . dinna_kitataka)
        , let nknk = o&j.y.o&j.y
            in (nknk, nknk.d.__.nknk.d.__.nknk)
        ]
    dinna_kitataka = o.n . su (k.t.o.k)

kir3 :: Korvai
kir3 = sarvalaghu $ sudhindra $ korvaiS1 adi $ repeat 2 $
    repeat 2 (n.d.__.n) & (o.o.__.o.__.o.__.o)
    . repeat 2 (n.d.__.n) & (o.__n 8)
    -- can end with faran: oonnpktk naka

kir4 :: Korvai
kir4 = sarvalaghu $ sudhindra $ korvaiS1 adi $
      on.__.on.__.on.od.__5.on.__.on.od.__2.o
    . on.k.on.k.on.od.__5.on.k.on.od.__2.o

kir5 :: Korvai
kir5 = sarvalaghu $ sudhindra $ korvaiS1 adi $
      nknd & (o.__.o.o.__.o.o.__) . nknd & (__.__.o.o.__.o.o.__)
    . nknd & (o.__n 8)            . nknd
    where
    nknd = n.k.n.d.__.n.d.__

yt_1 :: Korvai
yt_1 = sarvalaghu $ source "https://www.youtube.com/watch?v=OxESPQM08MA" $
    korvaiS1 adi $ r2 $
        rh & "o__o" . rh & "_oo_" . rh & "o__p" . rh & "_pp_"
    where
    rh = n.k.k.d

-- * melkalam

mel1 :: Korvai
mel1 = sarvalaghu $ sudhindra $ korvaiS1 adi $
    repeat 4 $ on.od.on. su (pk.n.o).od.on . su pk
    -- ta din ta din takadin ta din

mel2 :: Korvai
mel2 = sarvalaghu $ sudhindra $ korvaiS1 adi $ su $
    repeat 2 $ repeat 3 (yjyj.d.__.lt p.k) . (t.k.o.o.k.t.o.k)
    where yjyj = y.j.y.j

-- reduce with kir2 and kir5

dinna_kitataka :: Korvai
dinna_kitataka = exercise $ sudhindra $ korvaiS adi $
    map (sarvaSam adi) patterns
    where
    patterns = map su
        [ repeat 4 dinna
        , repeat 2 (od.__.dinna).dinna
        , repeat 2 (o.k.dinna) . dinna
        , repeat 2 (o.t.k.n.kttk) . dinna
        , tri_ (o.k) dinna
        ]
    kttk = su (k.t.o.k)
    dinna = o.n.kttk

farans :: Korvai
farans = sudhindra $ faran $ korvaiS adi $
    [ long . long
        . repeat 4 (o.o.k.t) . long
        . repeat 2 (o.o.k.t.p.k) . o.o.k.t . long
        . repeat 2 (o.o.k.t.__.k) . o.o.k.t . long
    ]
    where
    long = o.o.k.t.p.k.t.k.nakatiku


-- * ganesh

kir6 :: Korvai
kir6 = sarvalaghu $ date 2017 8 29 $ ganesh $ korvaiS adi $
    [ both . o1 rh
        -- TODO second half has D after prefix
        -- I could maybe do that by having transparent strokes, so I could
        -- add the trailing thom.  But it's seems over general for this
        -- specific case, e.g. I don't see how solkattu would support it.
    , prefix `replaceStart` both . prefix `replaceStart` rh
        . repeat 2 (prefix .od.l.od.on.l) . prefix `replaceStart` rh
        . prefix `replaceStart` both . (prefix . prefix) `replaceStart` rh
        . repeat 2 prefix `replaceStart` both
            . repeat 2 prefix `replaceStart` rh
        . (repeat 2 prefix . su (od.n.p.k) . prefix) `replaceStart` both
            . prefix `replaceStart` rh
        . repeat 2 prefix . repeat 5 (su (od.n.p.k)) . prefix `replaceStart` rh
    ]
    where
    rh = d.__.n.d. l.d.n. l.d.l .n.d. l.d.n.l
    lh = thomLH rh
    both = rh & lh
    prefix = su $ od.__.od.n.p.k -- din dinataka

kir_misra_1 :: Korvai
kir_misra_1 = sarvalaghu $ date 2017 8 29 $ ganesh $ korvaiS1 Tala.misra_chapu $
    sd $ rh & thomLH rh . o1 rh
    where rh = n.l.n.n.d.l.n.l.d.l.n.n.d.l

-- | Misra version of nddnnddn.
kir_misra_2 :: Korvai
kir_misra_2 = sarvalaghu $ date 2017 9 26 $ ganesh $ korvaiS1 Tala.misra_chapu $
    sd $ sd $ rh & thomLH rh . o1 rh
    where rh = n.d.n . su (n.n) . d.d.n

c_17_10_23a :: Korvai
c_17_10_23a = sarvalaghu $ date 2017 10 23 $ ganesh $ korvaiS1 adi $
    repeat 2 $ repeat 3 cell . n.d . su (p.k.t.k)
    where
    cell = n.d . su (p.k) . d

c_17_10_23b :: Korvai
c_17_10_23b = sarvalaghu $ date 2017 10 23 $ ganesh $ korvaiS1 adi $
    su $ repeat 2 $ p.n.p.k.d.__.p.k . repeat 3 (n.__.p.k.d.__.p.k)

-- * mine

c_chatusram1 :: Korvai
c_chatusram1 = elaforge $ sarvalaghu $ korvaiS adi
    [ "D,nd,nN," . "D,nd,nN".su "ok" . "D,nd,nN," . su "D_n_n_pn_ld_pn_l"
    , "D,nd,d".su "n,n,"."Dnnonn".su "nook"."D,nd,nN,".su "N_N_N_pn_ld_pn_l"
    ]

c_kandam1 :: Korvai
c_kandam1 = elaforge $ date 2023 3 10 $ sarvalaghu $ korvaiS adi $ map (nadai 5)
    [ r3 "d,dn," . su "d_pkd_n_,_" . su (r2 "d_pkd_n_pk" . "d_pkd_n,^,"
        . "dpkd_pn_pk")
    , su $ r2 $ r3 "dpkd_pn_pk" . "d_".nakatiku
    , su $ r2 "d_pkd_n,p," . "d_pk" . nakatiku.nakatiku
        . r2 "d_pkd_n_,_" . "dpkd_" . r3 p5
    , r3 "dlNN," . su "d_pkD_N_,_"
    . r2 "dlNN," .su "d_pkD_N_,_" . su "d,p,D_N,n,"
    ]

c_kandam_tisram :: Korvai
c_kandam_tisram = elaforge $ date 2023 3 10 $ sarvalaghu $ korvaiS1 adi $
    r3 "dlNN," . su "d_pkD_N_,_"  . su "d,^,d,n,^,"
    . su (g "k_p_n_ktpkptok")

-- * misra

c_18_05_25 :: Korvai
c_18_05_25 = sarvalaghu $ date 2018 5 25 $ ganesh $ korvaiS1 Tala.misra_chapu $
    sd $
      o .k.on.on.od.__.on.k.od.k.on.on.od.__
    . on.k.on.on.od.__.on.k.od.k.on.on.od.__
    . on.k. n. n. d.__. n.k. d.k. n. n. d.__
    .  n.k. n. n. d.__. n.k.od.k.on.on.od.k

-- * candiramani

candiramani_pakhawaj_kehrwa :: Korvai
candiramani_pakhawaj_kehrwa = korvaiS (Tala.beats 4)
    -- "dhin_dhadhin_dhindhage" . "dhin_dhatin_tintaage"
    [ "D_nD_dNo" . "D_nd_dno"
    -- "dhin_dhin_dha_tin_" . "trakra dhin_dha_ trakra"
    , "U_U_N_".p&i.__ . "ktU_nokt" . "U_U_N_".p&i.__ . "ktU_nnkt"
    -- "dhet ti ta taa" . "_dhitati" . "katatitaa" . "_dhitati"
    , su $ "o_k_t_u__oK_t_u_" . "p_k_t_u__oK_t_u_"
         . "o_k_t_u__oo___i_" . "p_k_t_u__".su "pk"."o_k_t_"
    ]

candiramani_pakhawaj_adi :: Korvai
candiramani_pakhawaj_adi = korvaiS adi
    -- "dha_ki_ṭa_dha_kiṭadha_" . "ki_ṭa_ka_ti_" . "ṭa_taa_tiṭakata gadigene"
    [   "U_k_toU_ktU_" . "k_t_p_" . "k_t_u_" . "ktpuoiok"
      . "U_k_toU_ktU_" . "k_t_p_" . "k_t_U_" . "ktpuoiok"
    -- "dha_ki_ṭa_dha_" . "dhet_dhi_ṭa_taa_" . "ka_ti_ṭa_dha_dhet_dhi_ṭa_dha_"
    ,   "U_k_t_u_o_K_t_u_" . "p_k_t_u_" . "o_K_t_u_"
      . "U_k_T_u_o_K_t_u_" . "p_k_t_u_" . "o_K_t_uo"
    ]


-- * transcription

{-
    These are easier to transcribe and read as plain text.  Maybe there
    should be yet another entry mechanism, or maybe I just leave them as
    comments:

    0   .   1   .   2   .   3   .   x   .   o   .   x   .   o   .   |
                                    u   .   .   k t p k n o o k o k u
    0   .   1   .   2   .   3   .   x   .   o   .   x   .   o   ,   ;   ,   |
    u okD D N D D D N  D DN N D D N u dkd n nnd d n u pktpktk ktktkonookou k
                                                            k ktkooknpk ou k
    0   .   1   .   2   .   3   ,   ;   ,   x   .   o   .   x   .   o   .   |
    n D n d n D N D u pktpktpu kt k u pknookD okD D NND D D u  D Du U okD D
    N  D dN N D D N N pkd d n pkd pkd pknookD okD N NND D D N  D DD N D D D
    0   .   1   .   2   .   3   .   x   .   o   .   x   .   o   .   |
      uu ou   uu ou   uu ou   uu ou U okD N NND D n u kdd n nnd d n
    0   .   1   .   2   .   3   .   x   .   o   .   x   .   o   ,   ;   ,   |
      uu ou   uu ou   uu ou N okD D N  D DN N D D N u pktpkpu o o k D oknook
    0   .   1   .   2   .   3   ,   ;   ,   x   .   o   .   x   .   o   .   |
    D okD D NND D N u kdd nkoknooknooknookn D okD N NND D N u kdd n ktktkook

    0   .   1   .   2   .   3   .   x   .   o   .   x   .   o   .   |
    D   o k D   D   D   D   D   D   D   D   D     D     D     D     D

    ending
    0   .   1   .   2   .   3   .   x   .   o   .   x   .   o   .   |
    pktpu pknpupktpkpu kD pktpu pknpupktpkpu kD K   pu ko pu ko pu kD
-}
