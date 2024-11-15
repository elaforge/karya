-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Sarvalaghu.
module Solkattu.Score.MridangamSarva where
import           Prelude hiding ((.), repeat)

import qualified Solkattu.Tala as Tala

import           Global
import           Solkattu.Dsl.Mridangam


-- * adi kizhkalam

-- TODO these don't need to be a full avartanam, only a binary factor of it

kizh1 :: Korvai
kizh1 = sarvalaghu $ sudhindra $ korvaiV adi $
    [ r2 $ r4 (n.l.d.d) & "o_oo_oo_o_p" -- takadimi takajonu
    ]

kizh2 :: Korvai
kizh2 = sarvalaghu $ sudhindra $ korvaiV adi $
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

kizh3 :: Korvai
kizh3 = sarvalaghu $ sudhindra $ korvaiS1 adi $ repeat 2 $
    repeat 2 (n.d.__.n) & (o.o.__.o.__.o.__.o)
    . repeat 2 (n.d.__.n) & (o.__n 8)
    -- can end with faran: oonnpktk naka

kizh4 :: Korvai
kizh4 = sarvalaghu $ sudhindra $ korvaiS1 adi $
      on.__.on.__.on.od.__5.on.__.on.od.__2.o
    . on.k.on.k.on.od.__5.on.k.on.od.__2.o

kizh5 :: Korvai
kizh5 = sarvalaghu $ sudhindra $ korvaiS1 adi $
      nknd & (o.__.o.o.__.o.o.__) . nknd & (__.__.o.o.__.o.o.__)
    . nknd & (o.__n 8)            . nknd
    where
    nknd = n.k.n.d.__.n.d.__

yt_1 :: Korvai
yt_1 = sarvalaghu $ source "https://www.youtube.com/watch?v=OxESPQM08MA" $
    korvaiS1 adi $ r2 $ r4 "nkkd" & "o__o_oo_o__p_pp_"

yt_2 :: Korvai
yt_2 = sarvalaghu $ source "https://www.youtube.com/watch?v=93ywZgU9FQk" $
    korvaiV adi
    [ r2 $ rh & "o_o_oo_" . rh & "__o_ooo_"
    , "_knknnd_" & "o_o_oo_" . rh & "__o_ooo_"
        . "_knknnd_" & (su "oo" . "_o_oo_") . rh & ("_".su "_o" . "o_ooo_")
    , let rh = "ndndnndd" in r2 $ rh & "o_o_oo_" . rh & "__o_ooo_"
    , "D_ND_NND_NND_NNk" . "d_nd_nnd_nnd_NNk"
    , "D_ND_NND_NND_".su "kokook" . "d_".su "ko"."D_nnd_nnd_".su "nokook"
    , "ND_kND_knd_kno".su "ktok" . "ND_knd_knd_kno".su "ktok"
    ]
    where rh = "nknknnd_"

c_24_09_08_sarva :: Korvai
c_24_09_08_sarva = date 2024 9 8 $ ganesh $ korvaiV adi
    -- work on right left independence, vary left hand
    [ r2 $ r2 "nddd" & "o/o/o/" . r2 "nddd" & "___/o/_o"
    , let rh = su (su "n,^,n_d_")
        in r2 $ r4 rh & "/_o/_o/" . r4 rh & "___/_o/"
    ]

c_24_09_11_sarva :: Korvai
c_24_09_11_sarva = date 2024 9 11 $ rohan $ sarvalaghu $ korvaiV adi
    [ r2 $ r4 "n_dd" & (__D 2 . "_oo")
    , r2 $ r4 "nddd" & (__D 2 . "__oo_oo")
    , r4 $ r2 "ndnd" & (__D 1 . su "_oo_o")
    , r2 "D_ND_kNktkND_kNk"
    , r2 $ r4 "n,nd" & (__D 2 . "___o_oo")
    , r2 "NkNknnd___NkNND_"
    , r2 $ r3 "nd_k" & "oo" . su "n_o_ktok"
    -- pallavi
    , r3 "n__d__d_" . "nn_d__d_" -- thoms?
    , r4 "nkdd_kd_" -- thoms?
    , r2 "nkkd_kkd_kkd_kkd"
    -- anupallavi
    , r3 "n_kd_kd_" . "tkookook"
    -- , r2 "d__knkdkdkdknknk"
    , r2 "d__,n,d,d,d,n,n,"
    , r4 "n,n,d,n,"
    -- dom ki Ta ta ka dom ki Ta ; ta ka dom cha ta ki Ta ta ; ka ta ki Ta ta
    -- ka ta cha (16)
    , r2 $ o.kttk.o.kttk.o.v.p.kttk.p.kttk.p.v
    , let nk = su "n,"; nknk = r2 nk
        in r2 $ n.d.nk.d.nknk.d.nknk.d.nk.d.nknk.d.nk
    -- charanam
    , r2 $ "Nd_knd_knd_no".v&o."_k"
    , r2 $ o.su "_k" . "ooko_kt". su "_k" . "ooko_k" -- no nam din?
    ]
    where
    kttk = su "ktpk"

kdn_sarva :: Korvai
kdn_sarva = elaforge $ sarvalaghu $ korvaiV adi
    [ r2 $ r4 "_ndn" & "o__o__o_ ___p__o_"
    , "koN_" . r3 kdn_ . "koN_" . r2 kdn_ . su "pkpkd_n_"
        . r2 kdn_ . "kdn" . su "pkpko_pn_n" . "poN_" . r3 kdn_
        . r2 kdn_ . "k_" . su "d_n_pkd_n_pk" . "ndn_" . r2 kdn_ . su "k_pkpko_"
        . r3 kdn_ . su "pnpk"."dk"."koN_" . r2 kdn_ . su "onpknoD_"
    ]
    where
    kdn_ = "kdn_"

ndn_sarva :: Korvai
ndn_sarva = elaforge $ sarvalaghu $ korvaiV adi
    [ r3 "_ndn" & "o__o__o" . su "__v_ktok"
        . r3 "_ndn" & "o__o__o" . su "_upknook"
    , r2 $ r3 ".ndn" & "o__o" . ".o".su "ktok"
    , r2 $ r2 ".ndn" . r2 ".ndn" & "___o_oo"
    , r4 ".ndn" & o . r4 ".ndn" & "__oo_o_oo_o_oo_o" -- kendang sunda pattern
    ]

rohan_sollus_end :: Korvai
rohan_sollus_end = rohan $ sollu $ korvaiV adi
    [ __D 3.25 . su "okooko".o . __D 3 . su "otootoo"       -- 75
    , __D 2.5 . "otkn".kttk.od . __D 2.25 . "ktkn".kttk.od  -- 75
    , __D 7 . su "_upknook"                                 -- 75
    , __D 7 . su "nkktkktk"                                 -- 60
    , __D (6 + 6/8) . su "_np nnoD_ onn"                    -- 60
    , __D 2.5 . su (__ . tri_ od "npk") . od.__n 10 . su (r3 "npkD") -- 75
    , __D 4 . su ("oo_o_oo_" . r2 "___o_oo_" . "o_oo_o_o").od -- 75
    , __D 5 . su ("o___" . r2 (g "ktktpktp") . "ktkt")      -- 85
    , __D 5 . su "pu_ko_k_ pu_ko_k_ n_pu_ko_"               -- 75
    , __D 3 . "ktkt" . "ktkn".kttk."Tk" . "tkoo".kttk."Tk"  -- 60
    , __D 3 . "ktkp" . "ktkn".kttk."Tk" . su "n_ktpkT_k_n_ktpk" -- 60
        . "otkn".kttk.od .__ . __D 6
    , let kttk = "kt" in __D 5.5                            -- 60
        . su ("ktkp ktk" . r2 (n.kttk."Tk") . n.kttk
        . "oktn".kttk).od.__5 . __D 6
    , __D 4 . su ("N___koD_" . "N_k_kpko" . "D_N_k_ko" . "D_N_k_kp") -- 70
    , __D 2 . su "o_k_on,nok N_D_k_" . __D 2 . su "on,npk on,npk N_k_" -- 70
    , __D 4 . su ("o_t_k_N_ktpkoktk" . "nookN_D_k_N_ktpk")  -- 85
    , __D 6 . su ("puko" . r4 "Uko")                        -- 75
    , __D 6 . su "kt_kk_ktkk_oN_k_".od
        -- do with 3x: Nk_, N_k_, N__k_
    , __D 6 . su ("N_kNkNNk" . "NNkNkNNk")                  -- 60
    , __D 6 . su ("ookTkpnn" . "ppkTkonn")
    -- play T forward
    , __D 4 . su ("o_k_okTko_N_ktpk" . "n_k_okTko_N_ktpk")
            -- dhere for kTk
    , __D 4 . "otkNkdpn" . "ptkNkdpn"
    , __D 6 . "Nkdpnoto"
    ]
    where
    kttk = su "ktpk"
    -- for kt, keep thumb tucked in

sollu_variations :: Korvai
sollu_variations = sollu $ korvaiV adi
    [ su $ r4 theme
        . r2 (g "Nkdpn ptkNkdpn") . r2 (g "Nkdpn") . tri_ "d__" "Nk"
        . od.__8.nakatiku
    , su $ theme . r2 (g "Nkdpn ptkNkdpn") . r2 (g "Nkdpn") . tri_ "d__" "Nk"
        . od
    ]
    where
    theme = g "otkNkdpn ptkNkdpn"
    -- o    tk   N  k d   p  nptk   Nkdpn
    -- dom taka Ta ka din na nakadinna Ta ka din ta na
    -- 3        4            4         5


-- * adi melkalam

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
dinna_kitataka = exercise $ sudhindra $ korvaiV adi $
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
farans = sudhindra $ faran $ korvaiV adi $
    [ long . long
        . repeat 4 (o.o.k.t) . long
        . repeat 2 (o.o.k.t.p.k) . o.o.k.t . long
        . repeat 2 (o.o.k.t.__.k) . o.o.k.t . long
    ]
    where
    long = o.o.k.t.p.k.t.k.nakatiku


-- * ganesh

din_nadin :: Korvai
din_nadin = sarvalaghu $ ganesh $ korvaiV adi
    [ r2 "D.ND.DN." . o & r2 "d.nd.dn."
    , r2 $ su $ r2 $ d.__.p.k.n.l.d.__.p.l.d.__.n.__.l.__
    , r2 $ su $ r2 $ d.__.p.k.n.l.d.l.p.l.d.l.n.l.p.l
    , r2 $ rh & "o_o/o/__ ___/o/"
    ]
    where rh = "d_nd.dn.d.nd.dn."

nadin_ka :: Korvai
nadin_ka = sarvalaghu $ ganesh $ date 2017 5 15 $ korvaiV adi
    [ r2 $ on.od.__.k.(n.d.__.k).(n.d.__.k).o.od.__.k
    , nadai 5 $ r2 $ "ND_k" . r3 "nd_k" . "oD_k"
    , nadai 6 $ r2 $ "ND_k" . r4 "nd_k" . "oD_k"
    ]
    -- 4 nd to switch to kandam

nadindin :: Korvai
nadindin = sarvalaghu $ korvaiV adi $ map sd
    [ template $ lh & rh
    , template $ su (on.on) . od.od.on
    , template $ su (su (k.t.o.k) . o.k) . o . k
    , "NDD" . su "Knon" . "DDN". "Ndd" . su (su "__pn_ktpk_o_") . "DN"
    -- TODO if I have a notation for alternatives I could put it in here
    -- melkalam
    , su $ inter l $ r4 rh & (r8 o . o.__4 . __ . r3 o)
    -- TODO For the others, I should have a way to intersperse at a certain
    -- speed, or maybe mix together two sequences.  Or maybe I can infer 'l'
    -- for melkalam?
    ]
    where
    template var = (lh & rh) . var . (rh . rh) & (o.__4 . __ . r3 o)
    rh = n.d.d.n
    lh = o.o.o.o

namita_dimita :: Korvai
namita_dimita = sarvalaghu $ korvaiV adi
    [ r2 (rh & "o__o__o_") . rh & o . rh & "__o__o_"
    ]
    where rh = "n.dd.dd."

kir6 :: Korvai
kir6 = sarvalaghu $ date 2017 8 29 $ ganesh $ korvaiV adi $
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

c_17_10_23a :: Korvai
c_17_10_23a = sarvalaghu $ date 2017 10 23 $ ganesh $ korvaiV adi
    [ repeat 2 $ repeat 3 cell . n.d . su (p.k.t.k) ]
    where
    cell = n.d . su (p.k) . d

-- * misra chapu

kir_misra_1 :: Korvai
kir_misra_1 = sarvalaghu $ date 2017 8 29 $ ganesh $ korvaiS1 Tala.misra_chapu $
    sd $ rh & thomLH rh . o1 rh
    where rh = n.l.n.n.d.l.n.l.d.l.n.n.d.l

-- | Misra version of nddnnddn.
kir_misra_2 :: Korvai
kir_misra_2 = sarvalaghu $ date 2017 9 26 $ ganesh $ korvaiS1 Tala.misra_chapu $
    sd $ sd $ rh & thomLH rh . o1 rh
    where rh = n.d.n . su (n.n) . d.d.n

c_17_10_23b :: Korvai
c_17_10_23b = sarvalaghu $ date 2017 10 23 $ ganesh $ korvaiV adi
    [ su $ repeat 2 $ p.n.p.k.d.__.p.k . repeat 3 (n.__.p.k.d.__.p.k) ]

-- from Vijayadashami concert
c_24_10_23 :: Korvai
c_24_10_23 = sarvalaghu $ korvaiV Tala.misra_chapu
    [ r2 "N_pkD_". "N_pkn_" . "pkd_pkd_pk"
    , r2 "N_pkD_". "N_pknookD_pkd_pk"
    , r2 "N_pkD_". "N_pknookD_pknook"
    , r2 "N_pkd_". r2 "D_N_pkd_"
    , r2 "N_pknookD_" . "N_pkD_pk"
    ]

-- * mine

c_chatusram1 :: Korvai
c_chatusram1 = elaforge $ sarvalaghu $ korvaiV adi
    [ "D,nd,nN," . "D,nd,nN".su "ok" . "D,nd,nN," . su "D_n_n_pn_ld_pn_l"
    , "D,nd,d".su "n,n,"."Dnnonn".su "nook"."D,nd,nN,".su "N_N_N_pn_ld_pn_l"
    ]

c_kandam1 :: Korvai
c_kandam1 = elaforge $ date 2023 3 10 $ sarvalaghu $ korvaiV adi $ map (nadai 5)
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
candiramani_pakhawaj_kehrwa = korvaiV (Tala.beats 4)
    -- "dhin_dhadhin_dhindhage" . "dhin_dhatin_tintaage"
    [ "D_nD_dNo" . "D_nd_dno"
    -- "dhin_dhin_dha_tin_" . "trakra dhin_dha_ trakra"
    , "U_U_N_".p&i.__ . "ktU_nokt" . "U_U_N_".p&i.__ . "ktU_nnkt"
    -- "dhet ti ta taa" . "_dhitati" . "katatitaa" . "_dhitati"
    , su $ "o_k_t_u__oK_t_u_" . "p_k_t_u__oK_t_u_"
         . "o_k_t_u__oo___i_" . "p_k_t_u__".su "pk"."o_k_t_"
    ]

candiramani_pakhawaj_adi :: Korvai
candiramani_pakhawaj_adi = korvaiV adi
    -- "dha_ki_ṭa_dha_kiṭadha_" . "ki_ṭa_ka_ti_" . "ṭa_taa_tiṭakata gadigene"
    [   "U_k_toU_ktU_" . "k_t_p_" . "k_t_u_" . "ktpuoiok"
      . "U_k_toU_ktU_" . "k_t_p_" . "k_t_U_" . "ktpuoiok"
    -- "dha_ki_ṭa_dha_" . "dhet_dhi_ṭa_taa_" . "ka_ti_ṭa_dha_dhet_dhi_ṭa_dha_"
    ,   "U_k_t_u_o_K_t_u_" . "p_k_t_u_" . "o_K_t_u_"
      . "U_k_T_u_o_K_t_u_" . "p_k_t_u_" . "o_K_t_uo"
    ]


-- * transcription

-- 09-Thani Avarthanam, Aruna Sairam, Cleveland Aradhana 2004
-- mridangam: Mannargudi Easwaran
thani_24_03_26 :: Korvai
thani_24_03_26 = date 2024 3 26 $ sarvalaghu $ korvaiV Tala.rupaka_fast $ map su
    [ __M 4 . "k_k_t_k_u_" . "pkn,dNokon"
    , "D_ND_dn_" . r3 "d_nd_,n," . "d_nd_ktk" . su "n_ktok" . "okook"
    , r2 "D_ND_,N," . r2 ("tknd,dnk" & "o__o__o_") . r2 "tknd,dnk"
        . r5 "tknd,dnk" . su (r2 "N_ktok") . "ok"
    ]

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

    ---

    0   .   1   .   2   .   3   .   x   .   o   .   x   .   o:6   .     |
                    pkookookook ok kD k D       N k N k D pknpupktpko ok
                                                          pktpupktpu ktok ?
    0   .   1   .   2   .   3   .   x   .   o   .   x   .   o   .   |
    D k D       N k N k D pknpupktpkN D , N , N D , N , N D , N ktok

    0   .   1   .   2   .   3   .   x   .   o   .   x   .   o   .   |
    N N , N n , D     , ^ , ^ , ^ , N N , N n , D     , ^ , ^ , ^ ,

    0:6   .     1     .     2     .     3     .     4     .     x
    N N , N n , D   , ^ , ^ , ^ , ^ ,

    0   .   1   .   2   .   3   .   x   .   o   .   x   .   o   .   |
                k k k o D  kD k D k D
-}

-- * sollus

sollus :: Korvai
sollus = sollu $ korvaiV adi
    [ sarvaD_ 5.5 . "koD".su "_k"."DkDkD_"
    -- just pk.nakatiku.nakatiku
    , sarvaD_ 4.5 . "kktku".su ("pk".r2 "tpupktpk")
    , sarvaD_ 5 . su "u_pktpktpu_kt_k_u_pknook"
    ]

embellishments :: Korvai
embellishments = sollu $ korvaiV adi
    -- [ su "onpkno" `replaceStart` sarva `replaceEnd` su "onpkno"
    [ startEnd (su "onpkno") sarva
    , startEnd (su "pnpknp") s_nd_k
    , startEnd (su (r2 "onpkno")) sarva
    , startEnd (su "oktkno") sarva
    , startEnd (su "pu_kno") sarva
    , startEnd (su "pu_knpu_kno_") sarva
    ]
    where
    startEnd sol sarva = sol `replaceStart` sarva `replaceEnd` sol
    sarva = r2 "N.dD.dD." . "N.dd.dd.n.dD.dD."
    -- sarva = r2 "n_ddn_ddnoDdn_dd"
    s_nd_k = r2 $ r3 "nd_k" . su "n_o_ktok"
