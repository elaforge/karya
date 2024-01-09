-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
module Solkattu.Score.Tabla2023 where
import           Prelude hiding ((.))

import qualified Solkattu.Talas as Talas

import           Solkattu.Dsl.Bol


-- * pakhawaj

-- TODO I should be able to take sd off the 16 beat ones, but then the terminal
-- spacing changes, because it doesn't want to split into 4 beats
bats :: Korvai
bats = bat $ source "mary" $ korvaiS tintal $
    [ sd $ r2 "gadi gene nage tette" . "kata kata gadi gadi"
        . "gadi gene nage tette"

    , sd $ "taki tettA _ tettA _" . "taki tetdhet tette dhage"
    . "tere kita takatA _" . tetekata

    , "dhage tette tAge tette kredhet tette dhage tette"
    . "tette kredhet tette dhage" . tetekata
    . "taki tetA _tetA _ ga _ di _ gere nage"
    . "tere kita takatA _" . tetekata

    , r2 "dhage   tette  | tA ge   tette |"
    . r2 "kredhet tette  | dhage   tette |"
    . "kredhet tette  | kredhet tette | dhage tette | kredhet tette"
    . "kredhet tette  | dhage   tette | gadi gene   | nage tette"
    . "kattet tekat   | tette   kattA | kat tr kttk | tAge tette"
    . "kredhet _ dhet | tette   dhage | tette katA  | gadi gene"
    . "dha_ kita      | taka    dhuma | kita taka   | dhet_ tA_"
    . "tere kita      | taka tA _     |" . tetekata

    , "gege tette  | gadi gene  | nage tere  | kita taka"
    . "tAge tette  | gege tette | gadi gene  | nage tette"
    . "gredhin _tA | _ ne tA _  | gege tette | gadi gene"
    . "tAge tette  | gege tette | gadi gene  | nage tette"

    , "dhette dhette | dhage tette | kredhet tette | dhage tette"
    . r3 "kredhet tette |" . "dhage tette"
    . "kredhet _ tA  | gene dha _    |" . tetekata
    . "nage tette  | katA katA  | ka tr kt dhet | tette kata"
    . "ge _ teran  | _ ne dha _ | di_ ge_   | __ tA _"
    . "dha _ _ ne  | kata kata  | ge_ teran | _ne dha _"
    . "di _ ge _   | _ _ tA _   | dha_ _ne  | kata kata"
    . "ge _ teran  | _ ne dha _ | di_ ge_   | __ tA_ | dha"
    ]

-- * akash

akash_kaida1 :: Korvai
akash_kaida1 = kaida $ akash $ korvaiS tintal $ map sd
    [ r2 "dha dha trkt dha dha tu na taa taa trkt dha dha tu na"
    -- palta 1
    , "dha dha trkt dha dha trkt dha dha trkt dha dha tu na"
    . "taa taa trkt taa taa trkt dha dha trkt dha dha tu na"
    -- palta 2
    , "dha dha dha trkt dha trkt dha dha trkt dha dha tu na"
    . "taa taa taa trkt taa trkt dha dha trkt dha dha tu na"
    -- palta 3
    , "dha dha trkt dha trkt dha dha dha trkt dha dha tu na"
    . "taa taa trkt taa trkt taa dha dha trkt dha dha tu na"
    -- palta 4
    , "dha dha trkt trkt trkt dha dha trkt dha dha tu na"
    . "taa taa trkt trkt trkt dha dha trkt dha dha tu na"
    -- palta 5
    , "trkt trkt dha dha trkt dha dha trkt dha dha tu na"
    . "trkt trkt taa taa trkt dha dha trkt dha dha tu na"
    , tri_ (dha.__8) (g "dha dha trkt dha dha tu na taa taa trkt dha dha tu na")
    -- first speed tihai
    , tri_ "dha___" (g "dha dha trkt dha dha tuna")
    ]

akash_kaida2 :: Korvai
akash_kaida2 = kaida $ akash $ korvaiS tintal $ map sd
    [ r2 "dha trkt tk dha trkt tk taa trkt tk dha trkt tk"
    , "dha trkt tk dha trkt tk dha trkt tk tu na kt tk"
    . "taa trkt tk taa trkt tk dha trkt tk tu na kt tk"

    -- palta
    , "dha dha trkt dha trkt tk dha trkt tk tu na kt tk"
    . "taa taa trkt taa trkt tk dha trkt tk tu na kt tk"
    , "dha trkt trkt dha trkt dha trkt tk tu na kt tk"
    . "taa trkt trkt taa trkt dha trkt tk tu na kt tk"
    , "trkt trkt dha trkt tk dha trkt tk tu na kt tk"
    . "trkt trkt taa trkt tk dha trkt tk tu na kt tk"
    , "dha trkt tk _ trkt tk dha trkt tk tu na kt tk"
    . "taa trkt tk _ trkt tk dha trkt tk tu na kt tk"

    , tri_ (dha.__) (g "dha tr kttk tuna kttk tA tr kttk tuna kttk dha _ kttk")
    ]

-- farmaisi chakradar paran
farmaisi :: Korvai
farmaisi = akash $ korvaiS1 tintal $ nadai 3 $
    r3 $ g $ r2 "dhadha gena tette" . "takita dha trkt" . "dhadha gena tette"
    . "kran__ kran__" . (nadai 2 "tktr kttk")
    . r3 (r3 "dha trkt" . "tak kran_ dha__")
    -- first finish on 11

tukra1 :: Korvai
tukra1 = tukra $ akash $ korvaiS1 tintal $
    sd "dha dha din din na na" . "tette tette katita katita"
      . tri_ (sd "dha__") (tri_ "dha_" "kita")

tukra2_chakradar :: Korvai
tukra2_chakradar = tukra $ akash $ korvaiS1 tintal $
    r2 "dha ga tette taa ge tette"
    . "kre dhet tette dha ge tette" . "gadi gene na ge tette"
    . "dhet _ dhet _"
      . tri_ "dha_dha_" (tri_ "dha_" (g "trkt dhet _ taage _ na"))

tukra3_otaan :: Korvai
tukra3_otaan = tukra $ akash $ korvaiS1 tintal $
    "dhet_dhet_ taa__ka taa__ka ta_kat_"
    . "dhagetette taa getette" . "kre dhi _ na" . "kredha tette"
    . tri_ "dha___" (g "kredha tette dha ge tette")

tukra4 :: Korvai
tukra4 = tukra $ akash $ korvaiS1 tintal $
    "gadi gene naga tette" . "gadi gene dha _"
    . "kata gadi gene kati gadi gene dha _"
    . r3 "ka tette ghen _ neran _ na dha tuna"

tukra5 :: Korvai
tukra5 = tukra $ akash $ korvaiS1 tintal $
    "takita dhikita taka tirikita dhirikita"
    . "nagadhit_ kran__na dha_dha_"
    . "dhinnaginadha _ dhinnagina dha _"
    . "dha ge tette katta gadigene"
    . "dha _ tuna kat _"
    . r3 "dhati dha _"
    -- this doesn't line up...
    . __M (15 * 4)

tukra6 :: Korvai
tukra6 = tukra $ akash $ korvaiS1 tintal $ nadai 3 $
    "dha _ na dhikita dha trkt dhi kita"
    . "ka tette tukita dhi gene nagene"
    . "takita taa_na taa__" . tri_ "dha__" "kredhadha"


-- * colby

c_23_09_07a :: Korvai
c_23_09_07a = colby $ date 2023 9 7 $ korvaiS1 tintal $
    "kat _ tette gege tette gege tun _"
    . "nana  tette kat tette ge tette"
    . tri_ (dha.__6) (tri_ "dha_" "kita")

c_23_09_07b :: Korvai
c_23_09_07b = colby $ date 2023 9 7 $ korvaiS tintal $ map su
    [ sd $     theme . tirikita . theme . kali nanagena
        . kali theme . tirikita . theme . nanagena
    ,          r3 (theme . tirikita) . theme . kali nanagena
        . r2 (kali theme . tirikita) . theme.tirikita . theme.nanagena
    , sd $ dhage_tette.gege_tette . r3 (kataagege.tirikita)
        . kali (dhage_tette.gege_tette . kataagege.tirikita)
        . r2 (kataagege.tirikita)
    , dhage_tette.gege_tette . r3 (kataagege.tirikita)
        . theme . tirikita . theme . kali nanagena
        . kali (dhage_tette.gege_tette) . r3 (kali kataagege.tirikita)
        . theme . tirikita . theme . nanagena
    , theme.tette . r2 (ka.taa)."gege tette" . r3 (ka.taa)."gege tette"
        . theme.tirikita.theme.kali nanagena
     . kali (theme.tette . r2 (ka.taa)."gege tette" . r3 (ka.taa)."gege tette")
        . theme.tirikita.theme.nanagena
    , tri_ (dha.__8) $ g $
        tri_ (dha.__6) (dhage_tette.gege_tette.kataagege.nanagena)
        . dha.__ . kataagege.nanagena.dha.__ . kataagege.nanagena
    ]
    where
    dhage_tette = "dhage tette"
    gege_tette = "gege tette"
    kataagege = "kataa gege"
    theme = dhage_tette.gege_tette.kataagege
    nanagena = "nanagena"
    tirikita = "tirikita"

c_23_09_21a :: Korvai
c_23_09_21a = colby $ date 2023 9 21 $ korvaiS tintal
    [ nadai 3 $ "dha dha dha di di di na na na" . "kat tette dha _"
        . "dha dha di di na na" . "kat tette dha_"
        . "dha _ di _ na _" . tri_ (dha.__4) "kat tette"
    , r3 $ g t2
    , g $ "dhadhadha _ _ dididi _ _ nanana _ _ kat tette dha _"
        . t2
    -- ? shouldn't it be 3x?
    , "dhadhadha _ _ dididi _ _ nanana _ _ kat tette dha _ _"
        . "dhadha _ _ didi _ _ nana _ _ kat tette dha _ _"
        . "dha _ di _ na _"
        . tri "kattette dhadhadha _"
    ]
    where
    t2 = "dha dha dha di di di na na na" . "kat tette dha"
        . "dha dha di di na na" . "kat tette dha"
        . "dha _ di _ na _" . tri_ dha "kat tette" . dha.__4

c_23_09_29a :: Korvai
c_23_09_29a = date 2023 9 29 $ colby $ rela $ korvaiS1 tintal $
    r2 dhage_tette_din_ . "dhage tette"
        . dhage_tette_din_ . "dhage tette" . "dhage nage tenne"
        . kali (r2 dhage_tette_din_ . "dhage tette")
        . "tage tette dhin_" . "dhage tette" . "dhage nage dhenne"
    where
    dhage_tette_din_ = "dhage tette dhin _"

c_23_10_12 :: Korvai
c_23_10_12 = date 2023 10 12 $ colby $ korvaiS tintal
    [ sd $ dtt.dtt.ddtt.endk . kali (dtt.dtt.ddtt) . end
    , palta $ r2 (dtt.dtt.dha.dha)
    , palta $ r4 dtt . ddtt
    , palta $ dtt.ddtt.r3 dtt
    , palta $ r2 (dtt.ddtt).dha.__
    , palta $ dtt.ddtt.dha.__.dtt.ddtt
    , palta $ ddtt.dtt.dha.__.ddtt.dtt
    , palta $ dtt.ddtt.dha.ddtt.ddtt
    , tri_ (dha.__) (dtt.ddtt . tri_ (dha.__) dtt)
    ]
    where
    palta seq = seq.dtt.dtt.ddtt.endk . kali seq.dtt.dtt.ddtt.end
    endk = "dhage tuna kena"
    end = "dhage dhina gena"
    dtt = "dha tette"
    ddtt = dha.dtt

c_23_10_19 :: Korvai
c_23_10_19 = date 2023 10 19 $ colby $ korvaiS1 tintal $
      sd $ "nagegena gegenana gegenage tuna kena"
    . kali "nagegena gegenana" . "gegenage dhina gena"

c_23_10_19_chak :: Korvai
c_23_10_19_chak = date 2023 10 19 $ colby $ chakradar $ korvaiS1 tintal $
    tri_ (dha.__8) $
        tetekata . nadai 3 (dha.tetekata) . sd ("dhatet_teka_taga_dige_nedha")
        . tri_ (dha.__4) tetekata

c_23_10_16 :: Korvai
c_23_10_16 = date 2023 10 26 $ colby $ mukra $ korvaiS1 tintal $
    "dha_dha__ tet_te__ tet_te__ dha_dha__"
    . spread 3 (r2 "dhatette" . dha) . dha.__
    . g (r2 "dhatettedha_dha_" . "ta_dhatettedha_dha")


-- TODO some kind of more automatic, repeat with 1/2 kali?
-- But also need to do kali slightly before the middle.
-- So: kaliAt (-1) 0
-- This means kali from 2/4 - 1, to 3/4+0
c_23_11_09 :: Korvai
c_23_11_09 = date 2023 11 9 $ colby $ rela $ kaida $ korvaiS tintal
    [ theme . kali (theme1.theme2) . theme3.dha_tette.grng."dhin_na_gerenage"
    , r2 (theme1.theme2) . theme
    -- accent on Terekita
    , r2 theme1 . theme1.theme2 . theme
    , dha_tette.grng . tri_ (dha.__4) (grng.trkttk)
        . dha.__4.grng.trkt.trkttk . theme
    , dha_tette . r2 (grng.grng.trkttk.dha.__4) . grng.trkttk.dha.__4
        . grng.trkttk . theme
    -- accent beginning of each phrase
    , dha_tette . r2 (grng.grng.trkttk.dha.__4) . grng.trkttk.dha.__4
        . trkttk.dha.__4 . theme
    , dha_tette . grng.grng.trkttk.dha.__4
        . grng.trkttk.dha.__4 . trkttk.dha.__4 . "kitetaka".dha.__4
        . ki.te.dha.__4 . dha.__4
        . theme
    , dha_tette . grng.grng.trkttk.dha.__4
        . grng.trkttk.dha.__4 . trkttk.dha.__4 . "kitetaka".dha.__4
        . tri_ (dha.__) (ki.te)
        . theme
    , dha_tette . r2 (g (grng.grng.trkttk)) . theme3 . theme4
    , dha_tette.grng . r3 (grng.trkt) . theme3 . theme4
    , dha_tette.grng . r3 (trkt.grng) . theme3 . theme4
    , r2 (dha_tette.grng.trkt) . dha_tette.grng . theme3 . theme4
    , dha_tette.r3 grng.trkt.r2 grng.trkt . theme3 . theme4
    -- Prepare for transition to rela.
    -- actually 3 4 1 2, but with appropriate kali
    , theme3 . theme2 . theme1 . theme4
    , r3 (theme3 . theme2) . theme1 . theme4 -- aaab transformation
    , r3 theme3 . theme4 -- also aaab
    , grng.trkt . r3 (g (taka.dhette_tette_kite)) . theme3 . theme4
    -- with kali, dha comes back early on dhet
    , theme3 . theme4 . kali (grng.trkttk).dhette_tette_kite.theme2
    -- tihai
    -- Which is easier to write and read?
    -- , r3 $ g $
    --     theme3 . "dha_dha_dha".__6 . "dha_dha".__6 . dha.__3.dha.__3.dha.__4
    , tihai (g (theme3 . "dha_dha_dha".__6 . "dha_dha".__6 . dha.__3.dha.__3))
        (dha.__4)
    -- , tri_ (hv dha.__8) $
    --     g (theme3 . tri_ (hv dha.__) (tri_ (hv dha.__4) "dha_dha_"))
    , tihai (theme3 . tihai (tihai "dha_dha_" (dha.__4)) (dha.__)) (dha.__8)

    -- already sam to sam, can substitute any 4 beat phrase for dha_dha_
    -- , tri_ (dha.__) (tri_ (dha.__4) "dha_dha_")
    , tihai (tihai "dha_dha_" (dha.__4)) (dha.__)
    ]
    --  0 theme
    --  1 abababcd -- aab
    --  2 abababcd -- a' a' b
    --  3 " gtd gtd gtt
    --  4 " ggtd gtd gt
    --  5 " ggtd gtd td
    --  6 " gtd td ktd kd d
    --  7 " gtd td ktd ktd ktd kt
    --  8 d tt (gtkkt)2
    --  9 d tt ggt gt gt
    -- 10 d tt gt gt gt g
    -- 11 d ttgtd ttgtd ttg
    -- 12 d tt gggt ggt g
    -- 13 theme cdab
    -- 14 cdcdcd ab
    -- 15 cccd (finally drop ab)
    -- 16 gt (dhkttk)3
    -- 17 cd x2
    -- 18 tihai
    where
    theme = g (theme1 . theme2 . theme3 . theme4)
    theme1 = "dha_tette gerenaga" . "gerenaga terekite"
    theme2 = "dha_tette gerenaga" . "dhi_na_gerenage"
    theme3 = "gerenaga terekitataka" . "dhette tette kite"
    theme4 = "dha_tette gerenaga" . "tun_na_kerenake"
    dha_tette = "dha_tette"
    dhette_tette_kite = "dhette tette kite"
    trkt = "terekite"
    trkttk = "terekite taka"
    grng = "gerenaga"
    _tabla = [("tun", "ka&tun"), ("dha", "ge+din")]

-- 11 + 11 + 11 = 33
-- 10 + 1 + 10 + 1 + 10 = 32
--

-- mid is 11 if count every one
-- 6 if not
-- if start at 0 it's 10 and 5, *5 = 15 + 1/2 gaps = 16

tihai :: Sequence -> Sequence -> Sequence
tihai seq sep = tri_ (hv sep) seq

c_23_11_09_dhere :: Korvai
c_23_11_09_dhere = date 2023 11 9 $ colby $ korvaiS tintal
    [   "dha_terekitataka" . r2 "dhere dhere kitataka" . "takaterekitataka"
      . "terekitataka" . "gere dhere dhere" . "kitataka"
      . "dha_tette gerenaga" . kali "dhi_na_gerenage"
      . kali
        ( "dha_terekitataka" . r2 "dhere dhere kitataka" . "takaterekitataka")
      . "terekitataka" . "gere dhere dhere" . "kitataka"
      . "dha_tette gerenaga" . "dhi_na_gerenage"
    ]
    where
    _tabla =
        [ ("dha", "ge&na")
        , ("gere dhere dhere", "gerhe dherhe dherhe")
        ]

e_dhere :: Korvai
e_dhere = exercise $ colby $ korvaiS tintal
    [ sd $ r2 "dhere dhere kitataka" . r3 "dhere dhere" . "kitataka"
    , sd $ r2 $ "dhere dhere kitataka" . "taka terekitataka"
    ]

c_23_11_09_chak :: Korvai
c_23_11_09_chak = date 2023 11 9 $ colby $ chakradar $ korvaiS1 tintal $
    tri_ "dha___" $ "dhatette dha_tette dha_tedha_"
    . su (r2 "terekitataka" . "taka terekitataka") . taa
    . tri_ "dha_kat_" "ghen_te"

c_23_11_09_tukra :: Korvai
c_23_11_09_tukra = date 2023 11 9 $ colby $ tukra $ korvaiS1 tintal $
    "tu_na_ kttk tu_na_" . "tu_natu_na_ kttk" . "tu_na_ kttk"
    . su "tari kita taka taa_" . su tetekata . sd "dha dha dha"
    . r3 "tu_na kttk tu_na_"

c_23_11_09_b :: Korvai
c_23_11_09_b = date 2023 11 9 $ colby $ tukra $ korvaiS1 tintal $
    dha.ge.tetekata.dha.__4 . sd tetekata.dha.__4
    . tri_ "__" (r3 (dha.ge.tetekata))

c_23_11_16 :: Korvai
c_23_11_16 = date 2023 11 16 $ colby $ kaida $ korvaiS tintal $ map (nadai 3)
    [   theme1
      . kali (dhatette.dhagena.dhettette.dhagena)
        . dhatette.dhagena.dhagedhinagena
    -- TODO kali is predictable, I could either leave it out or have an
    -- automatic inference
    , dhatette.dhagena . r3 (dhettette.dhagena.dha.__)
        . dhettette.dhagena.dhatette.dhagena.dhagetunakena -- kali
    , dhatette.dhagena . r3 (dhettette.dhagena) . theme1 -- kali
    , dhatette.dhagena.dhettette.dhagena . r3 dhettette.dhagena . theme1
    , dhatette.dhagena.dhettette.dhagena . r3 "_dhette".dhagena . theme1
    , dhatette.dhagena.dhettette.dhagena . "___" . r3 dhette.dhagena . theme1
    , dhatette.dhagena.dhettette.dhagena . dhet7.dhet5 . theme1

    -- 2023-11-30
    , dhatette.dhagena.dhettette.dhagena . r3 (dhet7.dhet5)
        . dhatette.dhagena.dhettette.dhagena . dhet7.dhet5 . theme1
    , dhatette.dhagena.dhettette.dhagena . r3 dhet7 . r3 dhet5
        . dhatette.dhagena.dhettette.dhagena . dhet7.dhet5 . theme1
    , dhatette.dhagena.dhettette.dhagena . dha.__. r2 dhet5 . theme1
    , dhatette.dhagena.dhettette.dhagena . r2 (__.dhet5) . theme1
    , dhatette.dhagena.dhettette.dhagena . nadai 4 (__.r3 dhet5)
        . theme1
    , dhatette.dhagena.dhettette.dhagena . nadai 4 (r2 dhet5.__.dhet5) . theme1
    , dhatette.dhagena.dhettette.dhagena
        . nadai 4 (dha.__.r2 dhet5) . nadai 5 dhet5
        . theme1
    , dhatette.dhagena.dhettette.dhagena
        . tri_ (nadai 4 (dha.__)) (nadai 5 dhet5) . theme1
    , dhet5.dha.__.dhet5.dha.__.dhette.r2 dhagena.dha.__ . theme1
    , tri_ "dha__" $ dhatette.dhagena.dhagedhinagena. tri_ "dha_ne" "dha_dha_"
        -- dha is ge&tin
    ]
    where
    dhet5 = g $ dhette.dhagena
    dhet7 = g $ r2 dhette.dhagena
    theme1 = dhatette.dhagena.dhettette.dhagena.dhatette.dhagena.dhagetunakena
    dhatette = "dhatette"
    dhagena = "dhagena"
    dhettette = "dhettette"
    dhagedhinagena = "dhage dhina gena"
    dhagetunakena = "dhage tuna kena"
    dhette = "dhette"
    _tabla = [("dha", "ge&na")] -- because kaida

c_23_11_30 :: Korvai
c_23_11_30 = date 2023 11 30 $ colby $ korvaiS1 tintal $
    r3 "dhatuna".dha.__.r2 "dhatuna".dha.__
        . tri_ (dha.__3) (tri_ (dha.__) "dhatuna")
    -- karvai dha on tin
    -- replace dhatuna with any 3 or 6

c_23_11_30_tukra :: Korvai
c_23_11_30_tukra = date 2023 11 30 $ colby $ tukra $ korvaiS1 tintal $
    "takadi_" . "kitataka" . "takaterekitataka" . "takaterekitadin_"
    . "dha__redha___" . "terekitatakataa_".tetekata
    . dha.__4.taa.__4."gadigenedha___"
    . tri_ (dha.__4."kitataka") ("terekitatakataa_".tetekata)

c_23_12_29_rela :: Korvai
c_23_12_29_rela = date 2023 12 29 $ colby $ rela $ korvaiS tintal
    [ theme1 . theme2 . theme1
        -- TODO usual kali transformation
        . dhenne_gene.takga_dhenne.nanagene.kali dhenne_gene
        . kali (theme1 . theme2) . theme1 . theme1
    -- reduced version
    , sd $ "dhenne takdhen naga dhenne" . r2 "takdhen naga"
        . "dhenne takdhen naga dhenne"
        . "dhenne takdhen naga ".kali "dhenne"
        . kali ("dhenne takdhen naga dhenne" . r2 "takdhen naga")
        . r2 "dhenne takdhen naga dhenne"
    ]
    where
    theme1 = dhenne_gene.takga_dhenne.nanagene.dhenne_gene
    theme2 = takga_dhenne.nanagene.takga_dhenne.nanagene
    dhenne_gene = "dhenne gene"
    takga_dhenne = "takga dhenne"
    nanagene = "nanagene"

c_23_12_29_tukra :: Korvai
c_23_12_29_tukra = date 2023 12 29 $ colby $ tukra $ korvaiS tintal
    [ "ta_tun_na_ kitatakatun_ terekitatun_na_ kitataka" . r4 "tAka"
        . "takaterekite"
        -- Awkward to express that the last one is different.
        . tihai2 ("ghen_taran_ne" . tihai "dha_ti_" (dha.__4)) (dha.__6)
        . "ghen_taran_ne" . "dha_ti_" . sd (nadai 3 (r2 (dha.dha.ti)))
    ]
    -- na on sur

tihai2 :: Sequence -> Sequence -> Sequence
tihai2 seq sep = seq.sep.seq.sep

-- * candiramani tape

candiramani_pakhawaj_kehrwa :: Korvai
candiramani_pakhawaj_kehrwa = theka $ korvaiS Talas.kehrwa
    [ sd $ "dhin_dhadhin_dhindhage" . "dhin_dhatin_tintaage"
    , sd $ "dhin_dhin_dha_tin_" . "trakra dhin_dha_ trakra"
    , sd $ "dhet ti ṭa taa" . "_dhiṭati" . "kataṭataa" . "_dhiṭati"
    ]

candiramani_pakhawaj_adi :: Korvai
candiramani_pakhawaj_adi = theka $ korvaiS Talas.adi
    [ "dha_ki_ṭa_dha_kiṭadha_" . "ki_ṭa_ka_ti_" . "ṭa_taa_tiṭakata gadigene"
    , "dha_ki_ṭa_dha_" . "dhet_dhi_ṭa_taa_" . "ka_ti_ṭa_dha_dhet_dhi_ṭa_dha_"
    ]

-- *

legong1 :: Korvai
legong1 = korvaiS kehrwa
    [ "na___ ge_ge_ | na___na_na_ | ge _na _ ge_ge_ | na___na_na_"
    . "ge_tet_ na_na_ | ge_na_ge_gege | _na_na_dha_dha_ | __tuntun_tet_"
    , "ge_tet_na_tet_ | na_na_tet_na_ | __tet_na_tet_"
        . su "tiriki tu" . "tun tun tun _ge _"
    , "ge_na___na_ | ge_na_ge_gege | _na_na_dha_dha | __natuntun_tet_"
    . "ge_tet_na_tet_ | na_na_tet_na_ | __tet_na_tet_ | na_na_tet_na_"
    . "tet_tuntuntuntet_na | _gege_ge_ge_ | na___tet_na_na_ge_na___"
    , "gegegena_gegena | gegegena_gegena | ge_na_ge_ge_ | na_ge_na___"
    . "ge_tet_ge_ge_ | _tun_na_tun_tun | tun__ge_na___ | gege_ge_ge_ | na"
    ]

{-
    L   P   T   U Y     O
    W k   t   u     . a

    |   .   1   .   2   .   3   .   4   .   5   .   6   .   7   .   8
    a       O   O   a     t T t T a O t T a O a O . a     t T t T a O
      k P t T t T a O t T a O a O O . a   a   O   O   . a Y Y k P a O
      k P t T   t   T   T   t   T     k P t T k P t P  YY Y Y a O a O
      . a     t T a O t T a O a O O . a   a   O   O   . a Y Y k P a O
    |   .   1   .   2   .   3   .   4   .   5   .   6   .   7   .   8
      k P t T   t   T   T   t   T     k P t T k P t T t T k P t T k P
        Y Y Y P . a   O O a O a O . a     k P t T t T a U . a       O
      O O a   O O a O O O a   O O a O t T a O a O . a   O   a       O
      k P a U a U . a Y . a   Y   Y Y a   U . a       U U a U a U . a
    |   .   1   .   2   .   3   .   4   .   5   .   6   .   7   .   8
-}
{-
      .   1   .   2    .   3   .   4    .   5   .   6    .   7   .   8
          -   -   o|   k n k n k o +| n k o l o l . o|   k n k n k o -
          o   o   n|     k n k n k o| + n k o l o l n|     k n k n k o

      .   1   .   2    .   3   .   4    .   5   .   6    .   7   .   8
    k + n k n k o -| n k o l o   k .| D   D   +   +  | . D   - k   o -
    - k   n k n k o| - n k o l o o  | n   n   D   D  |     i i   k   o

      .   1   .   2    .   3   .   4    .   5   .   6    .   7   .   8
    k + n k   n   k|   k   n   k   -| k - n - k - n -| k - k t o - o -
    +   k d   k   n|   n   k   n    | - k - n - k - k|t+ii i i   o - o

      .   1   .   2    .   3   .   4    .   5   .   6    .   7   .   8
    . D     n   o -| n   o - o - - .| D   D   +   +  | . o   - k   o
    - n       n   o|   n   o   o o  | n   n   D   D  |   n i i   k   o

      .   1   .   2    .   3   .   4    .   5   .   6    .   7   .   8
    + k n k   n   k|   k   n   k   -| k - n - k - n -| n - k - n - k -
    +   k d   k   d|   d   k   d    | - k - n - k - n| - n - k - n - k

      .   1   .   2    .   3   .   4    .   5   .   6    .   7   .   8
    k     u k . D  | - - o - o - . D|   k - k n k n k| o l . D       -
      i i i k      | o o n o n o   n|   - k - k n k n| k o   n       o

      .   1   .   2    .   3   .   4    .   5   .   6    .   7   .   8
      - o   -   o -|   - o   -   o  | n k o - o   . D|   - . D       -
    o o n   o o n o| o o n   o o n o| k n   o   o   n|   o   n       o

      .   1   .   2    .   3   .   4    .   5   .   6    .   7   .   8
    k - o - o   . D|   . D   u   u  | o   - . o      | - - o - o - . o
    - k - o   o   -| i   n   i   i i|     o - n      | o o n o n o   n
-}
