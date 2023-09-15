-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
module Solkattu.Score.Tabla2023 where
import           Prelude hiding ((.))

import           Solkattu.Dsl.Bol


-- * pakhawaj

-- TODO I should be able to take sd off the 16 beat ones, but then the terminal
-- spacing changes, because it doesn't want to split into 4 beats
bats :: Korvai
bats = bat $ source "mary" $ korvaiS tintal $ map sd
    [ r2 "gadi gene nage tette" . "kata kata gadi gadi" . "gadi gene nage tette"

    , "taki tetTa _ tetTa _" . "taki tetdhet tette dhage"
    . "tere kita takaTa _" . tetekata

    , su $ "dhage tette Tage tette kredhet tette dhage tette"
    . "tette kredhet tette dhage tette kaTa gadi gene"
    . "taki teTa _teTa _ ga _ di _ gere nage"
    . "tere kita takaTa _" . tetekata

    , r2 "dhage   tette  | Ta ge   tette |"
    . r2 "kredhet tette  | dhage   tette |"
    . "kredhet tette  | kredhet tette | dhage tette | kredhet tette"
    . "kredhet tette  | dhage   tette | gadi gene   | nage tette"
    . "kattet tekat   | tette   katTa | kat tr kttk | Tage tette"
    . "kredhet _ dhet | tette   dhage | tette kaTa  | gadi gene"
    . "dha_ kita      | taka    dhuma | kita taka   | dhet_ Ta_"
    . "tere kita      | taka Ta _     |" . tetekata

    , "gege tette  | gadi gene  | nage tere  | kita taka"
    . "Tage tette  | gege tette | gadi gene  | nage tette"
    . "gredhin _Ta | _ ne Ta _  | gege tette | gadi gene"
    . "Tage tette  | gege tette | gadi gene  | nage tette"

    , "dhette dhette | dhage tette | kredhet tette | dhage tette"
    . r3 "kredhet tette |" . "dhage tette"
    . "kredhet _ Ta  | gene dha _    |" . tetekata
    . "nage tette  | kaTa kaTa  | ka tr kt dhet | tette kata"
    . "ge _ teran  | _ ne dha _ | di_ ge_   | __ Ta _"
    . "dha _ _ ne  | kata kata  | ge_ teran | _ne dha _"
    . "di _ ge _   | _ _ Ta _   | dha_ _ne  | kata kata"
    . "ge _ teran  | _ ne dha _ | di_ ge_   | __ Ta_ | dha"
    ]

-- * akash

qaida1 :: Korvai
qaida1 = qaida $ akash $ korvaiS tintal $ map sd
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

qaida2 :: Korvai
qaida2 = qaida $ akash $ korvaiS tintal $ map sd
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

    , tri_ (dha.__) (g "dha tr kttk tuna kttk Ta tr kttk tuna kttk dha _ kttk")
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


-- * Coalby

coalby :: Korvai -> Korvai
coalby = source "coalby"

c_23_09_07a :: Korvai
c_23_09_07a = coalby $ date 2023 9 7 $ korvaiS1 tintal $
    "kat _ tette gege tette gege tun _"
    . "nana  tette kat tette ge tette"
    . tri_ (dha.__6) (tri_ "dha_" "kita")

c_23_09_07b :: Korvai
c_23_09_07b = coalby $ date 2023 9 7 $ korvaiS tintal $
    [          theme . tirikita . theme . kali nanagena
        . kali theme . tirikita . theme . nanagena

    ,          r3 (theme . tirikita) . theme . kali nanagena
        . r2 (kali theme . tirikita) . theme.tirikita . theme.nanagena

    , dhage_tette.gege_tette . r3 (kataagege.tirikita)
        . kali (dhage_tette.gege_tette . kataagege.tirikita)
        . r2 (kataagege.tirikita)

    , dhage_tette.gege_tette . r3 (kataagege.tirikita)
        . theme . tirikita . theme . kali nanagena
        . kali (dhage_tette.gege_tette) . r3 (kali kataagege.tirikita)
        . theme . tirikita . theme . nanagena
    , dhage_tette.tirikita . gege_tette.tirikita . kataagege.nanagena
        . tirikita.kali nanagena
        . kali (dhage_tette.tirikita . gege_tette.tirikita) . kataagege.nanagena
        . tirikita.nanagena

    -- , "dhage".tirikita."gege".tirikita
    , dhage_tette.__.gege_tette.__.kataagege.__.tirikita.__
        . "dhagatet gegetet kataage nanaka"
        . kali (dhage_tette.__.gege_tette.__.kataagege.__.tirikita.__)
            . "dhagatet gegetet katage nanaga"
    ,  "dhage_tet_te" . "gege_tet_te" . "kataa_ge_ge" . "tiri_ki_ta"
        . "dhaga gege kataa nana"
        .  kali ("dhage_tet_te" . "gege_tet_te") . "kataa_ge_ge" . "tiri_ki_ta"
        . "dhaga gege kataa nana"

    -- . "dha__ge__tet__te__ ge_ge_tet_te_"
    --     . (kataagege.tirikita.nanagena)
    , "dha__ge__tet__te__ ge__" . "ge_tet_te_ ka_taa_" . "gege nakatenaka"
        . kali "dha__ge__tet__te__" .ge.__3."ge_tet_te_ ka_taa_"
        . "gege nagatenaga"

    ]
    where
    dhage_tette = "dhage tette"
    gege_tette = "gege tette"
    kataagege = "kataa gege"
    theme = dhage_tette.gege_tette.kataagege
    nanagena = "nanagena"
    tirikita = "tirikita"
