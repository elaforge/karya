-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
module Solkattu.Score.Tabla2023 where
import           Prelude hiding ((.))

import           Global
import           Solkattu.Dsl.Bol


qaida1 :: Korvai
qaida1 = qaida $ akash $ korvaiS kehrwa $ map sd
    [ "dha dha trkt dha dha tu na taa taa trkt dha dha tu na"
    -- palta 1
    , "dha dha trkt dha dha trkt dha dha trkt dha dha tu na"
    , "taa taa trkt taa taa trkt dha dha trkt dha dha tu na"
    -- palta 2
    , "dha dha dha trkt dha trkt dha dha trkt dha dha tu na"
    , "taa taa taa trkt taa trkt dha dha trkt dha dha tu na"
    -- palta 3
    , "dha dha trkt dha trkt dha dha dha trkt dha dha tu na"
    , "taa taa trkt taa trkt taa dha dha trkt dha dha tu na"
    -- palta 4
    , "dha dha trkt trkt trkt dha dha trkt dha dha tu na"
    , "taa taa trkt trkt trkt dha dha trkt dha dha tu na"
    -- palta 5
    , "trkt trkt dha dha trkt dha dha trkt dha dha tu na"
    , "trkt trkt taa taa trkt dha dha trkt dha dha tu na"
    , tri_ (dha.__8) (g "dha dha trkt dha dha tu na taa taa trkt dha dha tu na")
        -- , "dha dha trkt dha dha tu na taa taa trkt dha dha tu na"
        -- . "dha _ _ _ _ _ _ _ dha dha trkt dha dha tu na"
        -- . "taa taa trkt dha dha tu na dha _ _ _ _ _ _ _"
        -- . "dha dha trkt dha dha tu na taa taa trkt dha dha tu na"
    -- first speed
    , tri_ "dha___" (g "dha dha trkt dha dha tuna")
        -- , "dha dha trkt dha dha tu na dha _ _ _ dha dha trkt"
        -- . "dha dha tu na dha _ _ _ dha dha trkt dha dha tu na"
    ]

qaida2 :: Korvai
qaida2 = qaida $ akash $ korvaiS kehrwa $ map sd
    [ "dha trkt tk dha trkt tk taa trkt tk dha trkt tk"
    , "dha trkt tk dha trkt tk dha trkt tk tu na kt tk"
    , "taa trkt tk taa trkt tk dha trkt tk tu na kt tk"

    -- palta
    , "dha dha trkt dha trkt tk dha trkt tk tu na kt tk"
    , "taa taa trkt taa trkt tk dha trkt tk tu na kt tk"
    , "dha trkt trkt dha trkt dha trkt tk tu na kt tk"
    , "taa trkt trkt taa trkt dha trkt tk tu na kt tk"
    , "trkt trkt dha trkt tk dha trkt tk tu na kt tk"
    , "trkt trkt taa trkt tk dha trkt tk tu na kt tk"
    , "dha trkt tk _ trkt tk dha trkt tk tu na kt tk"
    , "taa trkt tk _ trkt tk dha trkt tk tu na kt tk"

    , tri_ (dha.__)
        (g $ "dha trkt tk tu na kt tk taa trkt tk tu na kt tk"
            . "dha _ kt tk")
        -- , "dha trkt tk tu na kt tk taa trkt tk tu na kt tk"
        -- . "dha _ kt tk dha _ dha trkt tk tu na kt tk taa tr"
        -- . "kt tk tu na kt tk dha _ kt tk dha _ dha trkt tk"
        -- . "tu na kt tk taa trkt tk tu na kt tk dha _ kt tk"
    ]

-- farmaisi chakradar paran
farmaisi :: Korvai
farmaisi = akash $ korvaiS1 tintal $ nadai 3 $
    r3 $ g $ r2 "dha dha gena tette"
        . "takita dha trkt" . "dha dha gena tette"
    . "kran__ kran__" . (nadai 2 "tk tr kt tk")
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

tukra6 :: Korvai
tukra6 = tukra $ akash $ korvaiS1 tintal $ nadai 3 $
    "dha _ na dhikita dha trkt dhi kita"
    . "ka tette tukita dhi gene nagene"
    . "takita taa_na taa__" . tri_ "dha__" "kredhadha"

awantika :: Korvai
awantika = akash $ comment "Akash did for kathak dance with Awantika." $
    korvaiS1 tintal $ sd $ -- TODO should not be sd but wraps better that way
    r3 (g ("dha dha ".tkttktdhom))
        . __ . r3 (g ("dha ti dha ".tkttktdhom))
        . __ . r3 (g ("dha dha ti dha ".tkttktdhom))
    where
    tkttktdhom = tkt.tkt.dhom
    tkt = su "takita"
