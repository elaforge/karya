-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
module Solkattu.Score.TablaQaida where
import           Prelude hiding ((.))

import           Global
import           Solkattu.Dsl.Bol


qaida1 :: Korvai
qaida1 = qaida $ akash $ korvaiS adi $ map sd
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
qaida2 = qaida $ akash $ korvaiS adi $ map sd
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
