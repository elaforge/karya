{-# LANGUAGE RecordWildCards #-}
-- Copyright 2025 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
module Solkattu.Score.Tabla2025 where
import           Prelude hiding ((.))

import           Solkattu.Dsl.Bol


akash1 :: Korvai
akash1 = date 2025 2 25 $ korvaiV tintal tablaKinar
    -- practice alternating fingers for gege and keke
    [ sd $ kali2 0 8 "dha ge tette gege tette gege tette kitataka"
    -- practice middle finger
    , r2 $ sd "dhati dhati dhage nataa titaa kena taati taati"

    , kali2 (-4) 16 $
        "dhati dhati dhagena dhati dhagena dhati dhati"
        . "dhati dhagena dha trkt dhati dhage dhinna gene"
        -- kali
    -- trktrk practice
    , kali2 (-4) 16 $ sd $
        "dhatidha" . "trkttk trkt" . "dhati dhage dhinna gena"
        -- taka terekita -> pronounce as tak terekita
    , tihai
        ("dhatidha" . "trkttk trkt" . "dhati dhage dhinna gena"
            . r4 (g "dha trkttk trkt"))
        "dha___"
    ]
