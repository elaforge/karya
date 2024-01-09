-- Copyright 2024 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
module Solkattu.Score.Solkattu2024 where
import           Prelude hiding ((.), (^), repeat)

import qualified Solkattu.Score.Solkattu2018 as Solkattu2018
import qualified Solkattu.Score.SolkattuMohra as SolkattuMohra

import           Solkattu.Dsl.Solkattu


t0 = realizeScoreKon eureka_tani

eureka_tani :: Score
eureka_tani = tani
    [ Comment "farans"
    , K farans
    , Comment "mohra"
    , K $ index 0 SolkattuMohra.c_mohra
    , Comment "muktayi korvai"
    , K $ index 0 Solkattu2018.adi_muktayi
    -- variations:
    -- . final 3 tat_di_tdgnt can be 1 in chatusram, 3 in tisram
    -- . 666 666 666 -> 777 666 555, or 765 765 765
    ]

-- 2024-01-08
farans :: Korvai
farans = date 2024 1 8 $ korvai adi mempty $ map (fmap su)
    [ s $ r2 (sd "tang_gu" . tari.kita.taka) . sd "din_tat_"
      . r2 (sd "din_gu" . tari.kita.taka) . sd "din_tat_"
      . r2 (sd "tang_gu" . tari.kita.taka) . sd "din_tat_"
      . r2 (sd "din_gu" . tari.kita.taka) . nakatiku
    , s $ r2 "tam_takatat_din_kitataka" . nakatiku
        . r2 ("tang_kita".nakatiku) . nakatiku
    , s $ r2 (g "talang_gudin_" . tari.kita.taka) . nakatiku
        . r3 (g "talang_gudin_") . tari.kita.taka.nakatiku
    , x2 $ s $ r2 (g (na.kita.r3 takita)) . nakatiku
        . r2 (g (r2 takita.na.kita.takita)) . nakatiku
    , s $ r2 (taka.dugu.kita.taka.nakatiku) . r3 (taka.dugu.kita.taka).nakatiku
        . r5 (g (taka.dugu)).kita.taka.nakatiku
        . r5 (g (taka.dugu)).tat.__.taka.nakatiku
    , s $ r3 (r2 (tang.__.kita.nakatiku).nakatiku) . tri_ (dheem.__4) nakatiku
    ]
