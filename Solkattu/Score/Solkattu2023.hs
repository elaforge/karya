-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Solkattu.Score.Solkattu2023 where
import           Prelude hiding ((.), (^), repeat)

import           Solkattu.Dsl.Solkattu


haripriya :: Korvai
haripriya = date 2023 3 10 $ comment "from facebook" $ korvaiS1 adi mridangam $
    su (dim.__.tarikita.taka.nakatiku.dim.__4
    . dim.__.taka.nakatiku.dim.__4
    . nakatiku.dim.__4)
    . spread 3 tdgnt . spread 2 tdgnt . tri_ __ (g (r3 tdgnt))
    where
    mridangam = makeMridangam
        [ (dim, od)
        , (taka, k.p)
        , (tarikita, "pktp")
        ]

yt_1 :: Korvai
yt_1 = date 2023 10 12 $ source "https://www.youtube.com/watch?v=Y5rZ1FchPiw" $
        korvaiS1 adi mridangam $
    seq mempty "tat_dit_tatakadinnathom_ta_thom_" . seq (tam.__) (su p6)
    . seq mempty "tat_dit_takitathom_ta_thom_" . seq (tam.__) p5
    . seq mempty "tat_dit_tathom_ta_thom_" . seq (tam.__) p7
    where
    -- He plays takatiku as kptp rather than kpnp
    seq sep theme = trin sep (g theme) (su kp.theme) (su kpnp.theme)
    mridangam = makeMridangam
        [ (tat.dit, k.t)
        , ("tatakadinna", "kkoNk")
        , (ta.thom, k.od)
        , (ta.ki.ta, k.p.k)
        , (tam, u)
        ]

-- From Randy?
colby1 :: Korvai
colby1 = date 2023 11 2 $ source "colby" $ korvaiS adi mridangam
    [ r3 theme . spread 4 tdgnt . spread 3 tdgnt . spread 2 tdgnt
        . r3 tdgnt
    , nadai 3 $ r3 theme . spread 4 tdgnt . spread 3 tdgnt . spread 2 tdgnt
        . r3 tdgnt
    ]
    where
    theme = g "dinna_thom_dinna_thom_ta_"
    mridangam = makeMridangam [(theme, "Dn_D_kN_D_u_")]
