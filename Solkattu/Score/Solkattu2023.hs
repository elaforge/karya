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
