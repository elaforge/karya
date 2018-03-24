-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Solkattu.Score.Mridangam2018 where
import Prelude hiding ((.), repeat)

import Solkattu.MridangamGlobal


e_18_03_10 :: Korvai
e_18_03_10 = exercise $ date 2018 3 10 $ korvaiS adi
    [ sarva1 . sarva2
    , (on.kt.kt.k) `replaceStart` sarva1 . sarva2
    , (on.kt.kt.k) `replaceStart` sarva1 . (n.kt.kt.k) `replaceStart` sarva2
    ]
    where
    sarva1 = sd $ on.d.d.on.d.pn.d.d
    sarva2 = sd $ n .d.d.on.d.on.d.d
    pn = p&n
