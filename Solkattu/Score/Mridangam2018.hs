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

-- All these also work with 332 of course.
e_323s :: Korvai
e_323s = exercise $ date 2018 3 19 $ korvaiS adi
    [ make (n.kt.kt.k) (sd (n.d))
    , make (n.k.d.__.n.k) (sd (n.d))
    , make (n.k.d.__.n.k) (n.k.d.__)
    , make (n.kt.kt.k) (n.k.d.__)
    , make (n.kt.kt.k) (n.kt.k)
    ]
    where
    make t3 t2 = repeat 2 (t3.t2.t3) & lh
    lh = sd $ o.__.__.o.__.p.__.__ . __.__.__.o.__.o.__.__

e_18_03_19 :: Korvai
e_18_03_19 = exercise $ date 2018 3 19 $ korvaiS adi
    [ repeat 2 (v.__.p.kt.k.d.p . su (v.p.kt.p.kt.p))
        . repeat 2 (su (v.p.kt.p.kt.p))
    , repeat 2 (d.__.p.kt.p.d.p . su (t.k.o.o.ktpk))
        . repeat 2 (su (t.k.o.o.ktpk))
    ]
