-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Solkattu.Score.Mridangam2018 where
import Prelude hiding ((.), repeat)

import Solkattu.MridangamGlobal
import qualified Solkattu.Tala as Tala


e_323_1 :: Korvai
e_323_1 = exercise $ date 2018 3 19 $ ganesh $
    comment "All these also work with 332 of course." $ korvaiS adi
    [ make (sd (n.d.d)) (sd (n.d))
    , make (n.kt.kt.k) (n.__.d.__)
    , make (n.kt.kt.k) (n.kt.k)
    , make (n.kt.kt.k) (sd (n.d))
    , make (n.k.d.__.n.k) (sd (n.d))
    , make (n.k.d.__.n.k) (n.k.d.__)
    ]
    where
    make t3 t2 = repeat 2 (t3.t2.t3) & lh
    lh = sd $ o.__.__.o.__.p.__.__ . __.__.__.o.__.o.__.__

e_323_2 :: Korvai
e_323_2 = exercise $ date 2018 4 13 $ ganesh $ korvaiS adi
    [ repeat 2 $ rh & lh1
    , repeat 2 $ rh & lh2
    ]
    where
    rh = repeat 2 $ n.k.__.k.t.k.__.k
    lh1 = sd $ o.o.o.p.__.o.o.o
    lh2 = o.__.o.o.__3.p.__4.o.o.__3.o.__

e_18_03_19 :: Korvai
e_18_03_19 = exercise $ date 2018 3 19 $ ganesh $ korvaiS adi
    [ repeat 2 (v.__.p.kt.k.d.p . su (v.p.kt.p.kt.p))
        . repeat 2 (su (v.p.kt.p.kt.p))
    , repeat 2 (d.__.p.kt.p.d.p . su (t.k.o.o.ktpk))
        . repeat 2 (su (t.k.o.o.ktpk))
    ]

e_18_03_28 :: Korvai
e_18_03_28 = exercise $ date 2018 3 27 $ ganesh $ korvaiS1 Tala.misra_chapu $
     kt.k.n.o.od.__ . k.n.p.k.od.__.k
    .kt.k.n.p.d .__ . k.n.p.k.d .__.k

e_18_05_25 :: Korvai
e_18_05_25 = exercise $ date 2018 5 25 $ ganesh $ korvaiS adi
    [ repeat 3 ktkt . k.o.o.k . repeat 2 (od.o.o.u.__.o.o.k)
    , repeat 2 ktkt . k.k.t.k . k.o.o.k . repeat 2 (od.o.o.u.__.o.o.k)
    , repeat 2 ktkt . k.k.t.k . k.o.o.k . od.o.o.u.__.o.o.u.__.o.o.u.__.o.o.k
    ]
