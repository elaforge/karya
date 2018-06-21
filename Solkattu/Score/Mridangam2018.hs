-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Solkattu.Score.Mridangam2018 where
import Prelude hiding ((.), repeat)

import Solkattu.MridangamGlobal
import qualified Solkattu.Tala as Tala


e_323_1 :: Korvai
e_323_1 = exercise $ date 2018 3 19 $ ganesh $
    comment "All these also work with 332 of course." $ korvai adi $ variations
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
e_323_2 = exercise $ date 2018 4 13 $ ganesh $ korvai adi $ variations
    [ repeat 2 $ rh & lh1
    , repeat 2 $ rh & lh2
    ]
    where
    rh = repeat 2 $ n.k.__.k.t.k.__.k
    lh1 = sd $ o.o.o.p.__.o.o.o
    lh2 = o.__.o.o.__3.p.__4.o.o.__3.o.__

e_18_03_19 :: Korvai
e_18_03_19 = exercise $ date 2018 3 19 $ ganesh $ korvai adi $ variations
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
e_18_05_25 = exercise $ date 2018 5 25 $ ganesh $ korvai adi $ variations
    [ repeat 3 ktkt . k.o.o.k . repeat 2 (od.o.o.u.__.o.o.k)
    , repeat 2 ktkt . k.k.t.k . k.o.o.k . repeat 2 (od.o.o.u.__.o.o.k)
    , repeat 2 ktkt . k.k.t.k . k.o.o.k . od.o.o.u.__.o.o.u.__.o.o.u.__.o.o.k
    ]

tir_18_05_25 = tirmanam $ date 2018 5 25 $ ganesh $ korvaiS1 Tala.misra_chapu $
    __sam Tala.misra_chapu $
        kt.__.k.__.k.__.o.__.o.k.o . tri_ (od.__.p.k.t.k.o) (o.k.o.o.k)

tir_18_06_15 = tirmanam $ date 2018 6 15 $ korvai Tala.misra_chapu $
    variations $ map sd
    -- 42424 as 41414
    [ tri_ o (k.o.o.k)
    , tri_ o (su (k.o.o.k) . o . k)
    , tri_ od (su (k.__.o.od.__.k.__.k))
    , tri_ od (su (t.k.o.o.k.o.o.k))
    , tri_ od (su (o.k.o.k.o.u.__.k))
    -- 3x + 2y where x+y = 7, x ends on sam, y = 7 - dur x
    , restD 2    . tri_ __ (on.on.k.on.on.k)
    , restD 2.25 . tri_ (od.__) (k.t.k.n.o)
    , restD 2.5  . tri_ (od.__3) (k.o.o.k)
    , restD 2.75 . tri_ (od.__4) (on.on.k)
    -- with fancy takadinna
    , tri_ od takadinna
    , tri2 od takadinna od
    --
    , restD 3 . tri_ (od.su pk) takadinna
    , restD 3 . tri2 (od.su pk) takadinna (od.su o)
    --
    , restD 2.5  . tri_ (od.su (pk.t.k)) takadinna
    , restD 2.5  . tri2 (od.su (pk.t.k)) takadinna (od.__)
    ]
    where
    tri2 karv seq karv2 = seq . karv . seq . karv2
        . rtakeM 1.5 seq . karv2 . rtakeM 1.5 seq
    takadinna = group $ su $ t.k.o.od.__.on.__.k

-- din nakitataka at 140
