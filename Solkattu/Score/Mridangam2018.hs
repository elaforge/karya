-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Solkattu.Score.Mridangam2018 where
import Prelude hiding ((.), repeat)

import qualified Solkattu.Tala as Tala

import Solkattu.Dsl.Mridangam


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

tir_18_05_25 :: Korvai
tir_18_05_25 = tirmanam $ date 2018 5 25 $ ganesh $ korvaiS1 Tala.misra_chapu $
    __sam Tala.misra_chapu $
        kt.__.k.__.k.__.o.__.o.k.o . tri_ (od.__.p.k.t.k.o) (o.k.o.o.k)

tir_18_06_15 :: Korvai
tir_18_06_15 = tirmanam $ date 2018 6 15 $ korvai Tala.misra_chapu $
    variations
    -- 42424 as 41414
    [ sd $ tri_ od (k.o.o.k)
    , tri_ (od.__) (k.o.o.k . o.__.k.__)
    , tri_ (od.__) (k.__.o.od.__.k.__.k)
    , tri_ (od.__) (t.k.o.o.k.o.o.k)
    , tri_ (od.__) (o.k.o.k.o.u.__.k)

    -- 3x + 2y where x+y = 7, x ends on sam, y = 7 - dur x
    , restD 4   . sd (tri_ __ (on.on.k.on.on.k))
    , restD 4.5 . sd (tri_ (od.__) (k.t.k.n.o))
    , restD 5   . sd (tri_ (od.__3) (k.o.o.k))
    , restD 5.5 . sd (tri_ (od.__4) (on.on.k))
    -- with fancy takadinna
    , tri_ (od.__) p4
    , tri2 (od.__) p4 (od.__)

    , restD 6 . tri_ (od.__.pk) p4
    , restD 6 . tri2 (od.__.pk) p4 (od.__.o)

    , restD 5  . tri_ (od.__.pk.t.k) p4
    , restD 5  . tri2 (od.__.pk.t.k) p4 (od.__4)
    ]
    where
    tri2 karv seq karv2 = seq . karv . seq . karv2
        . rtakeM 3 seq . karv2 . rtakeM 3 seq
    p4 = group $ t.k.o.od.__.on.__.k

-- din nakitataka at 140

e_18_06_22 :: Korvai
e_18_06_22 = exercise $ date 2018 6 22 $ korvai adi $ variations
    [ make (su2 (kt.kt.k.o.o.k))
    , make (su2 (kook.o.k.k.__))
    , make9 (su2 (k.kook.o.k.k.__))
    , make9 (su2 (k.__.o.o.k.o.k.k.__))
    , make9 (su2 (k.__.o.o.k.__.o.o.k))
    -- TODO then 1 2 3 variations

    , make3 (k.o.o.k.o.o.k)
    , make123 (k . repeat 3 (o.o.k))

    , make3 (o.n. su (kt.o.k) . on . su (kt.o.k))
    , make123 (o.n. su (kt.o.k) . repeat 2 (on . su (kt.o.k)))
    , make3 (k.__.t.__.k.n.o)
    ]
    where
    make p =
          sd (on.od.od.on) . p . sd (od.od.on)
          . sd (on.d.d.n) . tri_ od p
        . sd (on.od.od.on) . p . sd (od.od.on)
          . sd (on.d.d.n) . p.od.p.su od . rtakeM 1 p . su od
                                         . rtakeM 1 p

    -- TODO I need some kind of start offset to express this naturally
    make9 p =
          sd (on.od.od) . shim on . p . sd (od.od.on)
          . sd (on.d.d) . shim n . tri_ od3 p
        . sd (on.od.od) . shim on . p . sd (od.od.on)
          . sd (on.d.d) . shim n . p.od3.p.su od . rtakeM 1 p . su od
                                                 . rtakeM 1 p
        where
        shim s = s . su2 __4
        od3 = su od . su2 __

    make3 p = nadai 6 $
          s3 (on.od.od) . shim on . su p . s3 (od.od.on)
        . s3 (on.d.d) . shim n . tri_ od (su p)
        where
        s3 = spread 3
        shim s = s.__.su __

    -- 123 variant for tisram
    make123 p123 = nadai 6 $
          s3 (on.od.od) . shim on . su (takeM 7 p123) . s3 (od.od.on)
        . s3 (on.d.d) . shim n . expand
        where
        s3 = spread 3
        shim s = s.__.su __
        expand = join od
            [ takeM 2 (su p123)
            , takeM 3.5 (su p123)
            , su p123
            ]

c_18_07_02_sarva :: Korvai
c_18_07_02_sarva = sudhindra $ korvai Tala.misra_chapu $ variations
    [ sd $ sarva & (o .__.o.o.o.__4.o.__.o.o.o.__)
         . sarva & (__.__.o.o.o.__4.o.__.o.o.o.__)
         . sarva & (o.__n 14)
         . sarva
    ]
    where sarva = n.k.n.n.d.__4.n.k.n.n.d.__

e_misra_tisram :: Korvai
e_misra_tisram = exercise $ date 2018 9 11 $ ganesh $
    korvai Tala.misra_chapu $ variations $ map sd $ map (nadai 6)
    [ repeat 5 (kt.k) . tk.kook
    , repeat 5 ndk . n.k.n.n.d.k
    , (o.__3.p) & repeat 5 ndk . n.k.on.on.od.k
    , o & ndk . n.k.on.on.od.k . (o.__3.p) & repeat 2 ndk . n.k.on.on.od.k
    , o & ndk . repeat 3 (n.k.on.on.od.k)
    , on.od.k . repeat 3 (on.k.on.on.od.k)
    , on.od.k . repeat 2 (on.k.on.on.od.k) . o&nang_kita
    , o & repeat 5 ndk . nang_kita
    , o & ndk . nang_kita . o & repeat 2 ndk . nang_kita
    , o & ndk . repeat 3 nang_kita
    , su (n.p.kt.pk) . repeat 3 nang_kita
    , su (n.p.kt.pk) . nang_kita . repeat 2 (su (n.p.kt.pk)) . nang_kita
    ]
    where
    nang_kita = n . su (pk.nakatiku)
    ndk = n.d.k

e_18_11_12 :: Korvai
e_18_11_12 = exercise $ date 2018 11 12 $ ganesh $ korvai Tala.misra_chapu $
    variations
    [ kook.sarvaD_ 6 . kook.sarvaD_ 6
    , pattern1 $ kook.od.__4 . kook.od.__4
    , pattern1 $ repeat 2 (kook.o.__.k.__)
    , pattern1 $ repeat 2 (kook.o.k.o.k)
    , pattern1 $ repeat 2 (o.k.o.k.kook)
    , pattern1 $ repeat 3 okko . kook
    , pattern1 $ repeat 2 okko . repeat 2 kook
    , pattern1 $ kook.okko.okko.kook
    , pattern1 $ repeat 2 (k.o.o.k.o.o.__.k)
    , pattern1 $ k.o.o.k.o.o.__.k.o.o.__.k.o.o.__.k
    , pattern1 $ o.k.o.k.kook.o.o.__.k.o.o.__.k
    , pattern1 $ okko.o.o.__.k.kook.kook
    , pattern1 $ okko.o.o.__.k.kook.o.k.o.k
    ]
    where
    pattern1 mid = kook.sarvaD_ 2 . mid . kook.sarvaD_ 6
    okko = o.k.k.o

e_18_11_19 :: Korvai
e_18_11_19 = sarvalaghu $ date 2018 11 19 $ ganesh $ korvai adi $ variations
    [ repeat 4 $ n.l.d.l.n.d.l.d
    , repeat 4 $ n.l.d.l.n.d.su (p.l).d
    , repeat 4 $ n.l.d.l.su (n.y.p.l).d.su (p.l)
    , repeat 4 $ n.l.d.su (p.l.n.y.p.l).d.su (p.l)
    , repeat 2 $ su $ repeat 3 (n.y.p.l.d.__.p.l) . (n.y.p.l.d.__.j.y)
    , repeat 4 $ su $ d.__.p.y.j.l.d.__.p.l.d.__.n.y.p.l
    , repeat 4 $ su $ d.__.p.y.j.l.d.__.p.k.t.k.n.y.p.l
    , repeat 4 $ su $ d.__.p.y.j.l.d.__.p.k.d.y.j.l.p.l
    ]

e_18_12_08 :: Korvai
e_18_12_08 = exercise $ date 2018 12 8 $ ganesh $ korvai adi $ variations
    [ repeat 3 (p.k.p.k.n.o.o.k) . end
    , repeat 3 (su (p.t.p.k.p.t.p.k).n.o.o.k) . end
    , repeat 3 (su (p.k.t.k.p.k.t.k).n.o.o.k) . end
    , repeat 3 (su (p.k.t.k).p.k . n.o.o.k) . end
    ]
    where end = n.p.k.__.p.u.__.k

e_18_12_08_b :: Korvai
e_18_12_08_b = exercise $ date 2018 12 8 $ ganesh $ korvai adi $ variations
    [nadai 6 $ a.b.a.b.a.b.a]
    where
    a = group $ n.p.k.t.p.k.t.p.k.t.p.k
    b = n.p.k.__.u.__.p.k.nakatiku
