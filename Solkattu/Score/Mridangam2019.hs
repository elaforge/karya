-- Copyright 2019 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Solkattu.Score.Mridangam2019 where
import Prelude hiding ((.), repeat)
import Solkattu.Dsl.Mridangam


e_naka :: Korvai
e_naka = sarvalaghu $ ganesh $ date 2019 3 14 $ korvai1 adi $ section $
    n.k.o.od.__.k.on.k . o & repeat 3 (n.k.p.d.__.k.n.k)

e_19_03_20 :: Korvai
e_19_03_20 = ganesh $ date 2019 3 20 $ sarvalaghu $ korvaiS adi $
    [ t1 . tri t2
    , t1 . tri t2_tis
    -- 4 on 5, frequently done by Raghu Sir.
    , nadinga 3
    , nadai 5 $ nadinga 4
    , nadai 6 $ nadinga 5
    -- chatusram to kandam: +4 beats at the end so 16 -> 20.
    , nadai 5 $ repeat 2 $ t1 . t2 . od.__.k.__
    ]
    where
    t1 = on.__3.k.n.o.o.k
    t2 = on.t.k.p&k.n.o.o.k
    t2_tis = nadai 6 (on.k.on.k.k.p&k) .n.o.o.k
    nadinga times =
        repeat times (n.d.__.k) . o.od.__.k
        . on.od.__.k . repeat (times-1) (n.d.__.k) . o.od.__.k

e_19_04_01 :: Korvai
e_19_04_01 = sarvalaghu $ ganesh $ date 2019 4 1 $ korvai adi $ map section
    [ sd (on.od.od).on.ookook . sd (od.od.on)
        . sd (on.d.d).n.ookook . sd (od.od.on)
    ] ++ map (startOn (1/2) • endOn (1/2) • section)
    [ initial . ookook . sd (od.od).on.ookook
    , initial . tri_ od ookook
    , initial . tri_ __ ookook
    , initial . tri123 od ook
    , initial . tri123 __ ook
    , initial . repeat 3 ook.su k.repeat 4 ook
    , initial . repeat 3 ook.su __.repeat 4 ook
    -- 4+5+6+7
    -- because: 62626 = 22 -> 2*11 -> 2 * (5+6) -> 4+7 + 5+6 -> 4567
    , initial . su (o.o.k.__ .o.o.__.k.__ .o.__.o.__.k.__ . o.o.__.o.__.k.__)

    , initial6 . tri_ od ook3
    , initial6 . tri_ (nadai 6 ook) ook3
    -- 939393 = 33, so 3*11 -> 345678
    , initial6 . nadai 6 (su (o.o.k .o.o.k.__ .o.o.__.k.__ .o.__.o.__.k.__
        .o.o.__.o.__.k.__ .o.__.o.__.o.__.k.__))
    ]
    where
    ookook = su (o.o.k.o.o.k)
    initial = sd (od.od).on.ookook . sd (od.od).on.ookook
        . sd (od.od).on
    initial6 = sd (od.od).on.ook3 . sd (od.od).on. ook3
        . sd (od.od).on
    ook3 = nadai 6 (tri ook)
    ook = su $ o.o.k
    -- Quick way to get arithmetic progressions:
    -- Factor into low and high.  The low factor is the number of pairs,
    -- (floor (high/2), ceiling (high/2)) is the mid pair, and expand from
    -- there.

e_19_04_15 :: Korvai
e_19_04_15 = date 2019 4 15 $ ganesh $ exercise $
    comment "practice left hand independance" $
    korvaiS adi
    [ rh
    , rh & repeat 4 (o.__4)
    , rh & repeat 7 (o.__)
    , rh & (o.o.__3.repeat 5 (o.__))
    , rh & (o.o.__3.o.o.__3.repeat 3 (o.__))
    , rh & strM "o_oo_oo_oo_oo"
    ]
    where
    rh = repeat 8 $ n.k.t.k

e_19_05_06_a :: Korvai
e_19_05_06_a = date 2019 5 6 $ ganesh $ comment "leads to e_19_05_06" $
    korvaiS adi $ map (nadai 6)
    [ group t1 . alt t1 . tri (p.n.y.d)
    , group t1 . repeat 3 (alt t1)
        . repeat 3 (alt (o.n.y.od.__.k.od.__))
        . repeat 3 (alt (o.n.y.od.__.k))
        . repeat 3 (alt (o.n.y.d))
    ]
    where
    t1 = o.n.y.od.__.k.od.__.on.y.d.__
    alt seq = group seq . group (replaceStart p (noThom seq))

e_19_05_06_b :: Korvai
e_19_05_06_b = date 2019 5 6 $ ganesh $ comment "tisram, built on 7s and 5s" $
    korvaiS adi $ map (nadai 6)
    [ p1 . p2 . p3 . p4
    , repeat 3 p1 . a7.a5
      . p1 . a7.a5 . p3 . a7.a5
      . p1 . tri a7 . tri a5
    , repeat 3 p1 . b7.b5
      . p1' . b7.b5 . p1' . b7.b5
      . p1' . tri b7 . tri b5
    , t7.t5e.od.__3 . p3_.t5e.od.__3
        . t7.t5e . p3_.t5e.od.__3
        . tri_ (i.__3) (p5.__.p5)
    ]
    where
    p1 = t7.t5
    p1' = o.__.t.__.k.n.__ . t5
    p2 = __.__.t.__.k.n.__.t5
    p3 = p3_.t5
    p3_ = k.p.t.__.k.n.__
    p4 = su (k.t.p.k) . t.__.k.n.__.t5

    a7 = group $ kp.p5
    a5 = group $ p5
    b7 = group $ o.k.o.k.o.o.k
    b5 = group $ o.k.o.o.k
    t7 = k.__.t.__.k.n.__
    t5 = k.__.k.n.__
    t5e = k.__.k.n.o

-- TODO I already did this one, find it
e_19_05_20a :: Korvai
e_19_05_20a = make_nakadit_talang_ga (od.su (pk.n.o))

e_19_05_20b :: Korvai
e_19_05_20b = make_nakadit_talang_ga (o.su (k.o.k.o))

e_19_05_20b2 :: Korvai
e_19_05_20b2 = date 2019 5 20 $ ganesh $ korvaiS adi
    [ sarva 8
    , t1 . sarva 7
    , t1 . sarva 3 . t1 . sarva 3
    , t1 . sarva 1 . t1 . sarva 1 . t1 . sarva 3
    , t1 . sarva 1 . t2 . sarva 1 . t2 . sarva 3
    , o_kokook . ookokook . t1 . sarva 1 . t1 . sarva 3
    , o_kokook . repeat 3 ookokook . t1 . sarva 3
    , o_kokook . repeat 4 ookokook . repeat 4 (su $ group $ o.k.o.k.k.o)
    ]
    where
    ookokook = group $ su $ o.o.k.o.k.o.o.k
    o_kokook = group $ su $ o.__.k.o.k.o.o.k
    t1_ = o.su (k.o.k.o)
    t2 = group $ su (o.o.k.o.k.o) . od
    t1 = group $ t1_ . od
    sarva = sarvaD sarvaStrokes
    sarvaStrokes = let lh = d.__.n.d.l.d.n.l . d.l.n.d.l.d.n.l
        in lh & strM "o_oo_oo_o_oo_oo_" . lh & o

e_19_05_20c :: Korvai
e_19_05_20c = make_nakadit_talang_ga (su $ o.y.j.y.on.__)

make_nakadit_talang_ga :: Sequence -> Korvai
make_nakadit_talang_ga t1_ = date 2019 5 20 $ ganesh $ korvaiS adi
    [ sarva 8
    , t1 . sarva 7
    , t1 . sarva 3 . t1 . sarva 3
    , t1 . sarva 1 . t1 . sarva 1 . t1 . sarva 3
    , tri (t1.__) . k.t1 . sarva 3
    , t1.__.k .t1.__.t1.__ .t1 . sarva 3
    , tri $ tri (t1.__) . tri_ __ p5
    , t1 . sarva 3 . t1'.t1 . sarva 2.25
    , repeat 2 $ t1'.t1 . sarva 2.25
    , t1'.t1.k.t1 . sarva 1 . t1 . sarva 3
    , t1x3 . talang 1 . t1 . sarva 3
    , trin (od.__) (t1x3 . talang 1) (t1x3 . talang 2) (t1x3 . talang 3)
    ]
    where
    t1 = group $ t1_ . od
    t1' = group t1_
    t1x3 = t1'.t1.k.t1'
    talang times = group $ su $ o.k . repeat times (n.p.k.__.k.u.__.k)
    sarva = sarvaD sarvaStrokes
    sarvaStrokes = let lh = d.__.n.d.l.d.n.l . d.l.n.d.l.d.n.l
        in lh & strM "o_oo_oo_o_oo_oo_" . lh & o

e_5x4_4x3 :: Korvai
e_5x4_4x3 = date 2019 6 3 $ exercise $ ganesh $ korvaiS adi
    [ make (su (o.__.k.o.k.o.o.k)) (su (o.o.k.o.k.o.o.k)) (su (o.k.o.k.k.o))
    , make1  (on.l.d.od.l.d.od.l) (on.l.d.od.l.d)
    , let nd_kpd = on.d . su kp . d in
        make (on.__ . su (on.y.j.y) . nd_kpd)
             (su (on.y.j.y.oj.y.j.y). nd_kpd)
                      (su (on.y.j.y). nd_kpd)
    , make1 (su $ d.__.p.y.j.y.d.__.p.k.d.__.j.y.j.y)
        (su $ repeat 2 $ d.__.p.y.j.y)
    , make (strM "n_ndnynd" & strM "o_oo_oo_")
        (strM "nyndnynd" & strM "o_oo_oo_") (on.y.on.on.od.l)
    , make (strM "n_ndnynd" & strM "o_oo_oo_")
        (strM "nyndnynd" & strM "oooo_oo_") (on.y.on.on.od.l)
    ]
    where
    make1 t4 t3 = make t4 t4 t3
    make t4a t4 t3 = group t4a . repeat 4 (group t4) . repeat 4 (group t3)
    oj = o&j

-- ktkto_
-- repeat 3 ktkto_ . repeat 3 ktpkpktkno

e_19_06_10a :: Korvai
e_19_06_10a = date 2019 6 10 $ ganesh $ exercise $ korvaiS adi
    [ repeat 8 t1
    , repeat 4 t1 . t1 . repeat 4 t2
    , repeat 4 t1 . t1 . repeat 3 t2 . repeat 2 t3
    , t1 . repeat 4 t2 . t1 . repeat 3 t2 . repeat 2 t3
    , repeat 2 $ repeat 5 t4 . repeat 4 t3
    , repeat 5 t4 . tri_ (od.__4) (repeat 4 t3)
    ]
    where
    t1_ = su $ on.__.o.k.od.__.pk.n.o.o.k.od.__.o.k
    t1 = group t1_
    t2 = takeM 6 $ t1
    t3 = su $ group $ p.k.n.o.o.k
    t4 = su $ group $ on.__.pk.n.o.o.k

e_19_06_10b :: Korvai
e_19_06_10b = date 2019 6 10 $ ganesh $ exercise $ korvaiS adi $ map su
    [ (rh.rh') & lh . rh' . rh' . repeat 4 rh'
    , (rh.rh') & lh . repeat 4 (group (on.__.l.d.__.l.n.p.l.d.__.l))
    , x5x4 (n.p.l.d.__.l.d.__) (n.p.l.d.__.l)
    , x5x4 (n.__.d.__.p.l.d.__) (n.__.d.__.p.l)
    , x5x4 (n.__.d.__.p.y.j.y) (n.__.d.__.p.l)
    , x5x4 (n.y.d.__.p.y.j.y) (n.y.d.__.p.y)
    ]
    where
    x5x4 t1 t2 = repeat 5 t1 . repeat 4 (group t2)
    rh = n.__.l.d.__.l.d.__
    rh' = n.__.l.d.p.l.d.__
    lh = strM "o__o__o_o"

e_19_06_17 :: Korvai
e_19_06_17 = date 2019 6 17 $ ganesh $ exercise $ korvaiS adi
    [ t1 (p.n.y.d.__) (on.y.d.__)
    , t1 (p.k.n.y.d) (p.k.n.y)
    , nadai 6 $
        let rh = __.n.y.d.__.l.d.__.n.y.d.__
            lh = strM "o__o__o_o__"
        in lh & rh . lh & rh . p & rh . p & rh
            . lh & rh . lh & rh . p & rh . tri (group (o.n.y.d))
    ]
    where
    t1 end1 end2 = su $
        repeat 5 (group (o.n.y.od.__.l.od.__.on.y.d.end1))
        . repeat 4 (group (o.n.y.od.__.l.od.__.end2))

c_19_06_24_a :: Korvai
c_19_06_24_a = date 2019 6 24 $ ganesh $
    similarTo "Mridangam2019" "c_19_06_24_b"  $ korvai adi
    [ x2 $ section $ sarvaD_ 6 . su t12
    , section $ n6 (d.__8 . t12).d.__8 . su t12
    , section $ n6 (d.__8.t12).d.__4 . su t1 . n6 (d.__4.t1)
    , section $ repeat 2 $ d.__4.su t1 . n6 (d.__4.t1)
    , section $ n6 $ d.__4.t1.d.__4 . t1 . tri t1
    , ending $
        -- TODO I should be able to do this variation automatically with a
        -- replace, see below.
        -- p&k.__.u . su (pk.nakatiku).o.__.k.__
        p&k.__.u . su (pk.nakatiku).su (o.__3.o.k.__.o.k)
        -- . n6 (od.__8.o&n.__.pk.nakatiku.o.__4.k.__4.od.__)
        . n6 (od.__8.o&n.__.pk.nakatiku.o.__3.o.k.__.o.k.od.__)
        -- . __5 . su nakatiku.o.__.k.__.od.__4
        . __5 . su nakatiku.su (o.__3.o.k.__.o.k).od.__4
        -- . n6 (__.__.o.__4.k.__4.od.__8)
        . n6 (__.__.o.__3.o.k.__.o.k.od.__8)

        . group (concatMap (\s -> s.__3.s) ktkno)
        . n6 (group (concatMap (\s -> s.__.s) ktkno))
        . n6 (group (concatMap (.__) ktkno))
        -- . n6 (tri_ __ (tri (group (mconcat ktkno))))
        -- . n6 (tri_ __ (trin ø (k.n.o) (k.t.k.n.o) (i.__4.k.n.o)))
        . n6 (trin __ (tri (k.n.o)) (tri (k.t.k.n.o)) (tri (i.__4.k.n.o)))
    ]
    where
    -- TODO replace (o.__.k.__) with (su (o.__3.o.k.__.o.k))
    -- I might have to tag the sequence, because (k.__.o.__) isn't unique and
    -- I'm not using sollus.
    n6 = nadai 6
    ktkno = [k, t, k, n, o]
    t12 = t1 . nakatiku
    t1 = group $ o'.__.kt.kt.pk

-- TODO reduce copy-paste with c_19_06_24_a
c_19_06_24_b :: Korvai
c_19_06_24_b = date 2019 6 24 $ ganesh $
    similarTo "Mridangam2019" "c_19_06_24_a" $ korvai adi
    [ section $ sarvaD_ 6 . t12
    , x2 $ section $ n3 (od.__4 . t12) . n4 (od.__8 . t12)
    , section $ n3 (od.__4 . t12) . n4 (od.__4 . t2) . n3 (od.__.t2)
    , section $ repeat 2 $ od.__4.t2 . n3 (od.__.t2)
    , section $ n3 $ od.__.t2.od.__.t2 . tri t2
    , ending $
        t12.o.__.k.__ . n3 (od.__4.t.k.k.o.o.k.o.__.k.__.od)
        . n4 (__5.t2.o.__.k.__) . n3 (od.__4.o.__.k.__.od.__4)

        . group (concatMap (\s -> s.__3.s) ktkno)
        . n6 (group (concatMap (\s -> s.__.s) ktkno))
        . n6 (group (concatMap (.__) ktkno))
        . n6 (tri_ __ (tri (group (mconcat ktkno))))
    ]
    where
    n3 = nadai 3
    n4 = nadai 4
    n6 = nadai 6
    ktkno = [k, t, k, n, o]
    t12 = group $ k.k.t.k.k.o.o.k
    t2 = group $ k.o.o.k

e_19_06_24 :: Korvai
e_19_06_24 = date 2019 6 24 $ ganesh $ korvaiS adi $ map su
    [ repeat 8 t12
    , repeat 6 t12 . t1.t1.t2.t2
    , repeat 5 t12 . t1.t1.t1.t2.t2.t2
    , repeat 5 t12 . od.__.t2.k.od.__.t2.p.k.od.__.t2
    , repeat 5 t12 . od.__.k.od.__.p.k.od.__.t2.t2.t2
    ]
    where
    t12 = t1.t2
    t1 = group $ k.od.__
    t2 = group ktkno

-- chatusram / tisram exercise:
-- nadai 4 (repeat 2 (k.od.__.ktkno)) . nadai 6 (repeat 3 (k.od.__.ktkno))

e_19_08_05_gumiki :: Korvai
e_19_08_05_gumiki = date 2019 8 5 $ ganesh $ exercise $ korvaiS adi
    [ su $ repeat 2 $
          o'.__.kt.kt.p.k.n.n.o'.__.d.__4
         .__.__.kt.kt.p.k.n.n.o'.__.d.__4
    , su $ repeat 2 $
          o'.__.kt.kt.p.k . repeat 3 (n.n.o'.__.d.__4)
    , su $ repeat 2 $
          o'.__.kt.kt.p.k . repeat 5 (n.n.o'.__).d.__4
    ]

-- search youtube:
-- bhajan govindam - ms
-- d_nd_dn_ for verse with nd_nd_nd for end
--
-- hanuman charisa - ms

-- gumiki exercise: nd_n_nd_n_nd_n ktok, but with o' on long o

-- sarva:
-- n-dd-dd-n-dd-dd-
-- d_nd-dn-d-nd-dn-
-- nd-n-nd-n-nd-nd-
-- nd-dnd-dnd-dnd-d
e_19_08_19 :: Korvai
e_19_08_19 = date 2019 8 25 $ ganesh $ exercise $ korvaiS1 adi $
    concatMap cycle [p, k, o, n]
    where cycle x = su (x.__.kt.kt.p.k.nakatiku) . sarvaD_ 2

-- tirmanam: td--t-d-- (4+5) * 3
-- tisram: c_19_06_24_a
-- ending 3x

c_19_08_26 :: Korvai
c_19_08_26 = date 2019 8 26 $ ganesh $ korvaiS adi $ map (nadai 6)
    [ restD 6 . tri (k.od.__.ktkno)
    , rh o & lh . rh p & lh . rh p . rh o
    , rh o & lh . rh p & lh . t7 p . t7 p . t5 p . t5 o
    , rh o & lh . t7 __ & strM "o_o_oo" . t7 __ & strM "o_o_oo"
        . t7 p . t5 p . t5 p . t5 o
    , rh o & lh . rh p & lh . rh p . t75
    , rh o & lh . rh p & lh . rh p . t75'
    , rh o & lh . t75 . rh p . t75'

    , t75.od.__3 . t75'.od.__3
        . t75 . t75' . od.__3
        . tri_ (i.__3) (p5.__.p5)
    ]
    where
    t7 e = n.__.d.__.n.d.e
    t5 e = n.__.d.__.e
    t75 = group $ (k.__.t.__.k.n.__) . (k.__.k.n.__)
    t75' = group $ (k.p.t.__.k.n.__) . (k.__.k.n.__)

    rh e = t7 __ . t5 e
    lh   = o.__.o.__.o.o.__.o.__.o.__.__

e_19_09_23 :: Korvai
e_19_09_23 = date 2019 9 23 $ ganesh $ exercise $ korvaiS1 adi $
    repeat 2 $ repeat 4 rh
        & (o'.__.o.o'.__.__.p_.__.__.__.o.o'.__.__.o.__)
    where
    rh = d.l'.__.l'
    -- rh2 = d.l'.lt d.l'

-- in kandam, in tisram
c_19_09_23 :: Korvai
c_19_09_23 = date 2019 9 23 $ ganesh $ trikalam $
    similarTo "Solkattu2017" "c_17_07_13" $ korvaiS adi
    [ repeat 2 purvangam, utarangam
    , nadai 5 $ repeat 3 purvangam . utarangam
    , nadai 6 $ repeat 4 purvangam . utarangam
    ]
    where
    purvangam = v.__4.k.o.od.__.on.__.on.__.od.__
        . su ktok . o.k.o.k.k.o.od.__.on.__.on.__.v.__4
    utarangam = tri_ (o.__) (group (v.__3.k.k.o.o.k.o.k.o.k.k.o.o.k.o.__.k.__))

e_19_09_30_gumiki :: Korvai
e_19_09_30_gumiki = date 2019 9 30 $ ganesh $ exercise $ korvaiS adi
    [ repeat 2 (o'.__.kt.kt.pk.nakatiku)
        . repeat 3 (o'.__.kt.kt.pk) . nakatiku
        . repeat 2 (o'.__.kt.kt) . o'.__.kt . (o'.__.kt.kt.pk.nakatiku)
        . repeat 3 (o'.__.kt.kt) . o'.__.kt.pk.nakatiku
    , sd (on.od.on.p'&d.n.od.on.d).o'.__.kt.kt.pk.nakatiku
    ]

-- Start from 70mm, aim for 90mm.
e_19_11_11_namita_dimita :: Korvai
e_19_11_11_namita_dimita = date 2019 11 11 $ ganesh $ exercise $
    similarTo "Mridangam2013" "namita_dimita_seq" $ korvaiS adi
    [ rh&lh . rh&lh . rh&o . end o
    , rh&lh . end o . rh&o . end o
    , o & end p . end o . o & end p . end o
    , invert . invert
    , tri_ (od.__.k.p.k) (o & end o . dropM 3 (end o) . dropM 3 (end o))
    ]
    where
    end s = su $ n.__.kt.o.k.o.t.o.k.on.__.kt.s.k
    invert = su $
         o.t.o.k.on.__.kt.o.k.on.__.kt.p.k
        .p.t.p.k.n .__.kt.o.k.on.__.kt.o.k
    rh = n.l.d.d.l.d.d.l
    lh = o.__3.o.__3.o.__

e_19_11_11_sarva :: Korvai
e_19_11_11_sarva = date 2019 11 11 $ ganesh $ sarvalaghu $ korvaiS1 adi $
    repeat 2 $ o.k.o.o&t.k.on.su ktpk . p.k.o.o&t.k.on.su ktok

-- revise kandom exercises from way back
