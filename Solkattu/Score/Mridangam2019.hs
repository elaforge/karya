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
             (su (od.y.j.y.oj.y.j.y). nd_kpd)
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
