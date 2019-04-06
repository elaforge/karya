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
