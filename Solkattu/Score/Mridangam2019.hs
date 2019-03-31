-- Copyright 2019 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Solkattu.Score.Mridangam2019 where
import Prelude hiding ((.), repeat)
import Solkattu.Dsl.Mridangam


e_naka :: Korvai
e_naka = sarvalaghu $ ganesh $ date 2019 4 14 $ korvai1 adi $ section $
    n.k.o.od.__.k.on.k . o & repeat 3 (n.k.p.d.__.k.n.k)

e_19_04_20 :: Korvai
e_19_04_20 = ganesh $ date 2019 4 20 $ sarvalaghu $ korvaiS adi $
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
