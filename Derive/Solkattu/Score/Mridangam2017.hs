-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Solkattu.Score.Mridangam2017 where
import Prelude hiding ((.), repeat)

import Derive.Solkattu.MridangamGlobal


c_17_07_10 :: Korvai
c_17_07_10 = exercise $ korvai1 adi $ su $
    o.__.k.o.k.o.o.k . repeat 4 (o.o.k.o.k.o.o.k) . repeat 4 (o.k.k.o.o.k)



c_17_08_13 = korvai1 adi $ su $ mconcat
    [ k.__.__.k.t.k.o.o.k.p.k.od.__.k.od.__.k.__.od.__3
    ,       p.k.t.k.o.o.k.p.k.od.__.k.od.__.k.__.od.__3
    ,           k.t.o.o.k.p.k.od.__.k.od.__.k.__.od.__3
    ,               o.o.k.p.k.od.__.k.od.__.k.__.od.__3
    ,                   k.p.k.od.__.k.od.__.k.__.od.__3
    ,                       k.od.__.k.od.__.k.__.od.__3
    ,                           od.__.od.__.k.__.od.__3
    ,                                 od.__.k.__.od.__3
    ,                                       k.__.od.__3
    ,                                            od.__3
    , tri_ (u.__3) (tri p5)
    ]


e_1 :: Korvai
e_1 = ganesh $ date 2017 9 18 $ exercise $ korvai adi $
    [ pkt . t.k.t.k . d.n.pk . t.k.t.k . repeat 3 (d.n.pk)
    , pkt . repeat 3 (t.k.t.k . d.n.pk)
    , su (repeat 6 pkt) . nakatiku
    ]
    where
    pkt = p.kt.p.kt.pk
