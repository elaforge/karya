-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Solkattu.Score.Mridangam2017 where
import Prelude hiding ((.), repeat)

import Derive.Solkattu.MridangamGlobal


c_17_07_10 :: Korvai
c_17_07_10 = exercise $ korvai1 adi $ su $
    o.__.k.o.k.o.o.k . repeat 4 (o.o.k.o.k.o.o.k) . repeat 4 (o.k.k.o.o.k)

e_1 :: Korvai
e_1 = ganesh $ date 2017 9 18 $ exercise $ korvai adi
    [ pkt . t.k.t.k . d.n.pk . t.k.t.k . repeat 3 (d.n.pk)
    , pkt . repeat 3 (t.k.t.k . d.n.pk)
    , su (repeat 6 pkt) . nakatiku
    ]
    where
    pkt = p.kt.p.kt.pk

e_2 :: Korvai
e_2 = ganesh $ date 2017 11 13 $ exercise $ korvai adi $ concat
    [ map pattern [s1, s2, s3, s4]
    , [cmap fast [p, k, o, n]]
    , map pattern [t1, t2]
    , [t_sarva]
    ]
    where
    pattern s = cmap s [p, k, o, n]
    s1 x = x.__.ktkt.pk.kt.pk.kt.pk
    s2 x = x.__.ktkt.pk.n.n.pk.kt.pk
    s3 x = x.x .ktkt.pk.n.n.pk.kt.pk
    s4 x = x.x .ktkt.pk.n.n.pk.d.d.pk
    fast x = x.__.ktkt.pk.n.__.pk.d.__.pk

    t1 x = x.__.ktkt.pk.pk.n.pk.d.pk
    t2 x = x.__.ktkt.pk . repeat 3 (pk.n.pk.d.pk)

    t_sarva = o.k.n.o.k.d.o.k . repeat 3 (pk.n.pk.d.pk)


-- Speaking exercises: tadinginathom, taka tadinginathom
e_3 = ganesh $ korvai (beats 15) $
    [ repeat 4 $ tdgnt.tdgnt . su (tdgnt.tdgnt)
    , repeat 4 $ nadai 6 (k.__.t.k.__.n.o.__ . k.t.__.k.n.__.o)
        . su (tdgnt.tdgnt)
    ]
e_4 = ganesh $ korvai (beats 21) $
    [ repeat 4 $ tktdgnt.tktdgnt . su (tktdgnt.tktdgnt)
    , repeat 4 $ nadai 6 (k.__.p.k.__.t.k.__.n.o.__ . k.p.__.k.t.__.k.n.__.o)
        . su (tktdgnt.tktdgnt)
    ]
    where
    tktdgnt = k.p.tdgnt
