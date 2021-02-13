-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Solkattu.Score.Mridangam2021 where
import           Prelude hiding ((.), repeat)

import           Solkattu.Dsl.Mridangam


-- originally on 2014-09-29
-- try at 80bpm
e_kanda :: Korvai
e_kanda = ganesh $ exercise $ korvaiS adi $ map (nadai 5)
    [ o&rh . r7 rh
    , su (o.t.o.k).on. su ktpk . r7 rh
    , su (o.t.o.k).on. su ktpk . r4 rh . r5 (g (n.su ktpk))
    , o&rh . r4 rh . r5 (g (n.su ktpk))
    , od.__. r6 (g (n.su ktpk)) . od.__ . r3 (n.su (pk.nakatiku))
    , let th = o&t.k.n.su ktpk.n.su ktpk . r2 (t.k.t.k.n.su ktpk.n.su ktpk) in
        od.__9.k . o&t.k.th
        .  o&t.k.th.nang_kita_tat_thom . __.__.th.nang_kita_tat_thom

    , let th = o&t.k.n.su ktpk.n.su ktpk . nang_kita_tat_thom
        in o&t.k.th . __.__.th

    -- from 2014-10-28a
    , r2 nang_kita_tat_thom . r2 (nang_kita.o) . nang_kita_tat_thom
    , let th = nang_kita.o.k.__.od.__
        in r2 $ o.th . __.th

    , let th = r3 (nang_kita.o) . k.__.od.__
        in o.th . __.th
    , let th = n.su ktok.o . n.su (ktok.ot.ok).o . nang_kita.o.k.__.od.__
        in o.th . __.th
    , let th = n. su (ktok.ot.ok.ot.ok.ot.ok).o . nang_kita.o.k.__.od.__
        in o.th . __.th

    -- endings
    , tri_ k (tri_ (k.__.od.__) (g (p.nang_kita.o)))
    , trin k
        (tri_ (k.__.od.__) (g (p.nang_kita.o)))
        (tri_ (k.__.od.__) (g (p.nang_kita.o)))
        (p.nang_kita.o . k.__.od.__ . p. r3 (nang_kita.o))
    , trin k
        (tri_ (k.__.od.__) (g (p.nang_kita.o)))
        (tri_ (k.__.od.__) (g (p.nang_kita.o)))
        (tri_ __5 (g (p.nang_kita.o)))
    ]
    where
    nang_kita = n . su (kt.pk.p.t)
    nang_kita_tat_thom = nang_kita.o.k.__.od.__.__
    rh = t.k.n.su ktpk
    ot = o.t
    ok = o.k

-- work on otokN
e_21_02_07 :: Korvai
e_21_02_07 = date 2021 2 7 $ ganesh $ exercise $ korvaiS1 adi $ nadai 5 $ su $
    g (o.t.o.k.on.__.ktpk) . r3 (g (p.t.p.k.n.__.ktpk))
    . r5 (g (p.t.p.k.p.n.p.k))

e_nd_d :: Korvai
e_nd_d = date 2021 1 31 $ ganesh $ exercise $ korvaiS adi
    [ r2 $ nd_d0 . r2 nd_d . nd_d_
    , nadai 5 $ r2 $ nd_d0 . r3 nd_d . nd_d_
    , nadai 6 $ r2 $ nd_d0 . r4 nd_d . nd_d_
    , r2 $ nd_ktpk0 . r2 nd_ktpk . nd_ktpk_
    , nadai 5 $ r2 $ nd_ktpk0 . r3 nd_ktpk . nd_ktpk_
    , nadai 6 $ r2 $ nd_ktpk0 . r4 nd_ktpk . nd_ktpk_
    ]
    where
    nd_d0 = on.od.y.od
    nd_d = n.d.y.d
    nd_d_= n.od.y.od

    nd_ktpk0 = (o.o) & nd_ktpk
    nd_ktpk = n.d.su ktpk
    nd_ktpk_ = su $ n.o.od.__.ktok