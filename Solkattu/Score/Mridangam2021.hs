-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Solkattu.Score.Mridangam2021 where
import           Prelude hiding ((.), repeat)

import qualified Solkattu.Tala as Tala

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

e_fours :: Korvai
e_fours = date 2021 2 15 $ ganesh $ exercise $ korvaiS Tala.rupaka_fast $
    map make
    [ on.od.__.k
    , on.od.su ktpk
    , su $ on.k.p.k.n.o.o.k
    , su $ k.p.k.od.__.k.od.__
    , su $ k.t.__.k.__.n.__.o
    , su $ k.od.__.ktkno
    , on.on.su (on.d.__.k)
    , su $ on.y.on.y.on.d.__.k
    ] ++
    [ sarva . su (k.o.on.k.d.p.n.k . r2 (g (d.o.on.k.d.p.n.k)))
    , make $ su $ on.__.su ktpk.n.o.su ktok
    , sarva . su (r2 (on.__.su ktpk.n.o.su ktok) . on.__.su (ktpk.nakatiku))
    ]
    where
    sarva = on.y.on.on.p&d.y.n.y.on.on.od.y
    make s = sarva . r3 (g s)

e_tisram :: Korvai
e_tisram = date 2021 2 21 $ ganesh $ exercise $ korvaiS adi $ map (nadai 6)
    [ sarva (6*8) . d.__ . trin __ (r3 p1) (r3 p2) (r3 p3)
    , sarva (6*8) . d.__ . trin __ (r3 p3) (r3 p2) (r3 p1)
    , sarva (6*8) . d.__ . trin __ (r3 p2) (r3 p2) (r3 p2)
    ]
    where
    sarva = sarvaM (d.__.p.k.t.k)
    p1 = g (k.n.o)
    p2 = g (k.__.k.n.o)
    p3 = g (k.__.t.__.k.n.o)

s_tisram_sarva :: Korvai
s_tisram_sarva = sarvalaghu $ ganesh $ korvaiS1 adi $ nadai 6 $
    r3 (v.__.p.kt.k . d.__.p. su (kt.p.kt.p)) . su (r3 (g (v.p.kt.p.kt.p)))

e_tisram_tdgno :: Korvai
e_tisram_tdgno = date 2021 4 18 $ ganesh $ exercise $ korvaiS adi $
    map (nadai 6)
    [ repeat 9 (g ktkno) . u.__.p
    , tri_ __ (g (ktkno.ktkno)) . __. g (ktkno.ktkno) . od.__.od.__.k
    ]

e_5s :: Korvai
e_5s = date 2021 5 11 $ ganesh $ exercise $ korvai1 adi $
    endOn 4 $ s $ su $ r7 t5 . r4 (n.__.r7 t5) . od
    where t5 = g "ktpkpto_"

e_gumiki :: Korvai
e_gumiki = date 2021 6 6 $ ganesh $ exercise $ korvaiS adi
    [ sd $ r4 nddn & "_ooooooooo"
    , sd $ r4 nddn & "óoooóoooóo"
    , sd $ r4 nddn & r5 "óo"
    , sd $ r4 nddn & r5 "oó"
    , rh & sd "óooooooo" . rh & o'
    , o.d._'&y.n.o.k.od.y . "^,n,^,d," & sd "oooo" . rh & o'
    ,   o.d._'&y.n.o.k.od.y . "^,n,nnd," & "o_o_ooo_"
      . o.d._'&y.n.p.k.d .y . "^,n,nnd,"
    ]
    where
    rh = "_,n,^,d, ^,n,^,d,"
    nddn = "nddn"

sketch_21_06_12 :: Korvai
sketch_21_06_12 = date 2021 6 12 $ sarvalaghu $ korvaiS adi
    [ r3 (lt p.y.n.d) . su "nnp,".n.d . r3 (lt p.y.n.d) . su "nnp,n,".d
    , r3 "n,nd" . su "n,^,".n.d . r3 "n,nd" . su "nnpnpldl"
    ]

e_21_08_15 :: Korvai
e_21_08_15 = date 2021 8 15 $ ganesh $ korvai adi $
    let pknook123 = g $ su "pknook pkpknook pkpkpknook" in
    [ first
    , s $ rho `replaceEnd` su "pknook" . rh_ `replaceEnd` su "pknook"
    , s $ rho `replaceEnd` su "pknook" . rh_ `replaceEnd` su (r2 "pknook")
    , s $ rho `replaceEnd` su "pknook" . rh_ `replaceEnd` su (r3 "pknook")
    , s $ rho `replaceEnd` su "pknook" . rh_ `replaceEnd` pknook123
    , s $ rho . "ND,n,nd,n,nd,nd,"
    , ending $ tri123' pknook123 "NN_v_"
    ] ++

    let kookou = g $ su "kookou_kno"
        kook = su "kook"
    in
    [ first
    , s $ rho `replaceEnd` kookou . rh_ `replaceEnd` kookou
    , s $ rho `replaceEnd` kookou . rh_ `replaceEnd` (kook.kookou)
    , s $ rho `replaceEnd` kookou . rh_ `replaceEnd` (kook.kook.kookou)
    , ending $ r3 $ kookou . kook.kookou . kook.kook.kookou . r3 (on.v.__3)
    ] ++ concat
    [ mk1 (o&t.k) (t.k)
    , mk1 (su (o.t.o.k)) (su (p.t.p.k))
    , mk2
    , mk3
    , mk4
    ]
    where
    mk1 _Tk  _tk =
        [ first
        , s $ otk `replaceStart` rho . otk `replaceStart` rh_
        , s $ seq . otk `replaceStart` rh_
        , s $ seq . otk . r2 (n_ktpk._tk) . n_ktpk
        , s $ seq . otk . n_ktpk._tk._tk.n_ktpk.n_ktpk
        , ending $ tri_ (od.__) (seq . o.__.k.__)
        , ending $ tsep (seqN 2) (o.k.od.__) (o.__.k.__.od.__) . o.__3.k.__3.od
        , ending $ prefixes (map seqN [1, 2, 3]) (o.__.k.__.od.__)
        ]
        where
        seq = otk . r2 (on_ktok._Tk) . on_ktok
        seqN c = otk . on_ktok . repeat c _Tk . repeat c on_ktok
    mk2 = prepare seq ++
        [ ending $ tri_ (od.__) (otk.seq . o.__.k.__)
        , ending $ tsep (otk.seq) (o.k.od.__) (o.__.k.__.od.__) . o.__3.k.__3.od
        ]
        where
        seq = on_ktok . su "oktknook".n.od.k.on_ktok
    mk3 = prepare seq ++
        [ s $ otk.seq . otk . closed (seqN 2)
        , ending $ tri_ (od.__) (otk.seq . o.__.k.__)
        , ending $
            tsep (otk.seqN 2) (o.k.od.__) (o.__.k.__.od.__) . o.__3.k.__3.od
        , ending $ prefixes (map ((otk.) • seqN) [1, 2, 3]) (o.__.k.__.od.__)
        ]
        where
        seq = on_ktok.su (r2 (g "oktknooktk"))
        seqN c = on_ktok . su (repeat c (g "oktkno") . repeat c (g "oktk"))
    mk4 = prepare seq ++
        [ ending $ join (o.__) $ map ((otk.) • seqN) [1, 2, 3]
        , ending $ tri_ (od.__) (otk.seq.su "o_k_okk_")
        , ending $ tsep (otk.seq) (su "okk_D___") (su "o_k_okk_D___")
            . su "o_k_okk_okk_D"
        ]
        where
        seq = seqN 1
        seqN c = on_ktok . su "oktkoktknook" . repeat c (su "nok_ou_k")

    first = x2 $ s $ rho . rh_
    prepare seq =
        [ first
        , s $ otk `replaceStart` rho . otk `replaceStart` rh_
        , s $ otk.seq . otk `replaceStart` rh_
        , s $ otk.seq . otk.closed seq
        ]

    n_ktpk = g $ n.su ktpk
    on_ktok = g $ on.su ktok
    otk = o.t.k
    rho = rh & "oo_o_oo_o_oo_o"
    rh_ = rh & "oo"
    rh = "nd,n,nd,n,nd,n".su "ktok"
    tri123' sep a = sep . tri123 sep a

e_21_10_10 :: Korvai
e_21_10_10 = date 2021 10 10 $ ganesh $ korvai adi
    [ s $ rho . rh_
    , s $ rho . "ND,n,nd" . su (pk.r4 "dpn,")
    , x2 $ s $ su "dpn,do" `replaceStart` rho . "ND,n,nd" . su (pk.r4 "dpn,")
    , s $ su $ "dpn," . r5 "doN," . "do" . g (on.__.ktok) . r8 "dpn,"
    , s $ su $ r2 $ r4 "doN," . r4 "dpn,"
    , s $ su $ r4 $ r2 "doN," . r2 "dpn,"
    , ending $ su $ r4 "doN,dpn," . tri_ "d_pk" (g "doN,doN,")
    ]
    where
    rho = rh & "oo_o_oo_o_oo_o"
    rh_ = rh & "oo"
    rh = "nd,n,nd,n,nd,n".su "ktok"
