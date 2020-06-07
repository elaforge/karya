-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Solkattu.Score.Mridangam2020 where
import Prelude hiding ((.), repeat)
import Solkattu.Dsl.Mridangam


-- * exercises

e_20_02_24 :: Korvai
e_20_02_24 = date 2020 2 24 $ ganesh $ exercise $ korvaiS1 adi $
    tri_ (od.__4) (su ktok.t.o.su (ktok.kook))

e_20_03_27 :: Korvai
e_20_03_27 = date 2020 2 27 $ source "anand" $ exercise $ korvaiS adi $
    map (su • cycle)
    [ n.p.k.__.u.__.pk.nakatiku
    , n.p.k.__.u.__.o.k.n.o.u.o.k.t.o.k
    , n.p.ktkt.o.k . n.o.k.o&t.k.o.o&t.k
    , on.__.kt.k.o.o&t.k . n.o.kt.k.o.o&t.k
    ]
    where
    cycle = prefixes [prefix p, prefix k, prefix o, prefix n]
    prefix stroke = stroke.__.ktkt.pk.nakatiku

e_20_05_01 :: Korvai
e_20_05_01 = date 2020 5 1 $ source "anand" $ exercise $ korvaiS adi
    [ r2 $ rh & strM "ó_oó_oó"
    , r2 $ rh & strM "ó_oó_oó____p__p"
    , r2 $ rh & strM "oóoo_oo_oóoo"
    , r2 $ rh & strM "oóoo_oo_oóoo_pp"
    ]
    where
    rh = r4 $ n.k.t.k

-- * sarva tani

sarva_tani :: [Part] -- realizePartsM patterns misra_tani
sarva_tani =
    [ K sarva_20_05_29 All
    , K sarva_20_06_05 All
    , K sarva_20_01_27 All
    , Comment "namita dimita dimi"
    , K sarva_20_02_10 All
    , K sarva_20_02_27 All
    , Comment "farans"
    , K sarva_20_05_08 All
    ]

sarva_20_01_27 :: Korvai
sarva_20_01_27 = date 2020 1 27 $ ganesh $ sarvalaghu $ korvai adi
    [ x4 $ s $ nddn & _oo . nddn & o'
    , x4 $ s $ (__.k) `replaceStart` nddn & o'oo . nddn & o'
    , x4 $ s $ (__.k) `replaceStart` nd_n  & o'oo  . nd_n & o'
    , x4 $ s $ _d_n & o'oo . _d_n & o'
    , x4 $ s $ _d_n2 & o'oo2 . _d_n2 & o'
    , x4 $ s $ _n_d & o'oo . _n_d & o'
    , s $ repeat 6 (group (_n_d6 & sd (o'.o.o.o.o.o) . _n_d6 & o'))
        . repeat 4 (group (_n_d4 & sd (o'.o.o.o) . _n_d4 & o'))
        . repeat 4 (group (o'.k.n.y))
    , s $ _n_d & o'oo . _n_d & o'
    , s $ _d_n & o'oo . _d_n & o'
    , x2 $ s $ repeat 2 $ n_d_ & o'_oo'
    , x2 $ s $ repeat 2 $ d_d_ & o'_oo'
    -- plain dimita dimi, plus dinNa - din, reduce to 2/avartanam
    --
    -- skip this:
    -- dimita dimi + tang kitataka dhomka
    -- dimita dimi + tang kitataka dugudugu
    ]
    where
    o'oo = sd (o'.o.o.o.o.o.o.o)
    o'oo2 = sd (o'.o.o.o).o.o.sd (o.o.o)
    _oo  = sd $ __.o.o.o.o.o.o.o
    nddn  = n.l.d.l.d.l.n.l.n.l.d.l.d.l.n.l
    nd_n  = repeat 2 $ n.y.d.yjy.n.y
    _d_n  = __.k.d.yjy.n.yjy.d.yjy.n.y
    _d_n2 = __.k.d.yjy.n.y.n.n.d.yjy.n.y
    _n_d  = __.k.n.yjy.d.yjy.n.yjy.d.y
    _n_d6 = __.k.n.yjy.d.yjy.n.y
    _n_d4 = __.k.n.yjy.d.y
    n_d_  = repeat 2 $ n.yjy.d.yjy
    d_d_ = repeat 4 $ d.yjy
    o'_oo' = o'.__.o.o'.__5 .__.__.o.o'.__.__.o.__
    yjy = y.j.y

sarva_20_02_10 :: Korvai
sarva_20_02_10 = date 2020 2 10 $ ganesh $ sarvalaghu $ korvai adi
    [ x2 $ s $ (nd_n_nd_.n.d.y) & oo_o_oo_ . (nd_n_nd_.n.d.y) & (o.o')
    , x2 $ s $ (nd_n_nd_.n.ktok) & oo_o_oo_' . (nd_n_nd_.n.ktok) & (o.o')
    , s $ (nd_n_nd_.n.ktok) & oo_o_oo_' . (nd_n_nd_.n.pkd) & (o.o')
        . repeat 2 (d__n_nd_.n.pkd)
        . repeat 3 (d.__.y.n.y.d.pkd) . pkd.pkd.pkd.pkd
    , s $ pkd.pk.r2 (n.pk.n.d.y).n.pk.d    .d.__.k.r2 (n.pk.n.d.y).n.ktpk
        . n.d.y .r2 (n.pk.n.d.y).n.ktpk .ptknpk.r2 (n.pk.n.d.y).n.ktpk
        . ptknpk.n.y.n.d.y.ptknpk.n.y.n.d.y.ptknpk
            . (n.pk.n.d.y).ptknpk.ptknpk.su (p.n.p.k)
        . ptknpk.r2 (n.pk.n.d.y).n.ktpk . nd_n_nd_.n.ktpk
    , x2 $ s $ nd_n_nd_.n.ktpk . nd_n_nd_' . n5 (d_n_nd_.n.ktpk)
    , s $ r2 $ nd_n_nd_' . n5 (d_n_nd_.n.ktpk)
    ]
    where
    n5 = nadai 5
    nd_n_nd_  = n.d.y.r2 (n.y.n.d.y)
    nd_n_nd_' = n.d.y.   (n.y.n.d.y)
    d__n_nd_ = d.__.y.r2 (n.y.n.d.y)
    pkd = pk.d
    oo_o_oo_ = o.o.__.r2 (o.__.o.o.__).o.o.__
    -- delayed gumiki: on the rest
    oo_o_oo_' = o.o'.__.r2 (o.__.o.o'.__).o'.__.__
    d_n_nd_ = d.y.n.y.n.d.y
    pk = su (p.k)
    ptknpk = su (p.t.k.n.p.k)
    ktok = su (k.t.o.k)
    ktpk = su (k.t.p.k)

sarva_20_02_27 :: Korvai
sarva_20_02_27 = date 2020 2 27 $ ganesh $ sarvalaghu $ korvai adi $
    map (smap (nadai 5))
    [ s $ repeat 6 (d_n_nd_n.ktpk)
        . d.y.n.ktpk.d.y.n.ktpk
        . n.ktok.on.ktok . su (o.t.o.k.o.t.o.k)
    , s $ o.__5.__5.k .d_N_ND_N.ktok
        . o&d_n_nd_n.ktpk.d_n_nd_n.ktok
    , x2 $ s $ r2 (d_N_ND_N.ktok)
        . o&d_n_nd_n.ktpk . (n.ktok.on.ktpk . su nakatiku)
    , s $ d_N_ND_N.ktpk . n.ktok.on.ktpk.su nakatiku
        . d_n_nd_n.ktpk . n.ktok.on.ktpk.su nakatiku
    , s $ d_N_ND_N .ktok . o. tri_ __ (tri (ktkt.o))
    , s $ k.__5.__5.k .d_N_ND_N.ktok . o&d_n_nd_n.ktpk.d_n_nd_n.ktok
        . r2 (d_N_ND_N.ktok)         . o&d_n_nd_n.ktpk.d_n_nd_n.ktok
    , s $ r2 (d_N_ND_N.ktok) . o & (tri d_n_n . d.__.n.o.p&k)
    , s $ repeat 2 $ repeat 3 d_n_n . d.__.n.o.p&k
    , s $ repeat 4 $ d_n_n . d.__.n.o.p&k
    , s $ repeat 3 (d.__.n.o.k&p) . d.__.n.o . tri (group (k.o.o.k.n.p.k))

    , s $ o.__5.__5.k .d_N_ND_N.ktok . o&d_n_nd_n.ktpk.d_n_nd_n.ktok
    , s $ r2 (d_N_ND_N.ktok) . o&d_n_nd_n.ktpk . d.p.k.t.__.k.__.n.__.o
    , s $  d_N_ND_N.ktpk .d.p.k.t.__.k.__.n.__.o
        .o&d_n_nd_n.ktpk .d.p.k.t.__.k.__.n.__.o
    , s $  d_N_ND_N.ktok .od.__.takitatatakadinna
        .o&d_n_nd_n.ktpk . d.__.takitatatakadinna
    , s $ r2 $ r2 takitatatakadinna.o.__.k.__ -- 8 2 8 2
    , s $ r2 $ takitatatakadinna.od.__.p.k.__.t.__.k.__.ktkt.o -- (8 3 9) * 2
        -- 2 8 3 999
    , s $ on.k . takitatatakadinna.od.__.p. tri (k.__.t.__.k.__.ktkt.o)
    , s $ k.__5 . __M (5*2).__5.k.od.__.k.t.k. r2 (n.k.k.t.k) . n.o.o.k.o
        . r2 (o&v.__.k.t.k. r2 (n.k.k.t.k) . n.o.o.k.o)
    -- tisram in kandam, effectively 7.5 nadai
    , s $ nadai 15 $ stride 2 $ -- 12:24
        let d_ktk = group (d.__.p.k.t.k)
            d_kook = d.__.k.o.o.k
        in r2 (o & r4 d_ktk . d_kook) . r4 (o&d_ktk . d_kook) . r2 (o&d_kook)
    , s $ r4 (o&v.__.k.t.k. r2 (n.k.k.t.k) . n.o.o.k.o)
    , x3 $ s $ od.__4 . tsep (p5.p5) (u.__5) (i.__5)
        . prefixes [ø, kp, kpnp] (group (k.p.k.od.__.p5))
    ]
    where
    takitatatakadinna = group $ on.t.k.p&k.n.o.o.k
    ktok = su (k.t.o.k)
    ktpk = su (k.t.p.k)
    ktkt = su (k.t.k.t)
    d_n_nd_n = d.y.n.y.n.d.y.n
    d_N_ND_N = d_n_nd_n & ooo
    ooo    = o.__.o.__.o.o.__.o
    d_n_n = d.__.n.y.n
    -- make beginning o oo thing cleaner
    -- pull back on namita dimita
    -- isolate okotokot ending to make it cleaner

-- To farans and mohra.
sarva_20_05_08 :: Korvai
sarva_20_05_08 = date 2020 5 8 $ sarvalaghu $ korvai adi
    -- transition to farans
    [ s $ od.__3.k.n.o.o.k . r3 (on.t.k.p&k.n.o.o.k)
        . sam . r4 (od.o.o.v.__.o.o.k)
        . sam . od.o.o.v.p.k.n.k.d.n.k.d.k.k.__.k.__.i.__.i.__
            . k.n.k.t.k.v.su (pk.nakatiku)
    , let rnaka = group $ u.p.k.t.p.k.t.p in
        s $ su $ r2 (od.__4 . r2 rnaka.u.__.pk.nakatiku)
        . sam . r2 (pk.o.__.u.__.pk.nakatiku)
            . pk.o.__.u.__.pk.pk.o.__.u.__ . pk.sd (p.k.__.k.__.i.__.i)
    -- farans
    , s $ nadai 6 $
        sd (__.v.p.k.t.k.v) . pk.nakatiku . pk.pk . sd (o.k.t.k.v) . pk.nakatiku
        . sam . o.u.__.k.o.u.__.k.nakatiku.nakatiku
              . o.k.p.k.p.k.t.k.nakatiku.nakatiku
        . sam . o.o.k.n.p.k.t.n.nakatiku.nakatiku
              . p.k.o.o.k.t.p.k.nakatiku.nakatiku
    , s $ nadai 6 $
        n.p.u.__.k.__ . pk.nakatiku . n.p.u.p.k.t.p.u
        . __.k.p.k . sd (o.u.p.k.u) . pk.nakatiku
    -- mohra
    , s $ nadai 6 $
        r2 (od.__.p.k.nakatiku . r2 (n.__.p.k.nakatiku) . r3 (p.k.o.__))
          . od.__.p.k.nakatiku . r2 (n.__.p.k.nakatiku) . p.u.__.k.o.__
          . n.__.p.k.nakatiku . p.u.__.k.o.__
          . n.__.p.k.nakatiku . r3 (p.u.__.k.o.__.k.__)
    -- mohra korvai -- 2+4 + 2+3 + 3+3 = 17
    , s $ nadai 6 $ r3 $ k.__.od.__4.pk.od.__3.k.pk.od.__3.tri p5
    -- alternates, for practice:
    , s $ nadai 6 $ r3 $ sd (p.k.od.__.k.od.__) . od.__3 . tri p5
    , s $ nadai 6 $ r3 $ k.__.od.__.pk.od.__.pk.od.__ . tri_ __ p6 -- Also 567
    ]

sarva_20_05_29 :: Korvai
sarva_20_05_29 = date 2020 5 29 $ korvai adi
    [ s $ sarva 7 . end
        . sam . start . sarva 6 . end
        . sam . r2 (start . sarva 2 . end)
        . sam . r4 (start . end)
    -- In the recording, he has 2 matra karvai, and lands on eddupu +0.5.
    , s $ r2 (start.end) . end.o'&d . end.o'&d . nadai 3 (su __.end)
    ]
    where
    sarva = sarvaD (n.od.od.on.su (on.on).od.od.on . on.d.d.n.n.d.d.n)
    start = o'.su (o.d).od.on
    end = su $ r4 $ o'&d . d

-- 7 reductions
sarva_20_06_05 :: Korvai
sarva_20_06_05 = date 2020 6 5 $ korvaiS1 adi $
    -- 8*7 + 8 = 8 = 4 avartanams
      g (v.v.__         .su (d.o).od.od.on   . on.d.d.n.d.d.n)
    . g (v.v.__         .su (n.n).d.d.n      . n .d.d.su (n.o.o.v.o.o.o.v))
    . g (su (o.k).od.__ .su (on.on).od.od.od
                            . on.od.od.su (n.o.o.v.o.su (__.k).o.v))
    . g (su (o.k).od.__ .su ( n.od).od.od.od . su (n.o).od.od. oktk4)
    . g (su (o'.k).od.__.su ( n.od).od.od.od . on.od.__      . oktk4)
    . g (r2 $ o'.od.__  .oktk4)
    . g (r2 $ o'.od.__  .oktk4)
    . g (o'.od.__       .oktk4 . o'.o'.__.oktk4)
    . o'.__n 6       .oktk4 . su (su (r8 (o'.__.t))) -- 6 + 4 + 8*(3/4)
    where
    oktk4 = su (su (r4 (o'.k.t.k)))
    g = group

{-
    thani:
    tang -- kitakitataka
    tam - takatat din ...
    talang ga din
    takita takita takita ...
    tkoo ktpk diku ...
    tang taka diku 3x
    diku 3x
    mohra
-}
