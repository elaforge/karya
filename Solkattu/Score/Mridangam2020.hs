-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Solkattu.Score.Mridangam2020 where
import           Prelude hiding ((.), repeat)

import           Solkattu.Dsl.Mridangam


-- * exercises

e_20_02_24 :: Korvai
e_20_02_24 = date 2020 2 24 $ ganesh $ tirmanam $ korvaiS1 adi $
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
    [ r2 $ rh & "ó_oó_oó"
    , r2 $ rh & "ó_oó_oó____p__p"
    , r2 $ rh & "oóoo_oo_oóoo"
    , r2 $ rh & "oóoo_oo_oóoo_pp"
    ]
    where
    rh = r4 $ n.k.t.k

-- * sarva tani

print_sarva_tani :: IO ()
print_sarva_tani = realizeScoreM (abstract patterns) sarva_tani

sarva_tani :: Score
sarva_tani = tani
    [ K sarva_20_05_29
    , Comment "sarva, no thom, use vv"
    , K sarva_20_06_05
    , K sarva_20_06_12
    , K sarva_20_06_12_reduction
    , K sarva_20_06_19_endings
    , K sarva_20_06_19
    , K sarva_20_06_19_reduce5
    , K sarva_20_01_27
    , Comment "namita dimita dimi"
    , K sarva_20_02_10
    , K sarva_20_02_27
    , Comment "farans"
    , K sarva_20_05_08
    ]

sarva_20_01_27 :: Korvai
sarva_20_01_27 = date 2020 1 27 $ ganesh $ korvai adi
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
sarva_20_02_10 = date 2020 2 10 $ ganesh $ korvai adi
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
sarva_20_02_27 = date 2020 2 27 $ ganesh $ korvai adi $
    map (fmap (nadai 5))
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
sarva_20_05_08 = date 2020 5 8 $ ganesh $ korvai adi
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

-- n d d n enters gradually
sarva_20_05_29 :: Korvai
sarva_20_05_29 = date 2020 5 29 $ ganesh $ korvaiS adi
    [ sarva 7 . end
      . sam . r2 (start . sarva 2 . end)
      . sam . r4 (start . end)
    -- In the recording, he has 2 matra karvai, and lands on eddupu +0.5.
    , sarva 4 . end.o'&d . end.o'&d . nadai 3 (su __.end)
    ]
    where
    sarva = sarvaD (n.od.od.on.su (on.on).od.od.on . on.d.d.n.n.d.d.n)
    start = o'.su (o.d).od.on
    end = su $ r4 $ o'&d . d

-- TODO starts and ends on arudi
-- sarva laghu before 7s, don't use thom
-- also prepare with some sarvalaghu that does vv_ stuff.
sarva_20_06_05 :: Korvai
sarva_20_06_05 = date 2020 6 5 $ ganesh $ korvaiS1 adi $
    -- 8*7 + 8 = 8 = 4 avartanams
      g7 (p&v.p&v.__     .su (d.o).od.od.on   . on.d.d.n.d.d.n)
    . g7 (p&v.p&v.__     .su (on.o).od.od.on . on.d.d.su (n.o.o.v.o.o.o.v))
    . g7 (su (o.k).od.__ .su (on.o).od.od.od
                            . on.d. d.su (n.o.o.v.o.su (__.k).o.v))
    . g7 (su (o.k).od.__ .su (on.o).od.od.od . su (on.o).od.od. oktk4)
    . g7 (su (o'.k).od.__.su (on.o).od.od.od . on.od.__      . oktk4)
    . g7 (r2 $ o'.od.__  .oktk4)
    . g7 (r2 $ o'.od.__  .oktk4)
    . g7 (o'.od.__       .oktk4 . o'.o'.__.oktk4) -- TODO slow gumiki
    . o'.__n 6       .oktk4 . su (su (r8 (o'.__.t))) . o' -- 6 + 4 + 8*(3/4)
    where
    oktk4 = su (su (r4 (o'.k.t.k)))
    g7 = checkD (7/2) • group

sarva_20_06_12 :: Korvai
sarva_20_06_12 = date 2020 6 12 $ ganesh $ korvaiS adi
    [ __D (3/2) . v.__.v.__8 .__3 . su ktpk . r4 (n.d.__)
    , sarva 6 . k.o.su (o.v.__.k).o.k.su (o.v.__.k)
    , o.k.od.__ . sarva 5 . su ktpk . t.o.su (ktpk.kook)
    , begin . sarva 4 . su (ktpk.p.t.p.k.p.t.o.__.kook)
    , begin . sarva 4 . su (ktpk.p.t.o.__.p.t.o.__.kook)
    ]
    where
    -- melkalam sarva
    sarva = sarvaD $
        (inter l (n.d.d.n) .n.n. inter l (d.d.n)) & strM "o_o_o_o_ooo_o_o"
        . o & inter l (n.d.d.n.n.d.d.n).l
    begin = od.__.od.__.o.l.on.l

sarva_20_06_12_reduction :: Korvai
sarva_20_06_12_reduction = date 2020 6 12 $ korvaiS adi
    [ nadai 6 $ sarvaD mempty 12
    , nadai 6 $ sd (sd (tri (kook.od.__3))) . sd (tri (kook.od.__3))
        . tri_ (od.__3) kook.k
    ]

sarva_20_06_19_endings :: Korvai
sarva_20_06_19_endings = date 2020 6 19 $ korvaiS adi
    [ __D 1.5 . v.__.v.__4.__D 1
    -- [ __D 1.5 .__.__.__ . n.__.n.d.n.d.d
            . n.y.n.d.n.n.__.d.n.__.p&k.__.p&t.__.p&k.__.o&v
              -- recording: 5:26.68
    , sarva 6 .__. su (p.k.n.o.o.k . o.t.o.k.o.t.o.k)
    , sarva 6 . su (k.t.o.k.o.v.__.k.o.k.o.k.o.v.__.k)
    ]
    where
    sarva = sarvaD mempty

sarva_20_06_19 :: Korvai
sarva_20_06_19 = date 2020 6 19 $ korvaiS adi
    -- 222 333 444
    [ tri_ __ (pk.od) . tri_ __ (k.pk.od) . tri_ __ (k.k.pk.od)
    -- 234 234 234
    , tri $ trin __ (pk.od) (k.pk.od) (k.k.pk.od)

    , tri_ __ pknpv . tri_ __ (o.pknpv) . tri_ __ (o.v.pknpv)
    , tri_ __ ktpko . tri_ __ (k.ktpko) . tri_ __ (k.k.ktpko)
    ]
    where
    pknpv = su (su (p.k.n.p)).v
    ktpko = su (su ktpk).o
    pk = su (p.k)

sarva_20_06_19_reduce5 :: Korvai
sarva_20_06_19_reduce5 = date 2020 6 19 $ korvaiS adi
    -- 5 + 5/2 + 5/2/2 + 5/2/2/2 * 2 =
    -- 5 + 5/2 + 5/2/2 + 5/2/2 =
    -- 5 + 5/2 + 5/2 =
    -- 10 * 3 = 30
    [ sd $ sd $ __.__. r3 (
          g (k.__3.od.__)
        . g (su (k.__3.od.__))
        . g (su (su (k.__3.od.__)))
        . g (su (su (su (k.p.k.od.__.ktkno))))
        )
    ]

e_20_07_03 :: Korvai
e_20_07_03 = date 2020 7 3 $ exercise $ korvaiS adi
    [ su $ prefix . r7 (o.o.o.v) . o.o.o.k
    , su $ prefix . r6 (o.o.o.v) . r3 (o.o&v).o.k
    , su $ prefix . r4 (o.o.o.v) . r7 (o.o&v).o.k
    , su $ prefix . r4 (o.__.v.__.pknpv)
    , su $ prefix . r4 (o.__.pknpv) . r2 pknpv
    , su $ prefix . r8 pknpv
    ]
    where
    pknpv = su (p.k.n.p).v.__
    prefix = p.__.ktkt.pk.nakatiku .n.p.k.__.u.__.pk.nakatiku

e_20_07_17 :: Korvai
e_20_07_17 = date 2020 7 17 $ exercise $ ganesh $ korvaiS adi
    [ su $ r3 (p.__.kt.kt.pk) . nakatiku . theme
    , su $ r3 (p.__.kt.kt.pk) . nakatiku . theme2
    , su $ nadai 5 $ r4 (takeD 5 theme)
    , r5 (od.__.k) . r3 (od.__.k.__.o) . od.__
    ]
    where
    theme = od.__.k.od.__.k.__.od.__.k.o.od.__
        . k.od.__.k.__.od.__.k.__.o.od.__.k.o.od.__.k.od.__
    theme2 = d.__.n.d.__.n.__.d.__.n.y.d.__
        . n.d.__.n.__.d.__.n.__.y.d.__.n.y.d.__.n.d.__

thani_exercise :: Korvai
thani_exercise = date 2020 7 3 $ exercise $ korvai adi
    [ s $ r2 (od.__.o.su (ktkt.pk)) . od.__.k.__
        . r2 (d.__.p.su (ktkt.pk)) . d.__.k.__
        . r2 (od.__.o.su (ktkt.pk)) . od.__.k.__
        . r2 (d.__.p.su (ktkt.pk)) . su nakatiku
    , s $ su $ r2 (on.__.pk.t.__.o.__.ktpk) . nakatiku
        . r2 (on.__.pk.nakatiku) . nakatiku
    , s $ su $ r2 (p.u.__.k.o.__.ktkt.pk) . nakatiku
        . r3 (p.u.__.k.o.__) . ktkt.pk.nakatiku
    , x2 $ s $ su $ r2 (n.pk . r3 (t.pk)) . nakatiku
        . r2 (r2 (t.pk).n.pk.t.pk) . nakatiku
    , s $ su $ r2 (t.k.o.o.ktpk.nakatiku)
        . r3 (t.k.o.o.ktpk) . nakatiku
        . t.k . r5 (o.o.kt).pk . nakatiku
        . t.k . r4 (o.o.kt).o.o.k.__.pk . nakatiku
    , s $ su $ r3 (r2 (on.__.pk.nakatiku).nakatiku)
        . tri_ (od.__4) nakatiku
    , commentS "mohra" $ s mempty
    ]

e_20_11_01_npk :: Korvai
e_20_11_01_npk = date 2020 11 1 $ exercise $
    comment "once in chatusram, then in tisram" $
    korvaiS adi
    [ pat (od.__6)
    , pat (sd (n.pk))
    , pat (od.__3.od.__3)
    , pat (od.__.od.__.od.__)
    , pat (o.u.__.o.u.__)
    , pat (o.o.k.od.__.k)
    , pat (o.o.k.o.od.__)
    ]
    where
    pat karvai = nadai 6 $ su $ r4 npk . tri_ karvai npk
    npk = g (n.pk.r3 (t.pk))

sketch_20_11_08 :: Korvai
sketch_20_11_08 = date 2020 11 8 $ korvaiS adi
    [ r2 (r2 (g (n.k.u.ktok.o)) . u.ktok.o)
    , r2 (g (n.k.u.ktok.o.__) . (k.n.k.u.ktok.n.ktok))
    , r2 (g (n.u.ktok.o.__.n.u.ktok.d.ktok.n.ktok))
    ]
    where
    ktok = su (kt.o.k)

e_20_12_06 :: Korvai
e_20_12_06 = date 2020 12 6 $ ganesh $ exercise $ korvaiS1 adi $
    r2 (on.__.pk.nakatiku) . su (r2 (t.__.o.__.ktpk))
