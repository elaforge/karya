-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Solkattu.Score.Mridangam2020 where
import Prelude hiding ((.), repeat)
import Solkattu.Dsl.Mridangam


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
    r2 = repeat 2

sarva_20_02_27 :: Korvai
sarva_20_02_27 = date 2020 2 27 $ ganesh $ sarvalaghu $ korvai adi $
    map (smap (nadai 5))
    [ s $ repeat 6 (d_n_nd_n.ktpk)
        . d.y.n.ktpk.d.y.n.ktpk
        . (n.ktok.on.ktok . su (o.t.o.k.o.t.o.k))
    , s $ o.__5.__5.k .d_n_nd_n & o_o_o .ktok
        . o&d_n_nd_n.ktpk.d_n_nd_n.ktok
    , x2 $ s $ repeat 2 (d_n_nd_n & o_o_o .ktok)
        . o&d_n_nd_n.ktpk . (n.ktok.on.ktpk . su nakatiku) -- x2
    , s $ d_n_nd_n & o_o_o .ktpk . n.ktok.on.ktpk.su nakatiku
        . d_n_nd_n         .ktpk . n.ktok.on.ktpk.su nakatiku
    , s $ d_n_nd_n & o_o_o .ktok . o. tri_ __ (tri (ktkt.o))
    , s $ k.__5.__5.k .d_n_nd_n & o_o_o .ktok
        . o&d_n_nd_n.ktpk.d_n_nd_n.ktok
        . repeat 2 (d_n_nd_n & o_o_o . ktok)
        . o&d_n_nd_n.ktpk.d_n_nd_n.ktok
    , s $  repeat 2 (d_n_nd_n & o_o_o . ktok)
      . o & (repeat 3 d_n_n . d.__.n.o.on)
    , s $ repeat 2 $ repeat 3 d_n_n . d.__.n.o.on
    , s $ repeat 4 $ d_n_n . d.__.n.o.on
    , s $ repeat 3 (d.__.n.o.on) . d.__.n.o . tri (group (k.o.o.k.n.p.k))
    ]
    where
    ktok = su (k.t.o.k)
    ktpk = su (k.t.p.k)
    ktkt = su (k.t.k.t)
    d_n_nd_n = d.y.n.y.n.d.y.n
    o_o_o    = o.__.o.__.o.o.__.o
    d_n_n = d.__.n.y.n

e_20_02_24 :: Korvai
e_20_02_24 = date 2020 2 24 $ ganesh $ exercise $ korvaiS1 adi $
    tri_ (od.__4) (su ktok.t.o.su (ktok.kook))

{-
    exercise:
    tha - kitakita taka dikutarikitataka nakadit - tang - taka dikutarikitataka
    di; thom; nam
-}

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

s :: Sequence -> Section
s = section
