-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Solkattu.Score.Mridangam2020 where
import Prelude hiding ((.), repeat)
import Solkattu.Dsl.Mridangam


sarva_20_01_27 :: Korvai
sarva_20_01_27 = date 2020 1 27 $ ganesh $ sarvalaghu $ korvai adi
    [ x4 $ section $ nddn2 & _oo2 . nddn & o'
    , x4 $ section $ (__.k) `replaceStart` nddn2 & o'oo2 . nddn & o'
    , x4 $ section $ (__.k) `replaceStart` nd_n  & o'oo  . nd_n & o'
    , x4 $ section $ _d_n & o'oo . _d_n & o'
    , x4 $ section $ _d_n2 & o'oo2 . _d_n2 & o'
    , x4 $ section $ _n_d & o'oo . _n_d & o'
    , section $ repeat 6 (_n_d6 & sd (o'.o.o.o.o.o) . _n_d6 & o')
        . repeat 4 (_n_d4 & sd (o'.o.o.o) . _n_d4 & o')
        . repeat 4 (o'.k.n.y)
    , section $ _d_n2 & o'oo2 . _d_n2 & o'
    , x2 $ section $ repeat 2 $ n_d_ & o'_oo
    , x2 $ section $ repeat 2 $ d_d_ & o'_oo
    -- plain dimita dimi
    -- dimita dimi + tang kitataka dhomka
    -- dimita dimi + tang kitataka dugudugu
    -- plain dimita dimi
    ]
    where
    o'oo = sd (o'.o.o.o.o.o.o.o)
    o'oo2 = sd (o'.o.o.o).o.o.sd (o.o.o)
    _oo2 = sd (__.o.o.o).o.o.sd (o.o.o)
    nddn2 = n.l.d.l.d.l.n.l.n.n.d.l.d.l.n.l
    nddn  = n.l.d.l.d.l.n.l.n.l.d.l.d.l.n.l
    nd_n  = repeat 2 $ n.y.d.yjy.n.y
    _d_n  = __.k.d.yjy.n.yjy.d.yjy.n.y
    _d_n2 = __.k.d.yjy.n.y.n.n.d.yjy.n.y
    _n_d  = __.k.n.yjy.d.yjy.n.yjy.d.y
    _n_d6 = __.k.n.yjy.d.yjy.n.y
    _n_d4 = __.k.n.yjy.d.y
    n_d_  = repeat 2 $ n.yjy.d.yjy
    d_d_ = repeat 4 $ d.yjy
    o'_oo = o'.__.o.o'.__5 .__.__.o.o'.__.__.o.__
    yjy = y.j.y

sarva_20_02_10 :: Korvai
sarva_20_02_10 = date 2020 2 10 $ ganesh $ sarvalaghu $ korvai adi
    [ x2 $ section $ (nd_n_nd_.n.d.y) & oo_o_oo_ . (nd_n_nd_.n.d.y) & (o.o')
    , section $ (nd_n_nd_.n.su ktok) & oo_o_oo_' . (nd_n_nd_.n.su ktok) & (o.o')
    , section $ (nd_n_nd_.n.su ktok) & oo_o_oo_' . (nd_n_nd_.n.pkd) & (o.o')
        . repeat 2 (d__n_nd_.n.pkd)
        . repeat 3 (d.__.y.n.y.d.pkd) . pkd.pkd.pkd.pkd
    , section $
          pkd.pk.r2 (n.pk.n.d.y).n.pk.d    .d.__.k.r2 (n.pk.n.d.y).n.su ktpk
        . n.d.y .r2 (n.pk.n.d.y).n.su ktpk .ptknpk.r2 (n.pk.n.d.y).n.su ktpk
        . ptknpk.n.y.n.d.y.ptknpk.n.y.n.d.y.ptknpk.r2 (n.pk.n.d.y).n.su ktpk
    ]
    where
    nd_n_nd_ = n.d.y.r2 (n.y.n.d.y)
    d__n_nd_ = d.__.y.r2 (n.y.n.d.y)
    pkd = pk.d
    oo_o_oo_ = o.o.__.r2 (o.__.o.o.__).o.o.__
    -- delayed gumiki: on the rest
    oo_o_oo_' = o.o'.__.r2 (o.__.o.o'.__).o'.__.__

    pk = su (p.k)
    ptknpk = su (p.t.k.n.p.k)
    r2 = repeat 2

e_20_02_24 :: Korvai
e_20_02_24 = date 2020 2 24 $ ganesh $ exercise $ korvaiS1 adi $
    tri_ (od.__4) (su ktok.t.o.su (ktok.kook))

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
