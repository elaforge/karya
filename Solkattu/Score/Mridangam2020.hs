-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Solkattu.Score.Mridangam2020 where
import Prelude hiding ((.), repeat)
import Solkattu.Dsl.Mridangam


sarva_20_01_27 :: Korvai
sarva_20_01_27 = date 2020 1 27 $ ganesh $ sarvalaghu $ korvai adi $ map section
    [ nddn2 & o'oo2 . nddn & o' -- x4
    , (__.k) `replaceStart` nddn2 & o'oo2 . nddn & o'
    , (__.k) `replaceStart` nd_n  & o'oo  . nd_n & o'
    , _d_n & o'oo . _d_n & o'
    , _d_n2 & o'oo2 . _d_n2 & o'
    , _n_d & o'oo . _n_d & o'
    , repeat 2 (_n_d6 & sd (o'.o.o.o.o.o) . _n_d6 & o')
        . repeat 4 (_n_d4 & sd (o'.o.o.o) . _n_d4 & o')
        . repeat 4 (o'.k.d.y)
    , repeat 2 $ n_d_ & o'_oo
    , repeat 2 $ d_d_ & o'_oo
    -- transition to namita dimita dimi
    ]
    where
    o'oo = sd (o'.o.o.o.o.o.o.o)
    o'oo2 = sd (o'.o.o.o).o.o.sd (o.o.o)
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
