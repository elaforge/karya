-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
module Solkattu.Score.MridangamTirmanam where
import           Prelude hiding ((.), repeat)

import qualified Solkattu.Tala as Tala

import           Solkattu.Dsl.Mridangam


tir_short_adi :: Korvai
tir_short_adi = tirmanam $ korvaiS adi
    [ __D 1 . tri_ (p&u.__.k.k.o) (on.d.__.on.d.__) . p&u
    , __D 4 . su (tri_ (u.__4) (k.n.o.od.__.od.__3)) . u
    , __D 4 . __ . tri (od.__ . su2 (p.k.n.p).k.__) . od
    , __D 7 . tri_ (od.__.k) (su (ktkt.p.kt.p.hv k.t.kt.p.k).od.od.k) . od
    ]

tir_long_adi :: Korvai
tir_long_adi = tirmanam $ korvaiS adi
    -- 1:08:07
    [ let p8 = group (mconcatMap (.__) [kt, k.n, k.o, o.k])
        in __D 4.5 . p8 . (od.__.k.k.o) . p8 . (od.__.on.o'&n.k)
            . (group (su (stride 3 (k.t.k.n.k.o.o.k))))
        . od
    ]

-- sam to sam
tir_sam_adi_kirkalam :: Korvai
tir_sam_adi_kirkalam = tirmanam $ korvaiS adi
    [ tri_ "D__k" "kookokk_"
    ]

tir_long_rupaka :: Korvai
tir_long_rupaka = tirmanam $ korvaiS Tala.rupaka_fast
    -- 20:06
    [ tri_ (od.__.k) (su (ktkt.p.kt.p.hv k.t.kt.p.k).od.od.k)
    ]
