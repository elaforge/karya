-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Solkattu.Score.Mridangam2015 where
import Prelude hiding ((.), repeat)

import qualified Solkattu.Tala as Tala

import Solkattu.Dsl.Mridangam


c_1 :: Korvai
c_1 = akash $ date 2015 1 1 $ tekha $
    comment "rupak hindustani = 2 * misra chapu" $
    korvaiS Tala.misra_chapu
    [ sd $ i. __.i. __.n. __.od.__.  n.__.od.__. n. __
    , sd $ p&i.k.p&i.__.n.k .od.o&k.p&n.k.od.o&k.p&n.k
    , sd $ od.__.k.od.__.k  .od.o&k.p&n.k.od.o&k.p&n.k
    , sd $ on.su (kt.o.k).on.on.su (kt.p.k).n.su (kt.p.k).on.on.su (kt.o.k)
    ]

c_2 :: Korvai
c_2 = akash $ date 2015 1 1 $ tekha $
    korvaiS adi $ map (nadai 6 • sd)
    [ r2 $ r2 (n.d.n.n.d.k) & (o.__.o.o.p.__ . __.__.o.o.__.__)
    , r3 (on.ktpk.p&n.ktok) . on.p&n.__.on.p&n.__
    ]
    where
    ktpk = su (k.t.p.k)
    ktok = su (k.t.o.k)

c_3 :: Korvai
c_3 = akash $ date 2015 1 1 $ tekha $ comment "14 beats" $
    korvaiS Tala.misra_chapu
    [ sd $ on.od.__.on.on.od.__.n.p&d.__.on.on.od.__
    , sd $ r2 (n.d.l.n.l.d.l) & (o.__3.o.__.o.__.p.__3.o.__.o.__)
    , sarvaD_ 5 . r2 (nadai 6 (n.p.kt.pk))
    ]

akash1 :: Korvai
akash1 = akash $ date 2015 1 1 $ korvaiS adi
    [ r2 $ rh & "o__o_oo_" . rh & "p__p_p__"
    , rh & "o__o_oo_" . su ktpk . "ndldnl" & "_p_p"
        . fill . su ktpk . "ndldnl" & "_p_p"
    ]
    where
    fill = g $ od.k.k.od.k.k.od.__
    rh = "dlndldnl"

akash :: Korvai -> Korvai
akash = source "akash"

tekha :: Korvai -> Korvai
tekha = withType "tekha"
