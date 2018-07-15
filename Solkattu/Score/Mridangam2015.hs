-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Solkattu.Score.Mridangam2015 where
import Prelude hiding ((.), repeat)

import Solkattu.MridangamGlobal
import qualified Solkattu.Tala as Tala


c_1 :: Korvai
c_1 = akash $ date 2015 1 1 $ tekha $
    comment "roopak hindustani = 2 * misra chapu" $
    korvai Tala.misra_chapu $ map section
    [ sd $ i. __.i. __.n. __.od.__.  n.__.od.__. n. __
    , sd $ p&i.k.p&i.__.n.k .od.o&k.p&n.k.od.o&k.p&n.k
    , sd $ od.__.k.od.__.k  .od.o&k.p&n.k.od.o&k.p&n.k
    , sd $ on.su (kt.o.k).on.on.su (kt.p.k).n.su (kt.p.k).on.on.su (kt.o.k)
    ]
    where

c_2 :: Korvai
c_2 = akash $ date 2015 1 1 $ tekha $
    korvai adi $ map section $ map (nadai 6 • sd)
    [ repeat 2 $ repeat 2 (n.d.n.n.d.k) & (o.__.o.o.p.__ . __.__.o.o.__.__)
    , repeat 3 (on.ktpk.p&n.ktok) . on.p&n.__.on.p&n.__
    ]
    where
    ktpk = su (k.t.p.k)
    ktok = su (k.t.o.k)

c_3 :: Korvai
c_3 = akash $ date 2015 1 1 $ tekha $ comment "14 beats" $
    korvai Tala.misra_chapu $ map section
    [ sd $ on.od.__.on.on.od.__.n.p&d.__.on.on.od.__
    , sd $ repeat 2 (n.d.l.n.l.d.l) & (o.__3.o.__.o.__.p.__3.o.__.o.__)
    , sarvaD 5 . repeat 2 (nadai 6 (n.p.kt.pk))
    ]

akash :: Korvai -> Korvai
akash = source "akash"

tekha :: Korvai -> Korvai
tekha = withType "tekha"
