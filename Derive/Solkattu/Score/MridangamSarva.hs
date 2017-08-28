-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Sarvalaghu.
module Derive.Solkattu.Score.MridangamSarva where
import Prelude hiding ((.), repeat)

import Derive.Solkattu.MridangamGlobal
import Global


-- * kirkalam

-- TODO these don't need to be a full avartanam, only a binary factor of it

kir1 :: Korvai
kir1 = sarvalaghu $ sudhindra $ korvai adi $
    [ repeat 4 $ repeat 2 (n.l.d.d) & (o.__.o.o.__.o.o.__) -- takadimi takajonu
    ]

kir2 :: Korvai
kir2 = sarvalaghu $ sudhindra $ korvai adi $
    repeat 2 sarva : map pattern prefixes
    where
    pattern (prefix, end) =
        (repeat 2 $ prefix `replaceStart` sarva) `replaceEnd` end
    -- takatadin or nakanadin
    sarva = repeat 2 (n.k.n.d) & (o.__.o.o.__.__.o.o) . (on.k.n.d) . (n.k.n.d)
    prefixes = map (su *** su)
        [ (takadinna, takadinna . repeat 3 (t.o.o.k))
        , (o.o.n.n . o.k, repeat 4 (o.o.n.n))
        , (o.o.k.t . p.k, repeat 4 (o.o.k.t))
        , (n.n.p.k, repeat 4 (n.n.p.k))
        , (p.u.__.k, repeat 4 (p.u.__.k))
        , (dinna_kitataka, repeat 2 dinna_kitataka . o.k.o.k . dinna_kitataka)
        , let nknk = o&j.y.o&j.y
            in (nknk, nknk.d.__.nknk.d.__.nknk)
        ]
    dinna_kitataka = o.n . su (k.t.o.k)

kir3 :: Korvai
kir3 = sarvalaghu $ sudhindra $ korvai1 adi $ repeat 2 $
    repeat 2 (n.d.__.n) & (o.o.__.o.__.o.__.o)
    . repeat 2 (n.d.__.n) & (o.__n 8)
    -- can end with faran: oonnpktk naka

kir4 :: Korvai
kir4 = sarvalaghu $ sudhindra $ korvai1 adi $
      on.__.on.__.on.od.__5.on.__.on.od.__2.o
    . on.k.on.k.on.od.__5.on.k.on.od.__2.o


-- * melkalam

mel1 :: Korvai
mel1 = sarvalaghu $ sudhindra $ korvai1 adi $
    repeat 4 $ on.od.on. su (pk.n.o).od.on . su pk
    -- ta din ta din takadin ta din

mel2 :: Korvai
mel2 = sarvalaghu $ sudhindra $ korvai1 adi $ su $
    repeat 2 $ repeat 3 (yjyj.d.__.lt p.k) . (t.k.o.o.k.t.o.k)
    where yjyj = y.j.y.j


