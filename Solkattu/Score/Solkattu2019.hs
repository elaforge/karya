-- Copyright 2019 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
-- | Solkattu scores from 2018.
module Solkattu.Score.Solkattu2019 where
import           Prelude hiding ((.), (^), repeat)

import           Solkattu.Dsl.Solkattu


-- TODO I think I did this one before
c_19_04_15 :: Korvai
c_19_04_15 = date 2019 4 15 $ ganesh $ korvai adi mridangam $ map section
    [ join (dhom.__3)
        [ start.end
        , dropM 2 (start.end)
        , dropM 4 (start.end)
        , end, end
        , dropM 4 end, dropM 4 end
        ]
    , let end2 = end.din.tat in join (2^dhom.__4)
        [ start.end2
        , dropM 2 (start.end2)
        , dropM 4 (start.end2)
        , end2, end2
        , dropM 4 end2, dropM 4 end2
        , din.tat, din.tat
        ]
    ]
    where
    start = startS . su nakatiku
    startS = tat.__.dit.__. su (kita.ki.na.ta.ki.taka)
    end = su $ dhom.__.takita.din.__.ga.dugu.takita.din.__.ga
    mridangam = makeMridangam
        [ (startS, k.__.t.__.k.t.k.n.p.k.t.k)
        , (end, o.__.k.p.k.od.__.k.o.o.k.p.k.od.__.k)
        , (dhom, o)
        , (2^dhom, od)
        , (din.tat, o.k)
        ]

c_19_06_17 :: Korvai
c_19_06_17 = date 2019 6 17 $ ganesh $ korvaiS adi mridangam
    [ seq
    , nadai 6 seq
    ]
    where
    seq = theme 1 . theme 2 . theme 3 . tri p5 . tri p6 . tri p7
    theme n = takadinna.takita.repeat n taka.dheem.__3
    mridangam = makeMridangam
        [ (takita, n.p.k)
        , (taka, p.k)
        , (dheem, od)
        ]

-- n d d n -- 1
-- _ kd nnd d n -- ktokou kokokou k
-- okd d n knn d d n -- 2
-- _ kd nnd d n _ nd d n _ kD D N -- 3 (v ook kpkkook...)
--
-- n d d n nnd d n n
--
c_19_07_15 :: Korvai
c_19_07_15 = date 2019 7 15 $ ganesh $ korvai adi mridangam $
    let var th = section $ sarva 8 . sarva 4 . din.__4 . th in
    [ var th0a
    , var th0b
    , var th0c
    , x2 $ section $ sarva 4 . din.__4 . th0a
    , section $ sarvaD sarvaPlainS 8
    , ending $ su $
        th1 . ta.dinga
        . dropM 2 (rdropM 2 th1) . ta.dinga
        . dropM 4 (rdropM 4 th1)
        . tri (group (tri_ dinga (ta.dinga.tat.__)))
    ]
    where
    th0a = su $ tanga.tat.__.dit.__.th0
    th0b = su $ tanga.taka.tang.__.th0
    th0c = su $ tang.__.__.kttk.tat.__.th0
    th0 = taka.ta.takadinna.taka.dinga.tat.__.tat.__.tat

    th1 = tat.__.dit.__.kita.ki.na.gu.gu.takita.dinga.tat.__.dit.__
    -- Reduce from both ends:
    -- tat.__.dit.__.kita.ki.na.gu.gu.takita.dinga.tat.__.dit.__.ta.dinga
    --        dit.__.kita.ki.na.gu.gu.takita.dinga.tat.__       .ta.dinga
    --               kita.ki.na.gu.gu.taktakita.dinga
    -- ta.dinga.tat.__.dinga
    -- ta.dinga.tat.__.dinga
    -- ta.dinga.tat.__

    sarva = sarvaD sarvaS
    sarvaS = repeat 2 $ na.dimi.nami.na.dimi.nami.na.dimi.nam.kttk
    sarvaPlainS = repeat 2 $ na.dimi.nami.na.dimi.nami.na.dimi.na.dimi
    dimi = di.mi; nami = na.mi
    -- TODO din.__8 as usual when coming back into sarva
    -- TODO sarva going into ending does na.dimi instead of nam.kttk.
    mridangam = makeMridangam
        [ (sarvaS, let rh = n.d.l.n.l.n.d.l.n.l.n.d.l.n.k.t.o.k
            in rh & strM "oo_o_oo_o_oo_o" . rh & (o.o))
        , (sarvaPlainS, let rh = n.d.l.n.l.n.d.l.n.l.n.d.l.n.d.l
            in rh & strM "oo_o_oo_o_oo_oo_" . rh & (o.o))

        , (tanga.tat.__.dit, v.__.p.k.__.t)
        , (tanga.taka.tang, v.__.p.k.p.v)
        , (tang.__.__.kttk.tat, v.__.__. o.k.t.p.k)
        , (th1,   k.__.t.__.k.t.k.n.o.o.k.p.k.od.__.p.k.__.t.__)
        , (th0, k.p.k.k.o.o.k.p.k.od.__.p.k.__.k.__.k)

        , (dinga, od.__.p)
        , (din, od)
        , (ta, k)
        , (tat, k)
        ]

c_19_09_23_exercise :: Korvai
c_19_09_23_exercise = date 2019 9 23 $ ganesh $ trikalam $
    similarTo "Solkattu2017" "c_17_07_13" $ korvaiS adi mempty
    [ nadai 5 $ sd $ repeat 3 purvangam . utarangam
    ]
    where
    purvangam = group takadinna.group takadinna.group takita.group tdgnt
    utarangam = tri_ (ta.__4) $ group $ takita.tdgnt
