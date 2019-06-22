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
