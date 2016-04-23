-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Korvais expressed in SolkattuDsl.
module Derive.Call.India.SolkattuScore where
import Prelude hiding ((-), repeat)

import qualified Derive.Call.India.Solkattu as Solkattu
import Derive.Call.India.SolkattuDsl


-- * chatusra nadai

chatusram1_2 :: Korvai
chatusram1_2 = check $ Solkattu.korvai (adi 4) chatusram1_mridangam $
      theme 0 - p5
    - dropM 2 (theme 1) - p6 - p6
    - dropM 4 (theme 2) - tri p7 - tri p6 - tri p5
    where
    theme gap = ta - __ - dit - __ - ta - ka - din - na - din
        - tri (ta - __n gap - din - __)

chatusram1_3 :: Korvai
chatusram1_3 = check $ Solkattu.korvai (adi 4) chatusram1_mridangam $
      theme 0 - p5 - tadin
    - dropM 3 (theme 1) - p6 - p6 - tadin
    - dropM 6 (theme 2) - sep tadin [tri p7, tri p6, tri p5]
    where
    theme gap = ta - __2 - dit - __2 - ta - ka - din - na - din
        - tri (ta - __n gap - din - __2)
    tadin = ta - __ - din - __ - __

chatusram1_4 :: Korvai
chatusram1_4 = check $ Solkattu.korvai (adi 4) mridangam $
      theme 0 - pat7 - st u dheem - __3
    - dropM 4 (theme 1) - repeat 2 pat8 - st u dheem - __3
    - dropM 8 (theme 2) - sep (st i dheem - __3) [tri pat9, tri pat8, tri pat7]
    where
    theme gap = ta - __3 - dit - __3 - ta - ka - din - na - din
        - tri (ta - __n gap - din - __3)
    pat7 = ta - ka - p5
    pat8 = ta - ka - __ - p5
    pat9 = ta - __ - ka - __ - p5
    mridangam = chatusram1_mridangam ++
        [ (ta - ka, [k, p])
        ]

chatusram1_mridangam :: [(Sequence, [Maybe Stroke])]
chatusram1_mridangam =
    [ (ta - dit, [k, t])
    , (dit, [k])
    , (ta - ka - din - na - din, [k, o, o, k, o])
    , (ta - din, [k, od])
    ]

-- * kanda nadai

kandam1_variations :: [Korvai]
kandam1_variations = [kandam1 p g | g <- gaps, p <- [pat 5, pat 6, pat 7]]
    where gaps = [thom - __ - ta - __, thom - __2, thom - __, thom, mempty]

kandam1 :: Sequence -> Sequence -> Korvai
kandam1 pt gap = check $ Solkattu.korvai (adi 5) mridangam $
      at0 - ta - __ - di - __ - ki - ta - __ - thom - __ - ta - din_ - pt
    - atX - ta - ka - di - __ - ki - ta - __ - thom - __ - ta - din_ - pt
    - at0 - ta - __ - di - __ - ki - ta - __ - gap
    -       ta - ka - di - __ - ki - ta - __ - gap
    - case duration pt of
        5 -> p567
        6 -> p666
        _ -> p765
    where
    mridangam =
        [ (ta - di - ki - ta, [k, t, k, n])
        , (ta - ka - di - ki - ta, [k, p, t, k, n])
        , (ta - din, [k, od])
        ]

adi :: Matras -> Solkattu.Tala
adi = Solkattu.adi_tala
