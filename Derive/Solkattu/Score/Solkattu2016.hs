-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
-- | Korvais from 2016.
module Derive.Solkattu.Score.Solkattu2016 where
import Prelude hiding ((.), (^), repeat)

import Derive.Solkattu.SolkattuGlobal


c_16_09_28 :: Korvai
c_16_09_28 = date 2016 9 28 $ ganesh $
        similar_to "Solkattu2016" "c_16_12_06_sriram1" $
        korvai1 adi mridangam $ su $
    tat.__.dit.__.kitakina . nakatiku . tri_ __ (na.ka.takadinna.dheem) . __6
          .dit.__.kitakina . nakatiku . tri_ __       (takadinna.dheem) . __6
                 .kitakina . nakatiku . tri_ __            (taka.dheem) . __6
    . tri (p6 . ta.ka.p6 . ta.ka.na.ka.p6)
    where
    kitakina = ki.ta.ki.na.ta.ki.ta.ka
    mridangam = make_mridangam
        [ (tat.dit, [k&p, t])
        , (dit, [k])
        , (kitakina, [k, t, k, n, o, k, t&o, k])
        , (na.ka, [n, p])
        , (ta.ka.dheem, [p, k, o&d])
        , (ta.ka, [k, p])
        , (ta.ka.na.ka, [k, p, n, p])
        , (dheem, [od])
        ]

c_16_12_06_sriram1 :: Korvai
c_16_12_06_sriram1 = date 2016 12 6 $ source "sriram" $ korvai adi mridangam $
    map su $ map (purvangam.)
    [ tri_ (dheem.__4) ta_takadin
    , tri_ (dheem.__4) (su $ p6 . tk.p6 . tknk.p6)
    -- All variations can get taka and takanaka in the karvai, and
    -- 345 345 345 can become 345 345 3333
    ]
    where
    purvangam =
        1^tat.__.1^dit.__ . su (kitakina.nakatiku . tri (dinnaka.takadinna))
            . din.din.__.tat.tat.__.thom.__
                .1^dit.__ . su (kitakina.nakatiku . tri dinnaka)
            . din.__.tat.__.thom.__
                          . su (kitakina.nakatiku . tri takadinna)
            . din.tat.thom.__

    dinnaka = din.na.ka.din.na.ka.ta.ka
    kitakina = ki.ta.ki.na.ta.ki.ta.ka

    ta_takadin = mconcat $ expand 3 1 (tat.dit.ta . su taka . din)
    mridangam = make_mridangam
        [ (1^tat, [p&k])
        , (1^dit, [p&t])
        , (kitakina, [k, t, k, n, o, k, o&t, k])
        , (dinnaka, [o, k, t, o, hv k, t, o, k])
        , (din, [o])
        , (tat, [k])

        , (tat.dit.ta.taka.din, [k, t, k, k, t, o])
        , (dheem, [u])
        ]
