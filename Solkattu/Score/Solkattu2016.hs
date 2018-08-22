-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
-- | Korvais from 2016.
module Solkattu.Score.Solkattu2016 where
import Prelude hiding ((.), (^), repeat)

import Solkattu.SolkattuGlobal


c_16_09_28 :: Korvai
c_16_09_28 = date 2016 9 28 $ ganesh $
    similarTo "Solkattu2016" "c_16_12_06_sriram1" $
    korvaiS1 adi mridangam $ su $
        group tat_dit_ . tri_ __   (group nakataka) . __6
    . dropM 2 tat_dit_ . tri_ __ (dropM 2 nakataka) . __6
    . dropM 4 tat_dit_ . tri_ __ (dropM 4 nakataka) . __6

    . tri (p6 . kp.p6 . kpnp.p6)

    -- TODO the old one kind of looks nicer though...
    -- tat.__.dit.__.kitakina . nakatiku . tri_ __ (na.ka.takadinna.dheem) . __6
    --       .dit.__.kitakina . nakatiku . tri_ __       (takadinna.dheem) . __6
    --              .kitakina . nakatiku . tri_ __            (taka.dheem) . __6
    -- . tri (p6 . kp.p6 . kpnp.p6)
    where
    tat_dit_ = tat.__.dit.__.kitakina . nakatiku
    nakataka = na.ka.takadinna.dheem

    kitakina = ki.ta.ki.na.ta.ki.ta.ka
    mridangam = makeMridangam
        [ (tat.dit, p&k.p&t)
        , (kitakina, k.t.k.n.o.k.t&o.k)
        , (nakataka, n.p.k.o.o.k.od)
        ]

c_16_12_06_sriram1 :: Korvai
c_16_12_06_sriram1 = date 2016 12 6 $ source "sriram" $ korvaiS adi mridangam $
    map su $ map (purvangam.)
    [ tri_ (dheem.__4) ta_takadin
    , tri_ (dheem.__4) (su $ p6 . kp.p6 . kpnp.p6)
    -- All variations can get kp and kpnp in the karvai, and
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

    dinnaka = group $ din.na.ka.din.na.ka.ta.ka
    kitakina = group $ kita.ki.na.ta.kita.ka

    ta_takadin = mconcat $ expand 3 1 tat_dit
    tat_dit = tat.dit.ta . su taka . din
    mridangam = makeMridangam
        [ (1^tat, p&k)
        , (1^dit, p&t)
        , (kitakina, k.t.k.n.o.k.o&t.k)
        , (dinnaka, o.k.t.o.hv k.t.o.k)
        , (takadinna, n.o.o.k)
        , (din, o)
        , (tat, k)
        , (tat_dit, k.t.k.k.t.o)
        , (dheem, u)
        , (mid^dheem, i)
        , (thom, od)
        ]

c_16_12_06_sriram2 :: Korvai
c_16_12_06_sriram2 =
    date 2016 12 6 $ sriram $ korvaiS1 adi mridangam $ nadai 7 $
      repeat 2 takadinnanakita . f1       . din.__7
    . repeat 2 takadinnanakita . f1.f2    . din.__7
    . repeat 2 takadinnanakita . f1.f2.f3 . 2^din.__7
    . tri (p5.tam.__ . p5.tam.__.tam.__ . p5)
    where
    takadinnanakita = group $ takadinna.na.kita
    f1 = group $ din.__.ta.din.__.tat.__
    f2 = group $ takita.din.__.tat.__
    f3 = group $ gu.gu.ta.din.__.tat.__
    mridangam = makeMridangam
        [ (na.kita, n.p.k)
        , (f1, od.k.d.k)
        , (f2, n.p.k.d.k)
        , (f3, o.o.k.d.k)
        , (din, od)
        , (2^din, p&u)
        , (tam, u)
        ]

c_16_12_06_janahan1 :: Korvai
c_16_12_06_janahan1 = date 2016 12 6 $ janahan $ korvaiS1 adi mridangam $ su $
    tri (mconcat (expand 3 2 theme)) . trin __ (tri p5) (tri p6) (tri p7)
    where
    theme = takadinna.takita.din.__.na
    mridangam = makeMridangam
        [ (theme, on.k.o&t.k.on.p.k.od.o)
        ]

c_16_12_06_janahan2 :: Korvai
c_16_12_06_janahan2 = date 2016 12 6 $ janahan $ korvaiS1 adi mridangam $ su $
    tri theme . tri (dropM 2 theme) . tri (dropM 4 theme)
        . spread 2 tdgnt . p6
        . spread 2 tdgnt . p6 . kp.p6
        . spread 2 tdgnt . p6 . kp.p6 . kpnp.p6
    where
    theme = group $ tat.__.dit.__.takadinna
    mridangam = makeMridangam [(theme, k.t.k.o.o.k)]
