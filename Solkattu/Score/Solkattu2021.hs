-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Solkattu.Score.Solkattu2021 where
import           Prelude hiding ((.), (^), repeat)

import qualified Solkattu.Score.Solkattu2017 as Solkattu2017
import qualified Solkattu.Score.Solkattu2020 as Solkattu2020
import qualified Solkattu.Score.SolkattuMohra as SolkattuMohra
import qualified Solkattu.Tala as Tala

import           Solkattu.Dsl.Solkattu


kon_21_01_24 :: Korvai
kon_21_01_24 = date 2021 1 24 $ ganesh $ exercise $
    korvaiS1 Tala.misra_chapu mempty $ nadai 3 $ mconcat $ map (g • r3)
    -- reduceToR 7 1 (tanga.tanga.takita.takadinna.__)
    [ tanga.tanga.takita.takadinna.__
    , tanga.tanga.takita.takadinna
    , tanga.tanga.takita.takita
    , tanga.tanga.tdgnt
    , tanga.tanga.takadinna
    , tanga.tanga.takita
    , tanga.tanga.taka
    , tanga.tanga.ta
    ]

kon_21_02_21 :: Korvai
kon_21_02_21 = date 2021 2 21 $ ganesh $ exercise $
    korvaiS1 Tala.kanda_chapu mempty $ nadai 3 $ mconcat $ map (g • r3)
    [ tanga.tanga.takita.takadinna.din.__.ga
    , tanga.tanga.takita.takadinna.din.__
    , tanga.tanga.takita.takadinna.__
    , tanga.tanga.takita.takadinna
    , tanga.tanga.takita.takita
    , tanga.tanga.tdgnt
    , tanga.tanga.takadinna
    , tanga.tanga.takita
    , tanga.tanga.taka
    , tanga.tanga.ta
    ]

kon_35_kanda, kon_35_misra :: Korvai
(kon_35_kanda, kon_35_misra) = (make Tala.kanda_chapu, make Tala.misra_chapu)
    where
    make tala = date 2021 2 21 $ ganesh $ exercise $ korvaiS tala mempty $
        map (nadai 3)
        [ tri_ tanga (tdktt_ .kp.tdktt_ .kpnp.tdktt_)
        , let p123 = tdktt_.kp.tdktt_.kpnp.tdktt_
          in p123.tanga.p123.tanga . kpnp.tdktt_.kp.tdktt_.tdktt_
        , trin tanga (r3 tdktt_) (r3 (kp.tdktt_)) (r3 (kpnp.tdktt_))
        , trin tanga (r3 (tdktt)) (r3 (kp.__.tdktt))
            (r3 (kp.__.tiku.__.tdktt))
        ]
    tdktt_ = g "tadi_ki_ta_thom_"
    tdktt = g "tadi_ki_ta_thom"

kon_tadit_tarikitathom :: Korvai
kon_tadit_tarikitathom = date 2021 4 25 $ ganesh $ exercise $
    korvaiS (beats 10) mempty
    [ r4 tadit
    , nadai 5 $ r5 tadit
    , nadai 6 $ r6 tadit
    , nadai 7 $ r7 tadit
    ]
    where
    tadit = g "ta_dit_tarikitathom_"

april_tani :: Score
april_tani = tani
    [ Comment "80 bpm"
    , Comment "my solo - Solkattu2020.c_20_10_25"
    , K $ slice 0 (-1) Solkattu2020.c_20_10_25
    , Comment "ganesh solo"
    , Comment "misra koraippu - Solkattu2017.c_17_10_23"
    , K koraippu_development
    , K $ slice 2 6 Solkattu2017.c_17_10_23
    , Comment "mohra korvai, together"
    , K $ index 1 SolkattuMohra.c_mohra
    , K c_mohra_korvai
    ]

-- TODO this is awkward, what I really want is to mix in mridangam notation.
-- Try inline mridangam:
koraippu_development :: Korvai
koraippu_development = ganesh $ koraippu $ korvaiS adi mridangam $ map su
    [ din.__8 . p7o.p7o.p7 . p7s.p7o.p7o.p7.takadinna.takita
    , other
    , din.__8 . p7o.p7o.p7 . takadinna.takita . p7o.p7o . ending
    , other
    , din.__8 . r4 ending
    , other
    ]
    where
    other = din.__n 64 -- TODO some notation for "other person plays"
    p7 = g $ na.__.din.__.na.din.__
    p7o = 2^p7
    p7s = 3^p7
    ending = g $ ta.dit.__.ta.__.kita . taka.__.din.__.tat.__
    mridangam = makeMridangam
        [ (p7,  on.__.d.__.n.d.__)
        , (p7o, on.__.od.__.on.od.__)
        , (p7s,  n.__.od.__.on.od.__)
        , (takita, n.p.k)
        , (din, od)
        , (ending, k.t.__.k.__.t.k . k.o.__.od.__.k.__)
        ]

c_mohra_korvai :: Korvai
c_mohra_korvai = ganesh $ mohraKorvai $ korvaiS1 adi mridangam $ mconcat
    [ g $ ta.__.dit.__.ta.din.__.ta.__.din.__.p5.__
    , g $       din.__.ta.din.__.ta.__.din.__.p5.__
    , nadai 6 $
        r2 (g (ta.din.__.ta.__.din.__.p5))
        . g (ta.din.__.ta.__.din.__)
        . tri_ __ (g p5)
    ]
    where
    mridangam = makeMridangam
        [ (ta, k)
        , (dit, t)
        , (din, od)
        ]

e_21_04_25 :: Korvai
e_21_04_25 = date 2021 4 25 $ ganesh $ korvaiS adi mridangam
    [ purvangam -- 7*24 = 5*8 + 7*8 + 9*8
        . spread32111 tdgnt . spread32111 (taka.tdgnt)
        . spread32111 (taka.tiku.tdgnt)
    , purvangam -- 7*24 = 28 17 7*1 + 28 14 7*2 + 28 14 7*3
        . sd tdgnt3 . tdgnt3 . t7
        . sd tdgnt3 . tdgnt3 . r2 t7
        . sd tdgnt3 . tdgnt3 . r3 t7
    , purvangam -- 7*24 = {6..2}111 * 7 + 7 in between = 21 + 2 + 1 = 24
        . join ta
        [ ta.__n 12.din.__n 12 . r3 (g $ ta.nam.su "kitatarikita".thom) -- 6
        , ta.__n 10.din.__n 10 . r3 (g $ nam.su "kitatarikita".thom) -- 5
        , ta.__8   .din.__8    . r3 (g $ su "kitatarikita".thom) -- 4
        , ta.__6   .din.__6    . r3 (g $ su "tarikita".thom) -- 3
        , ta.__4   .din.__4    . r3 (g $ su kita.thom) -- 2
        ] . __ . tri_ __ p7
    ]
    where
    purvangam = sd $ tri_ (din.__3) (g (ta.__3.ta.takadinna))
        . sandi (ta.takadinna) (tri_ (din.__) (g (ta.takadinna)))
    spread32111 seq = g (spread 3 seq) . g (spread 2 seq) . r3 (g seq)
    tdgnt3 = g $ ta.__3.din.__3.gin.__3.na.__3.thom.__
    t7 = g $ tat.__.dit.__.su tarikita.thom
    mridangam = makeMridangam
        [ (din, od)
        , (ta, k)
        , (taka, k.p)
        , (tiku, n.p)
        , (t7, "k_t_ktkto")
        , (ta.nam, p.n)
        , (nam, n)
        , ("kitatarikita".thom, "ktpkpto")
        , ("tarikita".thom, "ktkto")
        , (kita.thom, "kto")
        ]
