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


e_21_01_24 :: Korvai
e_21_01_24 = date 2021 1 24 $ ganesh $ exercise $
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

e_21_02_21 :: Korvai
e_21_02_21 = date 2021 2 21 $ ganesh $ exercise $
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

e_35_kanda, e_35_misra :: Korvai
(e_35_kanda, e_35_misra) = (make Tala.kanda_chapu, make Tala.misra_chapu)
    where
    make tala = date 2021 2 21 $ ganesh $ exercise $ korvaiS tala mempty $
        map (nadai 3)
        [ tri_ tanga (tdktt .kp.tdktt .kpnp.tdktt)
        , let p123 = tdktt.kp.tdktt.kpnp.tdktt
          in p123.tanga.p123.tanga . kpnp.tdktt.kp.tdktt.tdktt
        , trin tanga (r3 (tdktt)) (r3 (kp.tdktt)) (r3 (kpnp.tdktt))
        , trin tanga (r3 (tdktt')) (r3 (kp.__.tdktt'))
            (r3 (kp.__.tiku.__.tdktt'))
        ]
    tdktt = g $ ta.di.__.ki.__.ta.__.thom.__
    tdktt' = g $ ta.di.__.ki.__.ta.__.thom

april_tani :: Score
april_tani = tani
    [ Comment "80 bpm"
    , Comment "my solo: kt k _ - Solkattu2020.c_20_10_25"
    , K Solkattu2020.c_20_10_25
    , Comment "ganesh solo"
    , Comment "misra koraippu - Solkattu2017.c_17_10_23"
    , K $ slice 0 6 Solkattu2017.c_17_10_23
    , Comment "mohra korvai, together"
    , K $ index 1 SolkattuMohra.c_mohra
    , K c_mohra_korvai
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
