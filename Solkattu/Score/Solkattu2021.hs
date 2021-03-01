-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Solkattu.Score.Solkattu2021 where
import           Prelude hiding ((.), (^), repeat)

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
