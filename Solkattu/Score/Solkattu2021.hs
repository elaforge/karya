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
