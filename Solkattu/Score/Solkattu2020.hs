-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
-- | Solkattu scores from 2020.
module Solkattu.Score.Solkattu2020 where
import           Prelude hiding ((.), (^), repeat)

import           Solkattu.Dsl.Solkattu



-- say and play with the metronome
e_20_01_27 :: Korvai
e_20_01_27 = date 2020 1 27 $ ganesh $ exercise $
    comment "practice for accurate microbeats" $ korvaiS adi mridangam $
    -- namita dimita dim sequence
    [ tang.kttk.dhom.ka.tang.kttk
    , tang.kttk.du.gu.du.gu.tang.kttk
    , tang.kttk.dhom.su (ta.di.__.ki.__.ta.__.thom)
    ]
    where
    kttk = su (kita.taka)
    mridangam = makeMridangam
        [
        ]
