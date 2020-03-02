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
    comment "practice for accurate microbeats" $ korvaiS adi mempty $
    -- namita dimita dim sequence
    [ tri_ (din.__.dit.taka) (tri (tang.kttk.dhom.ka) . tang.kttk)
    , tri_ (din.__.dit.taka) (tri (tang.kttk.su (dugu.dugu)) . tang.kttk)
    , tri_ (din.__.dit.taka)
        (tang.kttk . tri (dhom.su (ta.di.__.ki.__.ta.__.thom)))
    ]
    where
    kttk = su (kita.taka)
