-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
-- | Solkattu scores from 2020.
module Solkattu.Score.Solkattu2020 where
import           Prelude hiding ((.), (^), repeat)

import qualified Solkattu.Score.SolkattuMohra as SolkattuMohra

import           Solkattu.Dsl.Solkattu


-- say and play with the metronome
e_20_01_27 :: Korvai
e_20_01_27 = date 2020 1 27 $ ganesh $ exercise $
    comment "practice for accurate microbeats" $ korvaiS adi mempty
    -- namita dimita dim sequence
    [ tri_ (din.__.dit.taka) (tri (tang.kttk.dhom.ka) . tang.kttk)
    , tri_ (din.__.dit.taka) (tri (tang.kttk.su (dugu.dugu)) . tang.kttk)
    , tri_ (din.__.dit.taka)
        (tang.kttk . tri (dhom.su (ta.di.__.ki.__.ta.__.thom)))
    ]

c_20_04_03 :: Korvai
c_20_04_03 = date 2020 4 3 $
    source "https://www.youtube.com/watch?v=wgI6uvtkao4" $
    SolkattuMohra.makeMohraKorvai adi mridangam
        su (t14, t16, t18) (end1, end2, end3)
        (purvangam . utarangam)
    where
    t14 = theme ø
    t16 = theme tat
    t18 = theme (tat.tat)
    theme inter = group $
          inter . su (kita.taka).tat.__.kita.su tarikita.thom
        . inter . su (kita.taka.tarikita).thom

    end1 = talanga.din.tat.tam.__3 . su kp.talanga.din.tat.tam.__4
    end2 = talanga.din.tat.tam.__4
    end3 = talanga.din.tat . talanga.din.tat.tam.__3 . su kp
        .  talanga.din.tat . talanga.din.tat.tam.__2 . su kpnp
        .  talanga.din.tat . talanga.din.tat
    talanga = su $ ta.lang.__.ga

    purvangam = -- 4:43
        t14 . tri (ta.__.ka.thom.__)
        . t16 . tri (ta.__.thom.__)
        . t18 . tri (ta.thom.__)
    utarangam = trin (tri tanga) -- 5:54, optionally, only play first
        (tri (g (tadingina . tadin_ginaka))) -- 7
        (tri (g (tadingina . r2 tadin_ginaka))) -- 10
        (tri (g (tadingina . r3 tadin_ginaka))) -- 13
        where
        tadingina = ta.din.gin.na
        tadin_ginaka = su (ta.din.__.gin.na.ka)
        g = group
    mridangam = makeMridangam -- 7:18
        [ (kita.taka.tat, p.k.t.p.v)
        , (kita.tari.kita.thom, k.t.k.t.k.t.o)
        , (kita.taka.tarikita.thom, p.k.t.p.v.p.k.t.o)
        , (tat, k)
        , (talang.ga.din.tat, p.u.k.o.k)
        , (tam, od)
        , (taka.thom, k.o.od)
        , (ta.thom, k.od)
        , (ta.din.gin.na, k.t.k.n)
        , (ta.din.gin.na.ka, k.t.k.n.o)
        , (tang.__.ga, o&u.__.__)
        ]
