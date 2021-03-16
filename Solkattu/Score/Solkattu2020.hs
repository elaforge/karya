-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
-- | Solkattu scores from 2020.
module Solkattu.Score.Solkattu2020 where
import           Prelude hiding ((.), (^), repeat)

import qualified Solkattu.Instrument.KendangPasang as KendangPasang
import qualified Solkattu.Instrument.KendangTunggal as KendangTunggal
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
    SolkattuMohra.mohraKorvai adi mridangam
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

c_20_10_25 :: Korvai
c_20_10_25 = date 2020 10 25 $ ganesh $ korvaiS1 adi (mridangam<>k1<>k2) $
    mconcat
    [g (r4 (ta.dit.__.ta.__4).ta.di.__.ki.__.ta.__.taka.__.din.__.na.__.dim.__7)
    , g (r4 (ta.dit.__.ta.__3) . ta.di.__.ki.ta.__ . taka.__.din.na.__.dim.__6)
    , g (r4 (ta.dit.__.ta.__) . ta.di.ki.ta.__.taka.din.na.__.dim.__5)
    , g (r4 (2^takadinna) . ta.di.ki.ta.takadinna.dim.__4)

    , g (ta.__3.di.__4.ki.__3.ta.__4.thom.__)
    , g (ta.__.di.__4.ki.__.ta.__4.thom.__)
    , g (ta.__.di.__3.ki.__.ta.__3.thom.__)

    , sd p5 . p5 . sd p5 . r2 p5 . sd p5
    , tri_ __ (r3 p5)
    ]
    where
    mridangam = mridangam3
    mridangam2 = makeMridangam
        [ (ta.dit.ta.ta.dit.ta, k.t.o.k.n.p)
        , (2^takadinna . 2^takadinna, k.t.o.k.n.p.k.p)
        , (ta.di.ki.ta, k.t.k.n)
        , (dim, od)
        , (thom, o)
        ]
    mridangam3 = makeMridangam
        [ (ta.dit.ta.ta.dit.ta, k.t.k.k.n.o)
        , (2^takadinna . 2^takadinna, k.t.k.o.k.n.p.k)
        , (ta.di.ki.ta, k.t.k.n)
        , (dim, od)
        , (thom, o)
        ]
    mridangam1 = makeMridangam
        [ (ta.dit.ta, k.t.k)
        , (2^takadinna, k.t.k.t)
        , (ta.di.ki.ta, k.t.k.n)
        , (dim, od)
        , (thom, o)
        ]
    k1 = makeKendang1
        [ (ta.dit.ta, t.p.t)
        , (ta.di.ki.ta, t.p.k.t)
        , (dim, a)
        , (thom, a)
        ] where KendangTunggal.Strokes {..} = KendangTunggal.notes
    k2 = makeKendang2
        [ (ta.dit.ta, t.l.t)
        , (ta.di.ki.ta, l.k.p.t)
        , (dim, a)
        , (thom, a)
        ] where KendangPasang.Strokes {..} = KendangPasang.notes

-- TODO the same as Kendang2020.farans, except this has the unnecessary sollus,
-- but can also more easily do kendang pasang... so I'm not sure which is
-- better!
kendang_farans :: Korvai
kendang_farans = faran $ korvaiS adi (k1<>k2) $ map su $ concat
    [ map (make (taka.naka) (taka.naka.tiku))
        [ ktknpktk
        , ooknpktk
        , otknpktk
        , od__odnpktk
        ]
    ]
    where
    ktknpktk = kita.ki.na.takitaka
    ooknpktk = dhom.dhom.ta.na.takitaka
    otknpktk = dhom.ta.ka.na.takitaka
    od__odnpktk = din.__.din.na.takitaka
    takitaka = ta.ki.taka
    k1 = makeKendang1
        [ (taka.naka, k.t.k.p) -- alt: p.k.t.k (p.n.p.k)
        , (taka.naka.tiku, k.t.k.p.k.p) -- alt: p.k.t.k.p.k (p.n.p.k . t.k)

        , (ktknpktk, k.p.k.t.p.k.a.k)
        , (ooknpktk, a.a.k.t.p.k.a.k)
        , (otknpktk, a.p.k.t.p.k.a.k)
        , (od__odnpktk, o.__.o.t.p.k.a.k)
        ] where KendangTunggal.Strokes {..} = KendangTunggal.notes
    k2 = makeKendang2
        [ (taka.naka, k.p.t.l)
        , (taka.naka.tiku, k.p.t.l.k.p)

        , (ktknpktk, k.p.t.l.k.p.a.o)
        , (ooknpktk, a.a.p.l.k.p.a.o)
        , (otknpktk, a.k.p.l.k.p.a.o)
        , (od__odnpktk, o.__.o.l.k.p.a.o)
        ] where KendangPasang.Strokes {..} = KendangPasang.notes
    make fill1 fill2 pattern =
        long . long
        . group pattern . group pattern . long
        . r2 short . fill1 . long
        . r3 short . fill2 . nakatiku
        where
        long = group pattern . nakatiku
        short = takeM 6 pattern
