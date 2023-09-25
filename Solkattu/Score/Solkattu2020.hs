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
c_20_10_25 = date 2020 10 25 $ ganesh $ korvai adi (mridangam<>k1<>k2) $
    map (fmap su)
    -- preparation
    [ s $ sarva 13 . sd (__.cham.cham.__4)
    , s $ sarva 6 . su (kitataka.talanga.taka.taka.talanga)
        . sarva 6 . su (kitataka.takataka.takataka.takataka)
    , s $ sarva 5.5 . ending7
    , s $ sarva 6 . su (tam.__.kita.takadinna.tam.__.kita.takadinna)
        . sarva 6 . su (__5.cham.__4.__.__.ta.__.takadinna)

    -- development
    , s $ sarva 5.5 . ending7
    , s $ sarva 7 . ending6
    , endOn 4 $ s $ sarva 8.5 . ending5.__8 . sarva 6

    -- korvai
    , startOn 4 $ ending korv
    , ending $ nadai 6 korv
    ]
    where
    -- structure:
    -- 7*7 + 7*6 + 7*5 + 7*4 = 22*7 = 154
    -- 34342 + 24242 + 23232 = 16 + 14 + 12 (8 + 7 + 6) = 42
    -- 10 (5) 10 (5 5) 10 = 45
    -- 555 (1) 555 (1) 555 = 45+2 = 47
    -- = 154 + 42 + 45 + 47 = 288 / 4 =  72 / 8 = 9 avartanams
    korv = mconcat
        [ g $ ending7.__7
        , g $ ending6.__6
        , g $ ending5.__5
        , g $ r4 (ta.dit.ta.ga) . tadikita .takadinna.dim.__4
        , g (ta.__3.din.__4.gin.__3.na.__4.thom.__)
        , g (ta.__. din.__4.gin.__. na.__4.thom.__)
        , g (ta.__. din.__3.gin.__. na.__3.thom.__)

        , sd p5 . p5 . sd p5 . r2 p5 . sd p5
        , tri_ __ (r3 p5)
        ]
    ending7 = r4 "tadit_ta_ga_" . "tadi_ki_ta_taka_din_na_dim"
    ending6 = r3 "tadit_ta_ga" . "tadit_taga_" . "tadi_kita_taka_dinna_dim"
    ending5 = r4 "tadittaga_" . "tadikita_takadinna_dim"
    takataka = taka.taka
    sarva = sarvaD (sd sarva_ndd)
    sarva_ndd = na.din.din.na.su (na.na).din.din.na . r2 (na.din.din.na)
    mridangam = mridangam3
    -- TODO: awkward, replace with inline mridangam:
    mridangamSollus =
        [ (kitataka.talanga.taka.taka.talanga,
            k.t.o.k.o.u.__.k.o.k.o.k.o.u.__.k)
        , (kitataka.takataka.takataka.takataka,
            k.t.o.k. o.t.o.k .o.t.o.k .o.t.o.k)
        , (tam.__.kita.takadinna.tam.__.kita.takadinna,
            o.__.k.n.o.o.k.n.o.__.k.n.o.o.k.n)
        , (ta, k)

        , (sarva_ndd, on.od.od.on . on.on.od.od.on . on.d.d.n . n.od.od.on)
        -- TODO some syntax to say od if there is space afterwards
        -- , (takadinna, k.o.o& (_ d) .d)
        ]
    mridangam3 = makeMridangam $ mridangamSollus ++
        [ (r4 $ ta.dit.ta.ga,
            hv k.t.k.p . hv t.k.n.p . hv k.n.o.k . hv o.u.p.k)
        , (tadikita, hv k.t.k.t)
        , (dim, od)
        , (thom, o)
        ]
    mridangam2 = makeMridangam $ mridangamSollus ++
        [ (r2 $ ta.dit.ta.ga, k.t.k.lt p . k.n.o.lt o)
        , (tadikita, k.t.k.n)
        , (dim, od)
        , (thom, o)
        ]
    mridangam1 = makeMridangam $ mridangamSollus ++
        [ (ta.dit.ta.ga, k.t.k.lt k)
        , (tadikita, k.t.k.n)
        , (dim, od)
        , (thom, o)
        ]
    k1 = makeKendang1
        [ (kitataka.talanga.taka.taka.talanga,
            k.t.k.p.o.u.__.k.p.k.p.k.o.u.__.p)
        , (kitataka.takataka.takataka.takataka,
            k.p.k.a. k.p.k.p .k.p.k.p .k.p.k.p)
        , (tam.__.kita.takadinna.tam.__.kita.takadinna,
            a.__.p.t.a.o.p.t.a.__.p.t.a.o.p.t)
        , (ta, p)

        , (ta.dit.ta.ga, t.p.t.lt k)
        , (tadikita, t.p.k.t)
        , (dim, a)
        , (thom, a)

        , (sarva_ndd, t.o.o.t . t.t.o.o.t . t.o.o.t . t.o.o.t)
        ] where KendangTunggal.Strokes {..} = KendangTunggal.notes
    k2 = makeKendang2
        [ (ta.dit.ta.ga, t.l.t.l)
        , (ta.di.ki.ta, l.k.p.t)
        , (dim, a)
        , (thom, a)
        ] where KendangPasang.Strokes {..} = KendangPasang.notes

-- TODO the same as Kendang2020.farans, except this has the unnecessary sollus,
-- but can also more easily do kendang pasang... so I'm not sure which is
-- better!  Maybe solve with inline mridangam:
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
