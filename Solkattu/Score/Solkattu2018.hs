-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
-- | Solkattu scores from 2018.
module Solkattu.Score.Solkattu2018 where
import Prelude hiding ((.), (^), repeat)

import qualified Solkattu.Instrument.KendangTunggal as KendangTunggal
import qualified Solkattu.Score.SolkattuMohra as SolkattuMohra
import qualified Solkattu.Tala as Tala

import Solkattu.Dsl.Solkattu


yt_mannargudi1 :: Korvai
yt_mannargudi1 = source "Mannargudi Easwaran" $
        recording "https://www.youtube.com/watch?v=E7PLgnsFBaI"
            (Just ((0, 4, 05), (0, 5, 22))) $
        date 2018 1 12 $
        korvai adi mridangam $ map (smap su)
    [ endOn 4 $ devel $
        sarvaM_ 8 . theme.din.__4 . in3 (1^theme) . theme.din.__8 . sarvaD_ 4
        . theme.din.__4 . in3 (1^theme) . din.__6
        . trkt.theme.tat.__6
        . kttk.trkt . in3 theme . din.__2
        . restD 1 . trkt.theme.din.__2
        . kttk.trkt . in3 theme
    , startOn 4 $ eddupu (3/4) $ ending $
        r3 $ theme . spread 3 tdgnt . theme . spread 2 tdgnt . theme
            . tri_ __3 (tri p5)
            -- Reduce __3 karvai in utarangam to __2 to arrive on sam.
    ]
    where
    theme = group $ takita.ta.takadinna
    mridangam = makeMridangam
        [ (takita.ta, k.k.t.k)
        , (1^takita.ta, p&k.p&k.t.k)
        , (din, od)
        , (tat, k)
        -- , (tarikita, p.k.t.p)
        -- , (kitataka, k.t.p.k)
        , (tarikita, o.k.n.p)
        , (kitataka, k.t.k.t)
        ]

e_18_02_26 :: Korvai
e_18_02_26 = ganesh $ exercise $ date 2018 2 26 $ korvaiS adi mridangam $
    map (nadai 6)
    [ rest 2 . tri (in3 p8)
        . rest 2 . mconcat fwd . rest 2 . mconcat bwd
    , join (din.__6) (replicate 3 (in3 p8))
        . join (din.__6) fwd . join (din.__6) bwd
    -- TODO technique: kt_ kn_ ko_ ok_ D -> kt_pkn_pko_ook_oD
    ]
    where
    p8 = group $ kita.ki.na.takadinna
    fwd = [p8, in3 p8, in4 p8]
    bwd = [in4 p8, in3 p8, p8]
    rest n = __M (n*6)
    mridangam = makeMridangam
        [ (p8, k.t.k.n.k.o.o.k)
        , (din, od)
        ]

in4 :: SequenceT sollu -> SequenceT sollu
in4 = appendEach 2 (__.__)

yt_mannargudi2 :: Korvai
yt_mannargudi2 = source "Mannargudi Easwaran" $
        recording "https://www.youtube.com/watch?v=E7PLgnsFBaI"
            (Just ((0, 9, 30), (0, 10, 30))) $
        date 2018 2 26 $
        korvai adi mridangam $ map (smap (nadai 6 • su))
    [ devel $
        sarva 6 . taka.naka.p8 . sarva 2 . __.__.tat.__.p8
        . sarva 1 .__.__. kpnp . in3 p8
    , devel $
        sarva 2 . tang.__.tang.__ . p8 . nadai 4 p8 . din.__3 . p9
            . din.__.na.__
        . tri_ (din.__.na.__) p8 . in3 p8
    , commentS "gradual transition to nadai 4" $ devel $
        din.__4 . join (din.__4)
        (replicate 2 (p8 . in3 p8) ++ replicate 2 (p8 . nadai 4 p8))

    , eddupu (3/4) $ ending $
        r3 $ suffixes (in3 p8 . din.__.ga)
            [tat_din_, r2 tat_din_, r3 tat_din_]
    , eddupu (3/4) $ ending $
        r3 $ suffixes (nadai 4 p8 . din.__.ga)
            [tat_din_, r2 tat_din_, r3 tat_din_]
    ]
    where
    p8 = group $ kita.ki.na.takadinna
    p9 = kita.ki.na.ta.takadinna
    tat_din_ = tat.__.din.__.p5

    sarva n = sarvaM_ (n*6)
    mridangam = makeMridangam
        [ (p8, k.t.k.n.k.o.o.k)
        , (p9, k.t.k.n.p.k.o.o.k)
        , (taka.naka, n.k.n.p)
        , (din, od)
        , (tat, k)
        , (tang, u)
        , (din.na, od.n)
        , (ga, p)
        ]

yt_pmi1 :: Korvai
yt_pmi1 = source "Palakkad Mani Iyer" $
        recording "https://www.youtube.com/watch?v=J2xgcBY4cXg"
            (Just ((0, 3, 50), (0, 4, 40))) $
        date 2018 1 12 $
        korvaiS Tala.triputa_tala mridangam $
    [ __.__.sar1.sar2.__.na.na.din.__
        . __. theme1 . din.__.na.na
        . din.__7 . sar3 . __.__. theme1.din.__.na.na.din.__
        . __ . theme2.din.__.na.na.din.__
        . __ . theme2
    , theme1.din.__ . su __ . fill1 . theme2.din.__ . su __ . fill1
        . theme1 . theme2 . utarangam
    ]
    where
    utarangam = su __ . tri (group $ su $ tat.dit.tat . __ . kita.taka.din)
    fill1 = tat.din . su tdgnt

    theme1 = group $ su (tat.__3.dit.__.ka.taka) . din.din.na
    theme2 = group $ su (ka.tat.__.dit.__.ka.taka) . din.din.na

    -- TODO awkward ad-hoc sarva
    sar1 = ta.ta.dit.ta.tang.ga . su (ta.lang.__.ga)
    sar2 = din.tat.din.tat.tam
    sar3 = din.din.__.na.ka.na.na.din
    mridangam = makeMridangam
        [ (sar1, k.k.t.k.u.o.o.u.__.k)
        , (sar2, o.k.o.k.od)
        , (sar3, d.d.__.n.d.n.n.u)

        , (tat.dit, p&k.p&t)
        , (ka.taka, k.n.n)
        , (din.din.na, o.o.k)
        , (tat.dit.tat, p&k.p&k.p&k)
        , (kita.taka.din, p.k.n.n.o)
        , (na.na.din, on.on.od)
        , (tat.din, p&k.od)
        , (din, od)
        , (ka, p&k)
        , (tdgnt, p&k.t.k.n.o)
        ]

yt_karaikudi1 :: Korvai
yt_karaikudi1 = source "Karaikudi Mani" $
        recording "https://www.youtube.com/watch?v=_33FkETjQoc"
            (Just ((0, 1, 34), (0, 2, 53))) $
        similarTo "Solkattu2016" "c_16_12_06_sriram1" $
        date 2018 1 12 $
        korvai adi mridangam
    [ devel $
        sarvaD_ (4+1/2) . theme . tat.__ . sarvaD_ (1/2)
        . sarvaD_ (4+1/2) . theme . din.__4
    , local $ devel $
        sarvaD_ 5 . theme.nakadinna
        . sarvaD_ 5 . __ . dropM 1 theme.nakadinna
        . tam.__4 . theme.nakadinna
        . tam.__4 . __ . dropM 1 theme.nakadinna
        -- . theme . dropM 1 theme . dropM 2 theme . p5
        -- . theme . dropM 2 theme . dropM 4 theme . tri_ tam nakadinna

        . theme.nakadinna.tam.__3 . dropM 1 theme . tri_ tam nakadinna
    , ending $ purvangam . nadai 6 (
        tri kitatakatam
        . p5
        . tri kitatakatam
        . p5.p5
        . tri kitatakatam
        . p5.p5.p5
        )
    -- simple versions for dummies like me
    , local $ ending $
        let t = ta.din.__.p6
        in purvangam . nadai 6 (trin (ta.__3.din.__3) t (t.t) (t.t.t))
    , local $ ending $
        let tadin = r3 (ta.din.__)
        in purvangam . nadai 6 (
            tadin . p6 . tadin . p6.__.p6 . tadin . tri_ __ p6)
    ]
    where
    purvangam = theme . tri (r3 nakadinna . din.__3)
        . theme . tri (r2 nakadinna . din.__)
        . theme . tri (nakadinna.din)

    theme = group $ su $ theme1 . nakatiku
    theme1 = tat.__.dit.__.kita.ki.na.ta.ki.taka
    nakadinna = group $ su $ na.ka.din.na
    kitatakatam = kttk.tam.__
    mridangam = makeMridangam
        [ (theme1, k.__.t.__.k.t.k.n.o.k.o&t.k)
        , (tat, k)
        , (ta, k)
        , (din, od)
        -- TODO technique: if preceded by a rest, play kook
        , (nakadinna, n.o.o.k)
        , (kitatakatam, o.k.n.p.u.__)
        , (tam, od)
        ]

c_18_03_19 :: Korvai
c_18_03_19 = date 2018 3 19 $ ganesh $ korvai Tala.misra_chapu mridangam $
    map devel
    [ (kitataka.sd2 din) <== 7 . sarvaD_ 7
    , (kitataka.sd2 din) <== 7 . 1^(kitataka.sd2 din) <== 7
    , kitataka . sd2 (din.na) . kitataka . sd2 (din.din.na)
        . 1^(kitataka . sd2 (din.na) . kitataka . sd2 (din.din.na))
    , kitataka . sd2 (din.na) . tri_ (din.__) kitataka
        . 1^(kitataka . sd2 (din.na) . tri_ (din.__) kitataka)
    , (kitataka.sd2 din) <== 7
        . sarvaD_ 6.5 . tri_ (sd (din.__3)) (tat.__.kitataka)
        . din.__4 . sarvaD_ 5
        . tri_ (din.__4) (tat.__4.kitataka)
        . din.__4 . sarvaD_ 4.5
        . tri_ (din.__) (tat.__.tat.__4.kitataka)
        . din.__4 . sarvaD_ 6
    ] ++
    [ ending $ purvangam 3 . tri_ (sd2 (ta.din)) p5
    ] ++ map (var • dateS 2018 3 27 • ending)
    [ purvangam 2 . tri_ (sd2 (ta.din)) (kp.p5)
    , purvangam 1 . tri_ (sd2 (ta.din)) (kpnp.p5)
    , purvangam 3 . tri_ (tam.__) (taka.tiku.p5)
    , purvangam 2 . tri_ (tam.__) (ta.__.ka.ti.__.ku.p5)
    , purvangam 1 . tri_ (tam.__) (ta.__.__.ka.ti.__.__.ku.p5)
    ]
    where
    p5 = pattern $ kita.taka.tari.kita.taka
    purvangam gap =
        tat.__.tat.__4.kitataka . din_din_na__ gap
        . tat.__4.kitataka . din_din_na__ gap
        . tat.__.kitataka . din_din_na__ gap
    kitataka = group $ kita.taka
    din_din_na__ gap = sd2 (din.din) . sd (na.__n gap)
    mridangam = makeMridangam
        [ (kitataka, k.t.k.o)
        , (1^kitataka, k.t.k.p)
        , (na, on)
        , (1^na, n)
        , (din, od)
        , (1^din, d)
        , (tat, p&k)
        , (ta, k)
        , (p5, k.t.p.k.p.k.t.k.n.o)
        , (tam, u)
        , (taka.tiku, k.p.n.p)
        ]

c_18_03_28 :: Korvai
c_18_03_28 = date 2018 3 27 $ ganesh $ korvaiS Tala.misra_chapu mridangam
    [ (theme.din.__4) <== 7 . sarvaD_ 7
    , (theme.din.__4) <== 7 . 1^(theme.din.__4) <== 7
    , (theme.din.__4) . theme.din.__4 . na.__4
        . 1^(theme.din.__4 . theme.din.__4 . na.__4)
    , (theme.din.__4) . theme . takeD 2 theme
        . 1^(theme.din.__4) . 1^theme . takeD 2 (1^theme)
    , (theme.din.__4) . kitakinakadin.__ . r2 (nakadin.__) . ga
        . 1^((theme.din.__4) . kitakinakadin.__ . r2 (nakadin.__) . ga)
    , (theme.din.__4) . kitakinakadin.__ . r3 (ka.din.__)
        . 1^((theme.din.__4) . kitakinakadin.__ . r3 (ka.din.__))
    , (theme.din.__4) <== 7 . sarvaD_ 6 . tri_ (din.__4) theme
    , sarvaD_ 7 . sarvaD_ 4.5 . tri_ (din.__4) (tat.__.theme)
    , sarvaD_ 7 . sarvaD_ 3 . tri_ (din.__4) (tat.__4.theme)
    , r3 (tat.__4.theme.din.__4.din.__4.na.__4) . sd (tri_ __ p6)
    , tsep (tat.__4.theme.din.__4.din.__4.na.__6)
        (tari.kita) (kita.kita.tari.kita)
        . tri_ (tat.__4.din.__4) (tri p10)
    ]
    where
    kitakinakadin = group $ kita.ki.na.ka.din
    nakadin = dropM 3 kitakinakadin
    p10 = group $ kita.taka.tari.kita.taka
    theme = group $ kita.ki.na.ka.din.__.ka
    mridangam = makeMridangam
        [ (kitakinakadin, k.t.k.n.o.od)
        , (1^kitakinakadin, k.t.k.n.p.d)
        , (ka.din, k.od)
        , (1^(ka.din), k.d)
        , (theme, k.t.k.n.o.od.__.k)
        , (din, od)
        , (1^theme, k.t.k.n.p.d.__.k)
        , (1^din, d)
        , (na, on)
        , (1^na, n)
        , (ga, p)
        , (tat, p&k)

        , (tari.kita, p.k.n.p)
        , (kita.kita, k.t.k.t)
        , (p10, k.t.p.k.p.k.t.k.n.o)
        ]

c_18_04_25 :: Korvai
c_18_04_25 = date 2018 4 25 $ ganesh $
    similarTo "Mridangam2013" "dinnagina_sequences" $
    korvaiS Tala.misra_chapu mridangam
    [ (4*7) ==> theme
    , (2*7) ==> theme . (2*7) ==> theme
    , r2 (7 ==> theme . 7 ==> theme)
        -- TODO open and closed sarva: o_k_d, p_k_d
    , r4 $ takeM 12 (1^theme) . theme
    , 1^theme . dhom.__4 . theme . dhom.__4 . theme
        -- TODO if I understood kali/tali I could use tri_
    , 1^theme . dhom.__4 . theme . dhom.__ . dropM 8 theme
                                 . dhom.__ . dropM 8 theme
                    -- TODO sandi
    , tri_ (dhom.__) (theme.dhom.__. dropM 8 theme.dhom.__ . dropM 8 theme)
                    -- TODO sandi again
    -- TODO incomplete
    ]
    where
    theme = group $ dhom.__.ka.__ . r3 (din.na.gin.na)
    -- TODO other themes
    mridangam = makeMridangam
        [ (theme, t.__.k.__.o.k.t.k.o.k.t.k.o.k.t.k)
        , (1^theme, o.__.k.__.o.k.t.k.o.k.t.k.o.k.t.k)
        , (dhom, o)
        ]

{-
    misra chapu sequence 100 bpm:

    . dinaginna sequence with 4 avartanams per section
    . dinaginna with dinnaginna taka taka talang ga
    . kita kina din
    . kita kinakadin kadin
-}

c_18_05_25 :: Korvai
c_18_05_25 = date 2018 5 25 $ ganesh $
    korvai Tala.misra_chapu mridangam
    [ devel $
        (4*7) ==> theme1 . (4*7) ==> dropD 1 theme1 . (4*7) ==> dropD 2 theme1
    , devel $
        (2*7) ==> theme1 . (2*7) ==> dropD 1 theme1 . (2*7) ==> dropD 2 theme1

    -- TODO This expresses din.__ karvai as a general connective, which is
    -- probably more accurate than the ad-hoc cases in 'tri_' or 'reduce3'.
    -- Unfortunately, this makes it hard to use 'sandi', which should be:
    --
    -- sandi . tri theme2 . sandi . tri (dropD 1 theme2)
    -- . sandi . tri (dropD 2 theme2)
    --
    -- but can't be, because 'sandi' takes the elided part explicitly, and
    -- that includes the din.__ karvais.  Since 'sandi' operates at the flat
    -- level, maybe it should be implicit after all?
    , ending $ join (din.__) $ concat
        [ take 3 $ reduceToL 0 4 (theme1.theme2)
        , replicate 2 theme2
        , replicate 2 (dropD 1 theme2)
        , replicate 2 (dropD 2 theme2)
        ]
    , ending $ let theme2 = theme2' in join (1^din.__4) $ concat
        [ take 3 $ reduceToL 0 4 (theme1.theme2)
        , replicate 2 theme2
        , replicate 2 (dropD 1 theme2)
        , replicate 2 (dropD 2 theme2)
        , replicate 2 (dropD 4 theme2)
        ]
    ]
    where
    theme1 = group $ theme1_.nakatiku
    theme1_ = tat.__4.dit.__4.kita.ki.na.ta.ki.taka
    theme2 = group theme2_
    theme2_ = dit.__.tat.__.di.mi.kita.dugu.dugu.talang.__.ga

    theme2' = group $ theme2_ . din.__.tat.__.din.na.tat.__
    mridangam = makeMridangam
        [ (theme1_, p&k.__4.p&t.__4.k.t.k.n.o.k.o&t.k)
        , (theme2, t.__.k.__.o.k.t.k.o.k.o.k.o.u.__.k)
        , (din.tat.din.na.tat, o.k.o.k.k)
        , (din, o)
        , (1^din, od)
        ]

-- * HS Sudhindra

-- | Shri HSS speaks it this way.
dikutarikitataka :: Sequence
dikutarikitataka = nakatiku

misra_tani :: [Part] -- realizePartsM patterns misra_tani
misra_tani =
    [ Comment "part 1"
    , K misra_tani1 All
    , K misra_to_mohra1a All
    , Comment "part 2"
    , K to_mohra_farans All
    , K misra_tani2 All
    , K misra_to_mohra1b All
    , K misra_to_mohra3 All
    , Comment "part 3"
    , K misra_to_mohra4 All
    , K misra_mohras (Index 0) -- or !! 1
    , K misra_muktayi1 All
    ]

misra_tani1 :: Korvai
misra_tani1 = date 2018 7 16 $ sudhindra $
    korvai Tala.misra_chapu (mridangam<>kendang)
    [ x2 $ section $ sd2 $
        tam.__3.tam.__.tam.__
      . tam.__3.tam.__.su (taka.jonu)
    , section $ sd2 $
        na.din.na.na.din.din.na.din.__n 7
        . na.din.na.su (na.na).din.din.na.din.__n 7
    , section $ (7*4) ==> takadugutarikita
    ]
    where
    mridangam = makeMridangam
        [ (tam, p&v)
        , (taka.jonu, k.o.o.k)
        , (na, on)
        , (din, od)
        ]
    kendang = makeKendang1
        [ (tam, a)
        , (taka.jonu, p.a.o.p)
        , (na, t)
        , (din, a)
        ] where KendangTunggal.Strokes {..} = KendangTunggal.notes

misra_tani2 :: Korvai
misra_tani2 = date 2018 7 16 $ sudhindra $ korvai Tala.misra_chapu mridangam
    [ x4 $ section $ sd $ taka.ta.ta.din.__6.tatadin_
    , x4 $ section $ sd $ taka.ta.ta.din.__4.taka.tatadin_
    , x3 $ section $ sd $ taka.r3 tatadin_
    , section $ sd (taka.r3 tatadin_)
    , x2 $ section $ taka.taka . sd (r3 tatadin_)
    , x4 $ section $ taka.taka . sd tatadin_ . taka.taka.din.__4 . sd tatadin_
    , x4 $ section $ taka.taka . sd tatadin_ . r2 (taka.taka.din.__4)
    , x3 $ section $ taka.taka.taka.taka.din.__4 . r2 (taka.taka.din.__4)
    , ending $ tri (taka.taka.din.__4) . tri_ (din.__4) takadugutarikita
    ]
    where
    tatadin_ = ta.ta.din.__
    mridangam = makeMridangam
        [ (taka.ta.ta.din, on.k.on.on.od)
        , (ta.ta.din, on.on.od)
        , (taka.taka, o&j.y.o&j.y)
        , (din, od)
        ]

-- abhipriyam (telugu) -> thoughts
misra_to_mohra1a :: Korvai
misra_to_mohra1a = date 2018 7 2 $ sudhindra $
    korvaiS Tala.misra_chapu (mridangam<>kendang)
    [ din.__8 . tA1
    , sarvaD_ (7*3)
    , sarvaD_ 4 . tA
    , sarvaD_ 7 . r3 (sarvaD_ 4 . dropD 2 tA1)
    , r3 tA . r2 (dropD 5 tA) . r2 (dropD 7 tA) . r2 (dropD 9 tA)
    ]
    where
    tA = group $ tA0.dikutarikitataka.tA1.din.__4
    tA0 = tat.__.dit.__.tarikita.kitataka
    tA1 = tat.__.tat.__.tarikita.gu.gu.takita.tong.__.ka.din.__.tat.__
    mridangam = makeMridangam
        [ (tA0, p&k.__.p&t.__.k.t.k.n.p.k.t.k)
        , (tA1, k.__.k.__.p.k.t.k.o.o.k.p.k.od.__.k.od.__.k.__)
        , (din, od)
        ]
    kendang = makeKendang1
        [ (tA0, pk.__.pk.__.k.p.k.t.p.k.p.k)
        , (tA1, p.__.p.__.p.k.p.p.a.a.k.p.k.a.__.o.a.__.o.__)
        , (din, a)
        ] where KendangTunggal.Strokes {..} = KendangTunggal.notes

misra_to_mohra1b :: Korvai
misra_to_mohra1b = date 2018 7 2 $ sudhindra $
    korvai Tala.misra_chapu (mridangam<>kendang)
    [ x4 $ devel sarva
    , x4 $ devel $ sarva `replaceEnd` group (ta.takita.takadinna)
    , x4 $ devel $ takeD 2 sarva.din.__4. theme
    , ending $ theme.takadinna.din.__6.tat.__4
        . theme . r2 takadinna.din.__6 . tat.__4.dit.__4
        . theme . trin (tam.__n 16)
            (tri_ (din.__6) (tri takadinna))
            (tri_ (din.__6) (r2 takadinna))
            (tri_ (din.__6) takadinna)
    ]
    where
    sarva = taka.taka.din.__.kita.din.__.kita.r2 (taka.taka.din.__.kita)
    theme = group $ theme0.dikutarikitataka
    theme0 = tarikita.kitataka
    mridangam = makeMridangam
        -- sarva
        [ (taka.taka, j.y.j.y)
        , (din.kita, d.lt p.k)
        , (ta.takita.takadinna, t.k.o.o.k.t.o.k)

        -- ending
        , (din, od)
        , (tat, p&u)
        , (tat.dit, p&u.p&u)

        , (theme0, k.t.k.n.p.k.t.k)
        , (takadinna, n.o.o.k)
        , (r3 takadinna, k.o.o.k.t.o.o.k.t.o.o.k)
        , (tam, od)
        ]
    kendang = makeKendang1
        [ (taka.taka, t.k.t.o)
        , (din.kita, u.lt p.k)
        , (ta.takita.takadinna, t.p.a.a.p.k.p.p)

        , (din, a)
        , (tat, u)
        , (tat.dit, u.u)

        , (theme0, k.p.k.t.p.k.p.k)
        , (takadinna, t.a.a.p)
        , (r3 takadinna, t.a.a.k.p.a.a.k.p.a.a.p)
        , (tam, a)
        ] where KendangTunggal.Strokes {..} = KendangTunggal.notes

to_mohra_farans :: Korvai
to_mohra_farans = date 2018 7 2 $ sudhindra $ faran $
    korvai Tala.misra_chapu mridangam
    [ section $ sarva 14
    , x2 $ section $ sarva (7+4) . tri (talang.__.gu)
    , x2 $ section $ sarva (7+4) . tri (gu.gu.na.na)
    , section $ r2 $ sarva 4 . tri dinnakitataka
    , section $ r2 $ sarva 4 . r2 takadinnakitataka
    , x2 $ section $ sarva 4 . r2 takadinnakitataka
      . r4 takadinnakitataka . dinnakitataka
    , section $ sarva 4 . r2 takadinnakitataka
      . r3 (r2 takadinnakitataka . r4 dinnakitataka)
    , section $ tri_ (din.__4) (r4 dinnakitataka)
    ]
    where
    dinnakitataka = group $ din.na.kttk
    takadinnakitataka = group $ taka.din.na.kttk
    sarva = sarvaD sarvaS
    sarvaS = r4 $ sd $ taka.ta.ta.din.__.taka.din.na.ta.ta.din.__
    sarvaM = let rh = strM "nknnd_nkdknnd_" in
          rh & strM "o_ooo_o_o_ooo_"
        . rh & strM "__ooo_o_o_ooo_"
        . rh & o
        . rh
    mridangam = makeMridangam
        [ (sarvaS, sarvaM)
        , (talang.gu, p.u.k)
        , (gu.gu.na.na, o.o.n.n)
        , (dinnakitataka, dinna)
        , (takadinnakitataka, o.k.dinna)
        , (din, od)
        ]
        where dinna = o.n.k.t.o.k

misra_to_mohra3 :: Korvai
misra_to_mohra3 = date 2018 7 2 $ sudhindra $
    korvai Tala.misra_chapu (mridangam<>kendang) $ map ending
    [ reduceTo 4 2 theme . tri theme2
    ]
    where
    theme = tat.__.dit.__.takadinna.din.__.tat.__.din.__4
    theme2 = ta.__.di.__.ki.__.ta.__.ta.tarikita.thom
    mridangam = makeMridangam
        [ (theme, k.__.t.__.k.o.o.k.od.__.k.__.od.__4)
        , (theme2, k.__.t.__.k.__.n.__.p.k.t.k.n.o)
        ]
    kendang = makeKendang1
        [ (theme, p.__.p.__.t.a.a.u.a.__.o.__.a.__4)
        -- TODO similar technique for kendang as for mridangam?
        -- , (din.na.din.tat.din, k.u.a.o.a)
        , (theme2, o.__.p.__.k.__.t.__.a.o.p.k.t.a)
        ] where KendangTunggal.Strokes {..} = KendangTunggal.notes

misra_to_mohra4 :: Korvai
misra_to_mohra4 = date 2018 7 2 $ sudhindra $
    korvai Tala.misra_chapu mridangam
    [ section $ restD 3 . tat.__.tat.__.kitataka.dikutarikitataka
        . tri123 (thom.__4) dikutarikitataka
    , x2 $ section $ r4 (din_gu_ . tarikitataka) . dim.__4 . tat.__4
    , x2 $ section $ din_gu_ . tri_ (dim.__4) tarikitataka . din_gu_
        . taka.dikutarikitataka . dim.__4 . tat.__4
    , x2 $ section $ r2 (prefixes [dim, thom] (__.taka.dikutarikitataka))
        . dikutarikitataka
    , section $ r7 $ gugunana_trkt
    , x2 $ section $ r3 (gu.gu.na.na) . gugunana_trkt
    , x2 $ section $ r2 (gu.gu.na.nang.__.gu) . gugunana_trkt
    , x2 $ section $ r2 (gu.gu.na.na.taka) . gugunana_trkt
    , section $ r3 gugunana_trkt . tri_ (dim.__8) gugunana_trkt
    ]
    where
    din_gu_ = din.__4.gu.__
    gugunana_trkt = group $ gu.gu.na.na.tarikita.dikutarikitataka
    tarikitataka = group $ tarikita.taka
    mridangam = makeMridangam
        [ (tat, k)
        , (kitataka, k.t.p.k)
        , (din.gu, od.o)
        , (taka, p.k)
        , (dim, od)
        , (thom, od)
        , (tarikita, p.k.t.k)
        , (tarikita.taka, k.t.k.t.p.k)
        , (gu.gu.na.na, o.o.n.n)
        , (gu.gu.na.nang.gu, o.o.n.n.k)
        ]

misra_mohras :: Korvai
misra_mohras = date 2018 7 11 $ sudhindra $
    SolkattuMohra.makeMohras Tala.misra_chapu mridangam id
        [ ((a1, a2, a1), (b1, b2, b3))
        , ((c1, a2, c1), (b1, b2, b3))
        ]
    where
    a1 = din.__.taka.tiku.tarikita.thom.__
    a2 = dit.__4.tam.__.taka.tiku.tarikita.thom.__
    b1 = talang.__.gu.din.__.tat.__.talang.__.gu.din.__4
    b2 = talang.__.gu.din.__4
    b3 = tri_ (din.__4) (talang.__.gu.din.__.tat.__)

    theme = tat.__.dit.__.takadinna.din.__4
    c1 = reduceTo 4 2 theme
    mridangam = makeMridangam
        [ (din.taka, u.p.k)
        , (tam.taka, u.p.k)
        , (tiku.tarikita.thom, t.p.u.p.k.t.o)
        , (dit, k)
        , (talang.gu, p.u.k)
        , (din.tat, o.k)
        , (din, od)
        , (theme, k.__.t.__.k.o.o.k.od.__4)
        ]

misra_muktayi1 :: Korvai
misra_muktayi1 = date 2018 7 11 $ sudhindra $
    korvai1 Tala.misra_chapu mridangam $ section $
    reduceTo 4 2 theme . tri_ (sd __) (sd (ta.__.di.__.p5))
    where
    theme = tat.__.dit.__.tat.__.tat.__.kita.thom.__4
    p5 = group $ su $ kita.kita.tari.kita.thom.__
    -- TODO replace reduction fingering.  How should I express irregular
    -- fingerings like this?  Maybe with per-korvai technique?
    _fingerings =
        [ [] -- same: [k, t, k, k]
        , [k, t, k]
        , [k, t]
        ]
    mridangam = makeMridangam
        [ (tat, k)
        , (dit, t)
        , (kita.thom, p.k.o)
        , (ta.di, p&k.p&k)
        , (p5, k.t.k.t.p.k.p.t.o.__)
        ]

trikalam1 :: Korvai
trikalam1 = date 2018 7 16 $ trikalam $ sudhindra $ korvai adi mridangam
    [ endOn 4 $ devel $
        tam.__4.theme0.di.__6.p6.__3.p6
        . ta.takita.theme0.di.__6.p6.__3.p6
        . tam.__4.theme0.di.__4
    , startOn 4 $ endOn 4 $ section $ theme
    , startOn 4 $ endOn 4 $ section $ r2 $ nadai 6 theme
    , startOn 4 $ section $ r3 $ su theme
    ]
    where
    theme = tam.__4.theme0.di.__6.p6
        . ta.takita.theme0.di.__6.p6
        . tam.__4.theme0.di.__6 . tri_ (di.__6) p6
    theme0 = taka.din.__.din.__.tat.__
    mridangam = makeMridangam
        [ (tam, p&v)
        , (theme0, k.o.od.__.od.__.k.__)
        , (ta.takita, k.t.k.t)
        , (di, od)
        ]
    -- At higher speed, tadi kitathom becomes tadi kitaka.

trikalam2 :: Korvai
trikalam2 = date 2018 7 16 $ trikalam $ sudhindra $
    korvai adi mridangam $ map section
    [ restD 2 . seq . r2 (nadai 6 seq) . r3 (su seq)
    ]
    where
    seq = reduce3 2 ø theme . tri p6
    theme = tat.__.dit.__.takadinna.di.__4
    mridangam = makeMridangam
        [ (theme, k.__.t.__.k.o.o.k.od.__4)
        ]

e_sarva1 :: Korvai
e_sarva1 = sarvalaghu $ date 2018 7 16 $ sudhindra $
    korvai Tala.misra_chapu mridangam $ map section $ map (nadai 3)
    [ din.__.gu.takita.din.__.gu . r2 (din.__.gu.takita)
    ]
    where
    mridangam = makeMridangam
        [ (din.gu.takita, od.y.o&j.y.od)
        , (din.gu, od.k)
        ]

e_sarva2 :: Korvai
e_sarva2 = sarvalaghu $ date 2018 7 25 $ sudhindra $
        korvai Tala.misra_chapu mridangam $ map section
    [ sarva
    , sarva `replaceEnd` group (ta.takita.takadinna)
    , tri (taka.taka.din.__.kita) . tri_ (tam.__4) (group (ta.takita.takadinna))
    ]
    where
    sarva = taka.taka.din.__.kita.din.__.kita.r2 (taka.taka.din.__.kita)
    mridangam = makeMridangam
        [ (taka.taka, j.y.j.y)
        , (din.kita, d.lt p.k)
        , (ta.takita.takadinna, t.k.o.o.k.t.o.k)
        , (tam, od)
        ]

e_misra_tisra :: Korvai
e_misra_tisra = exercise $ korvai Tala.misra_chapu mempty $
    map section $ map (nadai 3)
    [ r7 takita
    , r2 takita . tri (group p5)
    ,    takita . tri (group p6)
    , r7 takita . tri (group p7)
    , r6 takita . tri (group p8)
    , r5 takita . tri (group p9)
    ]

-- * adi

adi_tani :: [Part] -- realizePartsM (patterns <> namedThemes) adi_tani
adi_tani =
    [ K adi_tani1 All
    , K c_18_09_25 All
    , K trikalam1 All
    , K e_sarva1_tisra All
    , K e_adi_tisra All
    , K c_18_08_03 All
    , K adi_tani2 All
    , Comment "mohra"
    , K SolkattuMohra.c_mohra All
    , K adi_muktayi All
    ]

adi_tani_misra :: [Part] -- realizePartsM (patterns<>namedThemes) adi_tani_misra
adi_tani_misra =
    [ -- K adi_tani1_misra All
      K c_18_09_25_misra All
    , K misra_trikalam All
    -- K e_sarva1_tisra All
    , K e_adi_tisra_misra2 All
    , K c_18_08_03_misra All
    , K adi_tani2_misra All
    , K SolkattuMohra.misra1 All
    , K adi_muktayi_misra All
    ]

adi_tani1 :: Korvai
adi_tani1 = date 2018 8 3 $ sudhindra $ korvai adi mridangam
    [ section $ r6 (tam.__4) . tri_ din tkdn
    , section $ r2 $ r2 (tam.__4) . tri_ din tkdn
    , section $ tri_ (din.__4) (tri_ din tkdn)
    , section $ sd (na.din.din.na) . na.na.sd (din.din.na)
        . kttk. sd (din.din.na) . talang_gu
    , x2 $ section $ r2 $ sd (na.din.din.na) . talang_gu
    , section $ tri_ (din.__4) talang_gu
    , x2 $ section $ sarva2 6 . su (faran1.nakatiku)
    , section $ r2 (sarva2 2 . su (faran1.nakatiku))
        . r4 (su (faran1.nakatiku))
        . tri_ (din.__4) (su (faran1.nakatiku))
    , section $ r2 (sarva3a.sarva3')
        . r2 (sarva3b.sarva3')
        . r2 (sarva3c.sarva3')
        . r2 (sarva3d.sarva3')
    , section $ sarva3b.sarva3' . sarva3b . su (faran2.nakatiku)
        . r2 (sarva3b . su (faran2.nakatiku))
    , section $ r4 (su (faran2.nakatiku))
        . tri_ (din.__4) (su (faran2.nakatiku))
    -- , endOn 4 $ section $
    --     r2 (su (faran2.nakatiku))
    --     . tri_ (din.__4) (su (faran2.nakatiku))
    ]
    where
    talang_gu = su $ talang.__.gu.talang.__.gu.taka.taka.talang.__.gu
    sarva2 = sarvaD sarva2Sollu
    sarva2Sollu = r4 $ taka.ta.din.__.ta.din.__
    faran1 = gu.gu.taka.tari.kita
    faran2 = takadugutarikita
        -- TODO HSS plays first one as ktoo... rest as tkoo...
    sarva3' = 10^sarva3a
    sarva3a = r2 $ naka.na.din
    sarva3b = 2^sarva3a
    sarva3c = 3^sarva3a
    sarva3d = su (naka.naka).na.din . naka.na.din
    mridangam = makeMridangam
        [ (tam, p&u)
        , (din, od)
        , (na, n)
        , (kita.taka, k.t.p.k)
        , (talang.gu, p.u.k)
        , (taka.taka, o.k.o.k)
        , (sarva2Sollu, let rh = strM "n,nd_nd_" in
              rh & strM "o_oo_oo_"
            . rh & strM "__oo_oo_"
            . rh & o
            . rh)
        , (faran1, o.o.k.t.o.k.t.k)
        , (sarva3', on.y.n.d.n.y.n.d)
        , (sarva3a, on.y.on.od.n.o&y.on.d)
        , (sarva3b, on.y.on.od.n.y.on.od)
        , (sarva3c, on.on.on.od.n.y.on.od)
        , (sarva3d, on.y.on.y.on.od.n.y.on.od)
        ]

e_sarva1_tisra :: Korvai
e_sarva1_tisra = exercise $ date 2018 7 25 $ sudhindra $
    korvai adi mridangam $ map section $ map (nadai 6)
    [ restD (4*1.5) . __.__ . tri_ (din.__5) takadinna
    , r5 din_gutakita . din.__.gu . tri p5
    , r5 din_gutakita . tri p6
    , r4 din_gutakita . din.__.gu . tri p7
    , r4 din_gutakita . tri p8
    , r3 din_gutakita . din.__.gu . tri p9
    ]
    where
    din_gutakita = din.__.gu.takita
    mridangam = makeMridangam
        [ (din_gutakita, od.__.y.o&j.y.od)
        , (din.gu, od.k)
        , (din, od)
        ]

-- takadimi takajonu
-- where l is only slighly on saddam
-- n.l.d.d
-- increase the speed
-- also n.d.d.d

e_adi_tisra_misra1, e_adi_tisra_misra2 :: Korvai
(e_adi_tisra_misra1, e_adi_tisra_misra2) =
    ( date 2019 2 26 $ ganesh $ let sarva = sarva1 in
        korvai Tala.misra_chapu mridangam $ map (smap (nadai 3)) $ map section
        [ sarva 7
        , sarva 6 . tarikitadiku
        , sarva 2 . tarikitadiku . sarva 3 . tarikitadiku
        , sarva 1 . tarikitadiku . sarva 1 . tarikitadiku
            . sarva 1 . tarikitadiku
            . tarikitadiku . sarva 1 . tarikitadiku . sarva 1
            . tri_ (din.__3) tarikitadiku
        ] ++ ending

    , date 2019 4 14 $ ganesh $ let sarva = sarva2 in
        korvai Tala.misra_chapu mridangam $ map (smap (nadai 3)) $ map section
        [ sarva 7
        , sarva 6 . tarikitadiku
        , sarva 2.5 . tarikitadiku . sarva 2.5 . tarikitadiku
        , sarva 1 . tarikitadiku . sarva 1 . tarikitadiku
            . sarva 1 . tarikitadiku
            . tarikitadiku . sarva 1 . tarikitadiku . sarva 1
            . tri_ (din.__3) tarikitadiku
        , sarva_with (tri dinnakttk)
        , sarva_with trktkt3
        , sarva_with (r2 takadinnakttk)
        ] ++ ending
    )
    where
    sarva2 dur = sarvaD sarva2Sollu (dur * 1.5)
    sarva2Sollu = r2 $ taka.ta.ta.dim.__.ta.dim.__ . r2 (taka.ta.ta.dim.__)

    sarva_with end = sarva2 5 . end
        . sarva2 1.5 . end . sarva2 1.5 . end

    ending = map section
        [ r5 takadinnakttk . tri dinnakttk
        , r5 takadinnakttk . din_trktkt
        , r2 $ takadinnakttk . tang.kttk . din_trktkt
        , din.__3 . r4 tarikitadiku . tarikita . trktkt3
        , r2 $ tarikitadiku . tarikita . trktkt3
        , repeat 10 tarikitadiku . tri_ (din.__3) tarikitadiku
        ]
        where tarikita = takeM 3 tarikitadiku

    -- TODO share the where-clause with e_adi_tirsa, except this adds tang.kttk

    -- Duration is off due to map nadai 6.  TODO this is pretty awkward.
    sarva1 dur = sarvaD sarvaSollu (dur * 1.5)
    sarvaSollu = r4 $ taka.ta.ta.dim.__
    tarikitadiku = named "6npkt" $ su $ tari.kita.taka.diku.kita.taka
    dinnakttk = group $ din.na.kttk
    trktkt3 = tri trktkt
    trktkt = named "4npkt" $ trkt.kttk
    takadinnakttk = group $ taka.din.na.kttk
    -- TODO this uses trkt.kttk instead of trktkt above because being a group
    -- will prevent the full din_trktkt match.
    din_trktkt = din.__.trkt.kttk.tarikitadiku
    mridangam = makeMridangam
        [ (sarvaSollu, let rh = n.k.n.n.d.__ in
              rh & strM "o_ooo_" . rh & strM "__ooo_" . rh & o . rh)
        , (tarikitadiku, n.p.k.t.p.k.t.p.k.t.p.k)
        , (dinnakttk, o.n.k.t.o.k)
        , (taka.dinnakttk, o.k.o.n.k.t.o.k)
        , (taka, o.k)
        -- High speed variant of trktkt drops the tha.
        -- Say din.__.tari.kita.kita.taka,
        -- play din.__.nang.__.kita.kita.taka
        , (din.trkt.kttk, od.on.__.k.t.k.t.p.k)
        , (trktkt, n.p.k.t.k.t.p.k)
        , (din, od)

        , (tang.kttk, on.k.t.o.k)
        , (sarva2Sollu,
              strM "n_nnd_nd_" & strM "o_ooo_oo_"
            . strM "n_nnd_n_nnd_" & strM "o_ooo_o_ooo_"
            . strM "n_nnd_nd_" & o
            . strM "n_nnd_n_nnd_"
            )
        ]

e_adi_tisra :: Korvai
e_adi_tisra = exercise $ date 2018 7 30 $ sudhindra $
    korvai adi mridangam $ map (smap (nadai 6))
    [ section $ sarva 8
    , x2 $ section $ sarva 7 . tarikitadiku
    , section $ r2 $ sarva 3 . tarikitadiku
    , section $ sarva 1.tarikitadiku.sarva 1.tarikitadiku
        . tri_ (tang.__.gu) tarikitadiku

    , x2 $ section $ sarva 6 . tri dinnakttk
    , section $ r2 (sarva 2 . tri dinnakttk)
    , section $ sarva 2 . r3 (tri dinnakttk)
    , x2 $ section $ sarva 6 . trktkt3
    , section $ r3 (sarva 2 . trktkt3) . trktkt3 . trktkt3
    , x2 $ section $ sarva 6 . r2 takadinnakttk
    , section $ r2 $ sarva 2 . r2 takadinnakttk
    , section $ r8 takadinnakttk
    , x2 $ section $ r6 takadinnakttk . tri dinnakttk
    , section $ r6 takadinnakttk . trktkt3
    , section $ r6 takadinnakttk . din_trktkt
    , section $ r2 $ r2 takadinnakttk . din_trktkt
    , x2 $ section $ r4 $ din_trktkt
    , x2 $ section $ r2 $ din.__ . r4 trktkt . tarikitadiku
    , section $ r4 tarikitadiku
        . tri (tri_ (tam.__3) tarikitadiku)
    ]
    where
    -- Duration is off due to map nadai 6.  TODO this is pretty awkward.
    sarva dur = sarvaD sarvaSollu (dur * 1.5)
    sarvaSollu = r4 $ taka.ta.ta.dim.__
    tarikitadiku = named "6npkt" $ su $ tari.kita.taka.diku.kita.taka
    dinnakttk = group $ din.na.kttk
    trktkt3 = tri trktkt
    trktkt = named "4npkt" $ trkt.kttk
    takadinnakttk = group $ taka.dinnakttk
    -- TODO this uses trkt.kttk instead of trktkt above because being a group
    -- will prevent the full din_trktkt match.
    din_trktkt = din.__.trkt.kttk.tarikitadiku
    mridangam = makeMridangam
        [ (sarvaSollu, let rh = n.k.n.n.d.__ in
              rh & strM "o_ooo_" . rh & strM "__ooo_" . rh & o . rh)
        , (tarikitadiku, n.p.k.t.p.k.t.p.k.t.p.k)
        , (tang.__.gu, od.__.o)
        , (dinnakttk, o.n.k.t.o.k)
        , (taka, o.k)
        -- High speed variant of trktkt drops the tha.
        -- Say din.__.tari.kita.kita.taka,
        -- play din.__.nang.__.kita.kita.taka
        , (din.trkt.kttk, od.on.__.k.t.k.t.p.k)
        , (trktkt, n.p.k.t.k.t.p.k)
        , (din, od)
        , (tam, od)
        ]

c_18_08_03 :: Korvai
c_18_08_03 = date 2018 8 3 $ sudhindra $
    korvai1 adi mridangam $ section $ nadai 6 $
    dit_tang.din.__.tat.__.din.__4
    . dit_tang.din.__3.tat.__3.din.__4
    . dit_tang.din.__4.tat.__4.din.__4
    . tri123 dingu p6
    where
    dit_tang = dit.__.tang.su (kita.nakatiku) -- spoken taka.tari.kita.taka
    mridangam = makeMridangam
        [ (dit.tang.kita, k.u.p.k)
        , (din, od)
        , (tat, k)
        , (din.gu, od.lt o)
        ]

c_18_08_03_misra :: Korvai
c_18_08_03_misra = date 2019 2 26 $ ganesh $
    korvai1 Tala.misra_chapu mridangam $ section $ nadai 3 $
    takita_tang.din.__3.tat.__3.din.__3
    . takita_tang.din.__4.tat.__4.din.__4
    . takita_tang.din.__5.tat.__5.din.__5
    . tri123 dingu p6
    where
    -- alternate starts: dit.__3, tat.tat.__
    takita_tang = takita.tang.su (kita.nakatiku) -- spoken taka.tari.kita.taka
    mridangam = makeMridangam
        [ (takita.tang.kita, o.k.o.u.p.k)
        , (din, od)
        , (tat, k)
        , (din.gu, od.lt o)
        ]

adi_tani2 :: Korvai
adi_tani2 = date 2018 8 3 $ sudhindra $ korvai adi mridangam $ map section
    [ restD 4 . su (tri_ (tam.__4) nakatiku)
    , r2 $ r2 (din.__.gu.trkt.tk) . dim.__.tat.__
    , r2 $ din.__.gu . tri_ (dim.__) (trkt.tk)
    , r2 $ r2 (din.__.gu.trkt.tk) . su nakatiku
    , r2 $ r2 (dim.su (taka.tikutarikitataka)) . su dikutarikitataka
    , su $ r4 (faran1.dikutarikitataka)
        . r2 (r4 (takeM 4 faran1) . faran1.dikutarikitataka)
        . r2 (tri_ taka (takeM 4 faran1) . faran1.dikutarikitataka)
        . r2 (r2 faran2 . takeM 4 faran1 . faran1.dikutarikitataka)
    , su $ r4 (faran1.dikutarikitataka)
        . r4 dikutarikitataka
        . tri (tri_ (tam.__4) dikutarikitataka)
    ]
    where
    -- said this way, played dikutarikitataka aka nakatiku
    -- TODO maybe there should be a way to set sollus for set patterns
    -- effectively it's konnakol realization
    tikutarikitataka = dikutarikitataka
    faran1 = ta.ta.kita.tari.kita
    faran2 = ta.ta.ki.tang.__.gu
    mridangam = makeMridangam
        [ (tam, od)
        , (dim, od)
        , (din.gu, od.lt o)
        , (tari.kita.taka, k.t.k.t.p.k)
        , (tat, k)
        , (dim.taka, on.p.k)
        , (faran1, o.o.k.t.p.k.t.k)
        , (faran2, o.o.k.t.__.k)
        , (taka, p.k)
        ]

adi_tani2_misra :: Korvai
adi_tani2_misra = date 2019 4 6 $ ganesh $ korvai Tala.misra_chapu mridangam $
    map (section • sd)
    [ r4 (din.__.gu.trkt.tk) . dim.__.tat.__
        . r4 (2^din.__.gu.trkt.tk) . su dikutarikitataka
    , su $ tri (faran1.dikutarikitataka) . dikutarikitataka
    , su $ r2 $ tri (takeM 4 faran1) . faran1.dikutarikitataka
    , su $ r2 $ r2 (takeM 6 faran1) . faran1.dikutarikitataka
    , su $ r2 $ r2 (takeM 6 faran2) . faran1.dikutarikitataka
    , su $ tri (faran1.dikutarikitataka) . dikutarikitataka
    , su $ r2 dikutarikitataka
        . tri (tri_ (tam.__4) dikutarikitataka)
    ]
    where
    faran1 = ta.ta.kita.tari.kita
    faran2 = ta.ta.ki.tang.__.gu
    mridangam = makeMridangam
        [ (tam, od)
        , (dim, od)
        , (din.gu, od.__)
        , (tari.kita.taka, k.t.k.t.p.k)
        , (tat, k)
        , (faran1, o.o.k.t.p.k.t.k)
        , (faran2, o.o.k.t.__.k)

        , (2^din.gu, d.__)
        ]

adi_muktayi :: Korvai
adi_muktayi = date 2018 8 3 $ sudhindra $ korvai adi mridangam
    [ x3 $ section $ su $
      t0 . oknp.t0 . ktktoknp.t0
        . tri (sd p6) .__.__. oknp.tri (sd (tat.__.p6))
        .__.__. ktktoknp.tri (sd (tat.__.di.__.p6))
    -- same thing as above, but with other ending
    , x3 $ commentS "ganesh's variation" $ section $ su $
      t0 . oknp.t0 . ktktoknp.t0
        . tri (sd p6) .__.__. oknp.tri (sd (tat.__.p6))
        .__.__. ktktoknp. tisram13 (sd (tat.__.di.__.p6))
    ]
    where
    -- A general pattern: instead of tri, do 1, then 3 in tisram.
    tisram13 x = x . tri (nadai 6 x)
    t0 = dit.__4.tang.__.taka.dikutarikitataka.din.__4.tat.__4.din.__4
    mridangam = makeMridangam
        [ (dit.tang.taka, k.u.p.k)
        , (din, od)
        , (tat, k)
        , (tat.di, k.t)
        ]

adi_muktayi_misra :: Korvai
adi_muktayi_misra = date 2019 2 26 $ ganesh $ korvai Tala.misra_chapu mridangam
    [ x3 $ section $ t0 6 . t0 8 . t0 10
        . tri (sd p6) .__.__. oknp.tri (sd (tat.__.p6))
        .__.__. ktktoknp.tri (sd (tat.__.di.__.p6))
    ]
    where
    t0 gap = dit.__4.tang.__.taka.dikutarikitataka
        . din.__n gap.tat.__n gap.din.__n gap
    mridangam = makeMridangam
        [ (dit.tang.taka, k.u.p.k)
        , (din, od)
        , (tat, k)
        , (tat.di, k.t)
        ]


-- * ganesh

misra_trikalam :: Korvai
misra_trikalam = trikalam $ date 2018 8 20 $ ganesh $
        korvai Tala.misra_chapu mridangam
    [ devel $ sd $
        let din_nana = din.__3.na.__.na.__
        in
           tat.__3.din_nana.din.__.kttk
        . din.taka.din_nana.tam.__4
        .  __.taka.din_nana.din.__.kttk
        . din.taka.din_nana.dim.__4

        . tat.__3.din_nana.din.__.kttk
        . din.taka.din.__.tat.din.__.p6
        . ta.taka.din_nana.din.__.kttk
        . din.taka.din.__.tat.din.__.p6

        . tat.__3.din_nana.din.__.p6.tam.__4.p6
        . ta.taka.din_nana.din.__.p6.dim.__4.p6
    , section $ theme
    , section $ nadai 6 theme
    , section $ su theme
    ]
    where
    theme = mconcat $ map (group • sd)
        [ tat.__3.din.__3.na.__.na.__.din.__.p6.tam.__3
        , ta.taka.din.__3.na.__.na.__.din.__.p6.tam.__3
        , tat.__3.din.__3.na.__.na.__.din.__.p6
        , ta.taka.din.__3.na.__.na.__.din.__.p6
        , tat.__3.din.__3.na.__3.p6
        , ta.taka.din.__3.na.__3.p6
        , tat.__3.din.__3.p6
        , ta.taka.din.__3
        ] ++ [sd $ tri123 (tam.__3) p6]
    mridangam = makeMridangam0
        [ (tat, k)
        , (din, od)
        , (na, on)
        , (tam, v)
        , (ta.taka, k.k.o)

        , (kttk, k.t.o.k)
        , (din.taka, o.k.o)
        , (dim, i)
        , (taka.din, k.o.od)
        ]

c_18_09_25 :: Korvai
c_18_09_25 = date 2018 9 25 $ ganesh $ korvaiS1 adi mridangam $
    tri (su (takadugutarikita.nakatiku).din.__3.tat.__3.din.__3)
    . tri_ (tat.__4.tam.__4) p5
    . sandi p5 (tri_ (tam.__) p5)
    where
    p5 = pattern $ su $ ta.__.di.__.ta.di.__.ki.ta.thom
    mridangam = makeMridangam
        [ (din.tat.din, od.k.od)
        , (tat.tam, k.i)
        , (tam, u)
        , (p5, k.__.t.__.k.t.__.k.n.o)
        ]

c_18_09_25_misra :: Korvai
c_18_09_25_misra = date 2018 9 25 $ ganesh $
        korvaiS1 Tala.misra_chapu mridangam $ sd $
    tri (su (takadugutarikita.nakatiku).din.__3.tat.__3.din.__3)
    . tri_ (tam.__4) p5
    . sandi p5 (tri p5)
    where
    p5 = pattern $ su $ ta.__.di.__.ta.di.__.ki.ta.thom
    mridangam = makeMridangam
        [ (din.tat.din, od.k.od)
        , (tam, u), (mid^tam, i)
        , (p5, k.__.t.__.k.t.__.k.n.o)
        ]

c_18_10_06 :: Korvai
c_18_10_06 = date 2018 10 6 $ tirmanam $
    comment "practice in urukalai/rendaikalai adi, rupaka, kanda, misra" $
    korvaiS1 adi mridangam $
    __D 2 . reduceBy [0, 2, 5, 7] (dheem.__) (kita.taka.takadinna)
    where
    mridangam = makeMridangam
        [ (kita.taka, t.k.o.o)
        , (dheem, od)
        ]

c_18_10_22 :: Korvai
c_18_10_22 = date 2018 10 22 $ korvaiS1 adi mridangam $ nadai 6 $
    theme.thom.__6 . theme.thom.__3.takita.thom.__6
    . theme.thom.__3.takita.thom.__3.takita.thom.__6
        . tri (tri (ta.din.__) . tri p5)
    where
    theme = ta.__.ka.takita.taka.takadinna
    mridangam = makeMridangam
        [ (theme, k.__.p.k.t.k.t.k.k.o.o.k)
        , (takita, k.p.k)
        , (thom, od)
        , (ta.din, k.od)
        ]

c_18_10_29 :: Korvai
c_18_10_29 = date 2018 10 29 $ koraippu $ ganesh $ korvaiS adi mridangam
    [ r123 p5 (ta.din.__4)
    , r123 takadinna (ta.__.din.__4)
    , r123 takita (ta.__3.din.__4)
    , r123 taka (ta.__4.din.__4)
    , r123 ka (ta.__5.din.__4)
    ]
    where
    r123 a b = restD 1
        . su (concatMap (\n -> repeat n a . repeat n b) [1, 2, 3])
    mridangam = makeMridangam
        [ (ta.din, k.od)
        , (takita, k.p.k)
        , (taka, p.k)
        , (ka, p)
        ]

tisra_mohra :: Korvai
tisra_mohra = ganesh $ date 2018 12 7 $
    comment "Also works for kanda jati triputa talam and rupaka." $
    SolkattuMohra.makeMohra adi mridangam (su • nadai 6)
        (a1, a2, a1) (b1, b2, b3)
    where
    a1 = gugu.mi.na.dit.__4.tang.__.kita.nakatiku
    a2 = na.ka.dit.__.tang.__.kita.nakatiku
    b1 = ta.langa.din.__.tat.__.gugu.tat.__.dheem.__3.ka
    b2 = ta.langa.dheem.__3.ka
    b3 = tri_ (dheem.__4) (ta.langa.gugu.tat.__)
    mridangam = makeMridangam
        [ (gugu.mi.na.dit, o.o.k.p.k)
        , (tang.kita, u.p.k)
        , (na.ka.dit, n.p.k)
        , (talang.ga, p.u.k)
        , (din.tat, o.k)
        , (gugu.tat, o.o.k)
        , (dheem, od)
        , (ka, k)
        ]
