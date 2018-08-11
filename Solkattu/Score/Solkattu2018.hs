-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
-- | Solkattu scores from 2018.
module Solkattu.Score.Solkattu2018 where
import Prelude hiding ((.), (^), repeat)

import qualified Solkattu.Instrument.KendangTunggal as KendangTunggal
import qualified Solkattu.Score.SolkattuMohra as SolkattuMohra
import Solkattu.SolkattuGlobal
import qualified Solkattu.Tala as Tala

import Global


yt_mannargudi1 :: Korvai
yt_mannargudi1 = source "Mannargudi Easwaran" $
        recording "https://www.youtube.com/watch?v=E7PLgnsFBaI"
            (Just ((0, 4, 05), (0, 5, 22))) $
        date 2018 1 12 $
        korvai adi mridangam $ map (smap su)
    [ endOn 4 $ devel $
        sarvaM 8 . theme.din.__4 . in3 (1^theme) . theme.din.__8 . sarvaD 4
        . theme.din.__4 . in3 (1^theme) . din.__6
        . trkt.theme.tat.__6
        . kttk.trkt . in3 theme . din.__2
        . restD 1 . trkt.theme.din.__2
        . kttk.trkt . in3 theme
    , startOn 4 $ eddupu (3/4) $ ending $
        repeat 3 $ theme . spread 3 tdgnt . theme . spread 2 tdgnt . theme
            . tri_ __3 (tri p5)
            -- Reduce __3 karvai in utarangam to __2 to arrive on sam.
    ]
    where
    theme = group $ takita.ta.takadinna
    mridangam = makeMridangam
        [ (takita.ta, [k, k, t, k])
        , (1^takita.ta, [p&k, p&k, t, k])
        , (din, [od])
        , (tat, [k])
        -- , (tarikita, [p, k, t, p])
        -- , (kitataka, [k, t, p, k])
        , (tarikita, [o, k, n, p])
        , (kitataka, [k, t, k, t])
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
    rest n = restM (n*6)
    mridangam = makeMridangam
        [ (p8, [k, t, k, n, k, o, o, k])
        , (din, [od])
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
        repeat 3 $ suffixes (in3 p8 . din.__.ga)
            [tat_din_, repeat 2 tat_din_, repeat 3 tat_din_]
    , eddupu (3/4) $ ending $
        repeat 3 $ suffixes (nadai 4 p8 . din.__.ga)
            [tat_din_, repeat 2 tat_din_, repeat 3 tat_din_]
    ]
    where
    p8 = group $ kita.ki.na.takadinna
    p9 = kita.ki.na.ta.takadinna
    tat_din_ = tat.__.din.__.p5

    sarva n = sarvaM (n*6)
    mridangam = makeMridangam
        [ (p8, [k, t, k, n, k, o, o, k])
        , (p9, [k, t, k, n, p, k, o, o, k])
        , (taka.naka, [n, k, n, p])
        , (din, [od])
        , (tat, [k])
        , (tang, [u])
        , (din.na, [od, n])
        , (ga, [p])
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

    sar1 = ta.ta.dit.ta.tang.ga . su (ta.lang.__.ga)
    sar2 = din.tat.din.tat.tam
    sar3 = din.din.__.na.ka.na.na.din
    mridangam = makeMridangam
        [ (sar1, [k, k, t, k, u, o, o, u, k])
        , (sar2, [o, k, o, k, od])
        , (sar3, [d, d, n, d, n, n, u])

        , (tat.dit, [p&k, p&t])
        , (ka.taka, [k, n, n])
        , (din.din.na, [o, o, k])
        , (tat.dit.tat, [p&k, p&k, p&k])
        , (kita.taka.din, [p, k, n, n, o])
        , (na.na.din, [on, on, od])
        , (tat.din, [p&k, od])
        , (din, [od])
        , (ka, [p&k])
        , (tdgnt, [p&k, t, k, n, o])
        ]

yt_karaikudi1 :: Korvai
yt_karaikudi1 = source "Karaikudi Mani" $
        recording "https://www.youtube.com/watch?v=_33FkETjQoc"
            (Just ((0, 1, 34), (0, 2, 53))) $
        similarTo "Solkattu2016" "c_16_12_06_sriram1" $
        date 2018 1 12 $
        korvai adi mridangam
    [ devel $
        sarvaD (4+1/2) . theme . tat.__ . sarvaD (1/2)
        . sarvaD (4+1/2) . theme . din.__4
    , local $ devel $
        sarvaD 5 . theme.nakadinna
        . sarvaD 5 . __ . dropM 1 theme.nakadinna
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
        let tadin = repeat 3 (ta.din.__)
        in purvangam . nadai 6 (
            tadin . p6 . tadin . p6.__.p6 . tadin . tri_ __ p6)
    ]
    where
    purvangam = theme . tri (repeat 3 nakadinna . din.__3)
        . theme . tri (repeat 2 nakadinna . din.__)
        . theme . tri (nakadinna.din)

    theme = group $ su $ theme1 . nakatiku
    theme1 = tat.__.dit.__.kita.ki.na.ta.ki.taka
    nakadinna = group $ su $ na.ka.din.na
    kitatakatam = kttk.tam.__
    mridangam = makeMridangam
        [ (theme1, [k, t, k, t, k, n, o, k, o&t, k])
        , (tat, [k])
        , (ta, [k])
        , (din, [od])
        -- TODO technique: if preceded by a rest, play kook
        , (nakadinna, [n, o, o, k])
        , (kitatakatam, [o, k, n, p, u])
        , (tam, [od])
        ]

c_18_03_19 :: Korvai
c_18_03_19 = date 2018 3 19 $ ganesh $ korvai Tala.misra_chapu mridangam $
    map devel
    [ (kitataka.sd2 din) <== 7 . sarvaD 7
    , (kitataka.sd2 din) <== 7 . 1^(kitataka.sd2 din) <== 7
    , kitataka . sd2 (din.na) . kitataka . sd2 (din.din.na)
        . 1^(kitataka . sd2 (din.na) . kitataka . sd2 (din.din.na))
    , kitataka . sd2 (din.na) . tri_ (din.__) kitataka
        . 1^(kitataka . sd2 (din.na) . tri_ (din.__) kitataka)
    , (kitataka.sd2 din) <== 7
        . sarvaD 6.5 . tri_ (sd (din.__3)) (tat.__.kitataka)
        . din.__4 . sarvaD 5
        . tri_ (din.__4) (tat.__4.kitataka)
        . din.__4 . sarvaD 4.5
        . tri_ (din.__) (tat.__.tat.__4.kitataka)
        . din.__4 . sarvaD 6
    ] ++
    [ ending $ purvangam 3 . tri_ (sd2 (ta.din)) p5
    ] ++ map (var • dateS 2018 3 27 • ending)
    [ purvangam 2 . tri_ (sd2 (ta.din)) (kp.p5)
    , purvangam 1 . tri_ (sd2 (ta.din)) (kpnp.p5)
    , purvangam 3 . tri_ (tam.__) (taka.ti.ku.p5)
    , purvangam 2 . tri_ (tam.__) (ta.__.ka.ti.__.ku.p5)
    , purvangam 1 . tri_ (tam.__) (ta.__.__.ka.ti.__.__.ku.p5)
    ]
    where
    p5 = group $ kita.taka.tari.kita.taka
    purvangam gap =
        tat.__.tat.__4.kitataka . din_din_na__ gap
        . tat.__4.kitataka . din_din_na__ gap
        . tat.__.kitataka . din_din_na__ gap
    kitataka = group $ kita.taka
    din_din_na__ gap = sd2 (din.din) . sd (na.__n gap)
    mridangam = makeMridangam
        [ (kitataka, [k, t, k, o])
        , (1^kitataka, [k, t, k, p])
        , (na, [on])
        , (1^na, [n])
        , (din, [od])
        , (1^din, [d])
        , (tat, [p&k])
        , (ta, [k])
        , (p5, [k, t, p, k, p, k, t, k, n, o])
        , (tam, [u])
        , (taka.ti.ku, [k, p, n, p])
        ]

c_18_03_28 :: Korvai
c_18_03_28 = date 2018 3 27 $ ganesh $ korvaiS Tala.misra_chapu mridangam
    [ (theme.din.__4) <== 7 . sarvaD 7
    , (theme.din.__4) <== 7 . 1^(theme.din.__4) <== 7
    , (theme.din.__4) . theme.din.__4 . na.__4
        . 1^(theme.din.__4 . theme.din.__4 . na.__4)
    , (theme.din.__4) . theme . takeD 2 theme
        . 1^(theme.din.__4) . 1^theme . takeD 2 (1^theme)
    , (theme.din.__4) . kitakinakadin.__ . repeat 2 (nakadin.__) . ga
        . 1^((theme.din.__4) . kitakinakadin.__ . repeat 2 (nakadin.__) . ga)
    , (theme.din.__4) . kitakinakadin.__ . repeat 3 (ka.din.__)
        . 1^((theme.din.__4) . kitakinakadin.__ . repeat 3 (ka.din.__))
    , (theme.din.__4) <== 7 . sarvaD 6 . tri_ (din.__4) theme
    , sarvaD 7 . sarvaD 4.5 . tri_ (din.__4) (tat.__.theme)
    , sarvaD 7 . sarvaD 3 . tri_ (din.__4) (tat.__4.theme)
    , repeat 3 (tat.__4.theme.din.__4.din.__4.na.__4) . sd (tri_ __ p6)
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
        [ (kitakinakadin, [k, t, k, n, o, od])
        , (1^kitakinakadin, [k, t, k, n, p, d])
        , (ka.din, [k, od])
        , (1^(ka.din), [k, d])
        , (theme, [k, t, k, n, o, od, k])
        , (din, [od])
        , (1^theme, [k, t, k, n, p, d, k])
        , (1^din, [d])
        , (na, [on])
        , (1^na, [n])
        , (ga, [p])
        , (tat, [p&k])

        , (tari.kita, [p, k, n, p])
        , (kita.kita, [k, t, k, t])
        , (p10, [k, t, p, k, p, k, t, k, n, o])
        ]

c_18_04_25 :: Korvai
c_18_04_25 = date 2018 4 25 $ ganesh $
    similarTo "Mridangam2013" "dinnagina_sequence" $
    korvaiS Tala.misra_chapu mridangam
    [ (4*7) ==> theme
    , (2*7) ==> theme . (2*7) ==> theme
    , repeat 2 (7 ==> theme . 7 ==> theme)
        -- TODO open and closed sarva: o_k_d, p_k_d
    , repeat 4 $ takeM 12 (1^theme) . theme
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
    theme = group $ dhom.__.ka.__ . repeat 3 (din.na.gin.na)
    -- TODO other themes
    mridangam = makeMridangam
        [ (theme, [t, k, o, k, t, k, o, k, t, k, o, k, t, k])
        , (1^theme, [o, k, o, k, t, k, o, k, t, k, o, k, t, k])
        , (dhom, [o])
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
        [ (theme1_, [p&k, p&t, k, t, k, n, o, k, o&t, k])
        , (theme2, [t, k, o, k, t, k, o, k, o, k, o, u, k])
        , (din.tat.din.na.tat, [o, k, o, k, k])
        , (din, [o])
        , (1^din, [od])
        ]

-- * HS Sudhindra

-- | Shri HSS speaks it this way.
dikutarikitataka :: SequenceT sollu
dikutarikitataka = nakatiku

misra_tani :: [Part] -- realizeParts realizep misra_tani
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
misra_tani1 = date 2018 7 16 $ sudhindra $ korvai Tala.misra_chapu mridangam
    [ x2 $ section $ sd2 $
        tam.__3.tam.__.tam.__
      . tam.__3.tam.__.su (taka.jonu)
    , section $ sd2 $
        na.din.na.na.din.din.na.din.__n 7
        . na.din.na.su (na.na).din.din.na.din.__n 7
    , section $ (7*4) ==> (ta.takita.takadinna)
    ]
    where
    mridangam = makeMridangam
        [ (ta.takita.takadinna, [t, k, o, o, k, t, p, k])
        , (tam, [p&v])
        , (taka.jonu, [k, o, o, k])
        , (na, [on])
        , (din, [od])
        ]

misra_tani2 :: Korvai
misra_tani2 = date 2018 7 16 $ sudhindra $ korvai Tala.misra_chapu mridangam
    [ x4 $ section $ sd $ taka.ta.ta.din.__6.tatadin_
    , x4 $ section $ sd $ taka.ta.ta.din.__4.taka.tatadin_
    , x3 $ section $ sd $ taka.repeat 3 tatadin_
    , section $ sd (taka.repeat 3 tatadin_)
    , x2 $ section $ taka.taka . sd (repeat 3 tatadin_)
    , x4 $ section $ taka.taka . sd tatadin_ . taka.taka.din.__4 . sd tatadin_
    , x4 $ section $ taka.taka . sd tatadin_ . repeat 2 (taka.taka.din.__4)
    , x3 $ section $ taka.taka.taka.taka.din.__4 . repeat 2 (taka.taka.din.__4)
    , ending $ tri (taka.taka.din.__4)
        . tri_ (din.__4) (ta.ta.kita.takadinna)
    ]
    where
    tatadin_ = ta.ta.din.__
    mridangam = makeMridangam
        [ (taka.ta.ta.din, [on, k, on, on, od])
        , (ta.ta.din, [on, on, od])
        , (talang.gu, [p, u, k])
        , (taka.taka, [o&j, y, o&j, y])
        , (din, [od])
        -- TODO standard pattern
        , (ta.ta.kita.takadinna, [t, k, o, o, k, t, o, k])
        ]

-- abhipriyam (telugu) -> thoughts
misra_to_mohra1a :: Korvai
misra_to_mohra1a = date 2018 7 2 $ sudhindra $
    korvaiS Tala.misra_chapu (mridangam<>kendang)
    [ din.__8 . tA1
    , sarvaD (7*3)
    , sarvaD 4 . tA
    , sarvaD 7 . repeat 3 (sarvaD 4 . dropD 2 tA1)
    , repeat 3 tA . repeat 2 (dropD 5 tA) . repeat 2 (dropD 7 tA)
        . repeat 2 (dropD 9 tA)
    ]
    where
    tA = group $ tA0.dikutarikitataka.tA1.din.__4
    tA0 = tat.__.dit.__.tarikita.kitataka
    tA1 = tat.__.tat.__.tarikita.gu.gu.takita.tong.__.ka.din.__.tat.__
    mridangam = makeMridangam
        [ (tA0, [p&k, p&t, k, t, k, n, p, k, t, k])
        , (tA1, [k, k, p, k, t, k, o, o, k, p, k, od, k, od, k])
        , (din, [od])
        ]
    kendang = makeKendang1
        [ (tA0, [pk, pk, k, p, k, t, p, k, p, k])
        , (tA1, [p, p, p, k, p, p, a, a, k, p, k, a, o, a, o])
        , (din, [a])
        ] where KendangTunggal.Strokes {..} = KendangTunggal.notes

misra_to_mohra1b :: Korvai
misra_to_mohra1b = date 2018 7 2 $ sudhindra $
    korvai Tala.misra_chapu (mridangam<>kendang)
    [ x4 $ devel sarva
    , x4 $ devel $ sarva `replaceEnd` group (ta.takita.takadinna)
    , x4 $ devel $ takeD 2 sarva.din.__4. theme
    , ending $ theme.takadinna.din.__6.tat.__4
        . theme . repeat 2 takadinna.din.__6 . tat.__4.dit.__4
        . theme . trin (tam.__n 16)
            (tri_ (din.__6) (tri takadinna))
            (tri_ (din.__6) (repeat 2 takadinna))
            (tri_ (din.__6) takadinna)
    ]
    where
    sarva = taka.taka.din.__.kita.din.__.kita.repeat 2 (taka.taka.din.__.kita)
    theme = group $ theme0.dikutarikitataka
    theme0 = tarikita.kitataka
    mridangam = makeMridangam
        -- sarva
        [ (taka.taka, [j, y, j, y])
        , (din.kita, [d, lt p, k])
        , (ta.takita.takadinna, [t, k, o, o, k, t, o, k])

        -- ending
        , (din, [od])
        , (tat, [p&u])
        , (tat.dit, [p&u, p&u])

        , (theme0, [k, t, k, n, p, k, t, k])
        , (takadinna, [n, o, o, k])
        , (repeat 3 takadinna, [k, o, o, k, t, o, o, k, t, o, o, k])
        , (tam, [od])
        ]
    kendang = makeKendang1
        [ (taka.taka, [t, k, t, o])
        , (din.kita, [u, lt p, k])
        , (ta.takita.takadinna, [t, p, a, a, p, k, p, p])

        , (din, [a])
        , (tat, [u])
        , (tat.dit, [u, u])

        , (theme0, [k, p, k, t, p, k, p, k])
        , (takadinna, [t, a, a, p])
        , (repeat 3 takadinna, [t, a, a, k, p, a, a, k, p, a, a, p])
        , (tam, [a])
        ] where KendangTunggal.Strokes {..} = KendangTunggal.notes

to_mohra_farans :: Korvai
to_mohra_farans = date 2018 7 2 $ sudhindra $ faran $
    korvai Tala.misra_chapu mridangam
    [ section $ sarvaD 14
    , x2 $ section $ sarvaD (7+4) . tri (talang.__.gu)
    , x2 $ section $ sarvaD (7+4) . tri takadinna
    , x2 $ section $ sarvaD (7+4) . tri (gu.gu.na.na)
    , section $ repeat 2 $ sarvaD 4 . tri dinnakitataka
    , section $ repeat 2 $ sarvaD 4 . repeat 2 takadinnakitataka
    , x2 $ section $ sarvaD 4 . repeat 2 takadinnakitataka
      . repeat 4 takadinnakitataka . dinnakitataka
    , section $ sarvaD 4 . repeat 2 takadinnakitataka
      . repeat 3 (repeat 2 takadinnakitataka . repeat 4 dinnakitataka)
    , section $ tri_ (din.__4) (repeat 4 dinnakitataka)
    ]
    where
    -- Mridangam2018.c_18_07_02_sarva
    sarva = sd $ taka.ta.ta.din.__4.taka.ta.ta.din.__
    dinnakitataka = group $ din.na.kttk
    takadinnakitataka = group $ taka.din.na.kttk
    mridangam = makeMridangam
        [ (sarva, [on, k, on, on, od, on, k, on, on, od])
        , (talang.gu, [p, u, k])
        , (takadinna, [p, n, k, k])
        , (gu.gu.na.na, [o, o, n, n])
        , (dinnakitataka, dinna)
        , (takadinnakitataka, [o, k] ++ dinna)
        , (din, [od])
        ]
        where dinna = [o, n, k, t, o, k]

misra_to_mohra3 :: Korvai
misra_to_mohra3 = date 2018 7 2 $ sudhindra $
    korvai Tala.misra_chapu (mridangam<>kendang) $ map ending
    [ reduceTo 4 2 theme . tri theme2
    ]
    where
    theme = tat.__.dit.__.takadinna.din.__.tat.__.din.__4
    theme2 = ta.__.di.__.ki.__.ta.__.ta.tarikita.thom
    mridangam = makeMridangam
        [ (theme, [k, t, k, o, o, k, od, k, od])
        , (theme2, [k, t, k, n, p, k, t, k, n, o])
        ]
    kendang = makeKendang1
        [ (theme, [p, p, t, a, a, u, a, o, a])
        -- TODO similar technique for kendang as for mridangam?
        , (din.na.din.tat.din, [k, u, a, o, a])
        , (theme2, [o, p, k, t, a, o, p, k, t, a])
        ] where KendangTunggal.Strokes {..} = KendangTunggal.notes

misra_to_mohra4 :: Korvai
misra_to_mohra4 = date 2018 7 2 $ sudhindra $
    korvai Tala.misra_chapu mridangam
    [ section $ restD 3 . tat.__.tat.__.kitataka.dikutarikitataka
        . tri123 (thom.__4) dikutarikitataka
    , x2 $ section $ repeat 4 (din_gu_ . tarikitataka) . dim.__4 . tat.__4
    , x2 $ section $ din_gu_ . tri_ (dim.__4) tarikitataka . din_gu_
        . taka.dikutarikitataka . dim.__4 . tat.__4
    , x2 $ section $ repeat 2 (prefixes [dim, thom] (__.taka.dikutarikitataka))
        . dikutarikitataka
    , section $ repeat 7 $ gugunana_trkt
    , x2 $ section $ repeat 3 (gu.gu.na.na) . gugunana_trkt
    , x2 $ section $ repeat 2 (gu.gu.na.nang.__.gu) . gugunana_trkt
    , x2 $ section $ repeat 2 (gu.gu.na.na.taka) . gugunana_trkt
    , section $ repeat 3 gugunana_trkt . tri_ (dim.__8) gugunana_trkt
    ]
    where
    din_gu_ = din.__4.gu.__
    gugunana_trkt = group $ gu.gu.na.na.tarikita.dikutarikitataka
    tarikitataka = group $ tarikita.taka
    mridangam = makeMridangam
        [ (tat, [k])
        , (kitataka, [k, t, p, k])
        , (din.gu, [od, o])
        , (taka, [p, k])
        , (dim, [od])
        , (thom, [od])
        , (tarikita, [p, k, t, k])
        , (gu.gu.na.na, [o, o, n, n])
        , (gu.gu.na.nang.gu, [o, o, n, n, k])
        ]

misra_mohras :: Korvai
misra_mohras = date 2018 7 11 $ sudhindra $
    SolkattuMohra.makeMohras Tala.misra_chapu mridangam
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
        [ (din.taka, [u, p, k])
        , (tam.taka, [u, p, k])
        , (tiku.tarikita.thom, [t, p, u, p, k, t, o])
        , (dit, [k])
        , (talang.gu, [p, u, k])
        , (din.tat, [o, k])
        , (din, [od])
        , (theme, [k, t, k, o, o, k, od])
        ]

misra_muktayi1 :: Korvai
misra_muktayi1 = date 2018 7 11 $ sudhindra $
    korvai1 Tala.misra_chapu mridangam $ section $
    reduceTo 4 2 theme . tri_ (sd __) (sd (ta.__.di.__.p5))
    where
    theme = tat.__.dit.__.tat.__.tat.__.kita.thom.__4
    -- TODO this should be a p5, but I should be able to configure the specific
    -- one I want.  Also, alternate realization is ktktpkpko.
    p5 = su $ kita.kita.tari.kita.thom.__
    -- TODO replace reduction fingering.  How should I express irregular
    -- fingerings like this?  Maybe with per-korvai technique?
    _fingerings =
        [ [] -- same: [k, t, k, k]
        , [k, t, k]
        , [k, t]
        ]
    mridangam = makeMridangam
        [ (tat, [k])
        , (dit, [t])
        , (kita.thom, [p, k, o])
        , (ta.di, [p&k, p&k])
        , (p5, [k, t, k, t, p, k, p, t, o])
        ]

trikalam1 :: Korvai
trikalam1 = date 2018 7 16 $ trikalam $ sudhindra $ korvai adi mridangam
    [ startOn 4 $ endOn 4 $ section $ theme
    , startOn 4 $ endOn 4 $ section $ repeat 2 $ nadai 6 theme
    , startOn 4 $ section $ repeat 3 $ su theme
    ]
    where
    theme = tam.__4.theme0.di.__6.p6
        . ta.takita.theme0.di.__6.p6
        . tam.__4.theme0.di.__6 . tri_ (di.__6) p6
    theme0 = taka.din.__.din.__.tat.__
    mridangam = makeMridangam
        [ (tam, [p&v])
        , (theme0, [k, o, od, od, k])
        , (ta.takita, [k, t, k, t])
        , (di, [od])
        ]
    -- At higher speed, tadi kitathom becomes tadi kitaka.

trikalam2 :: Korvai
trikalam2 = date 2018 7 16 $ trikalam $ sudhindra $
    korvai adi mridangam $ map section
    [ restD 2 . seq . repeat 2 (nadai 6 seq) . repeat 3 (su seq)
    ]
    where
    seq = reduce3 2 ø theme . tri p6
    theme = tat.__.dit.__.takadinna.di.__4
    mridangam = makeMridangam
        [ (theme, [k, t, k, o, o, k, od])
        ]

e_sarva1 :: Korvai
e_sarva1 = sarvalaghu $ date 2018 7 16 $ sudhindra $
    korvai Tala.misra_chapu mridangam $ map section $ map (nadai 3)
    [ din.__.gu.takita.din.__.gu . repeat 2 (din.__.gu.takita)
    ]
    where
    mridangam = makeMridangam
        [ (din.gu.takita, [od, y, o&j, y, od])
        , (din.gu, [od, k])
        ]

e_sarva2 :: Korvai
e_sarva2 = sarvalaghu $ date 2018 7 25 $ sudhindra $
        korvai Tala.misra_chapu mridangam $ map section
    [ sarva
    , sarva `replaceEnd` group (ta.takita.takadinna)
    , tri (taka.taka.din.__.kita) . tri_ (tam.__4) (group (ta.takita.takadinna))
    ]
    where
    sarva = taka.taka.din.__.kita.din.__.kita.repeat 2 (taka.taka.din.__.kita)
    mridangam = makeMridangam
        [ (taka.taka, [j, y, j, y])
        , (din.kita, [d, lt p, k])
        , (ta.takita.takadinna, [t, k, o, o, k, t, o, k])
        , (tam, [od])
        ]

e_misra_tisra :: Korvai
e_misra_tisra = exercise $ korvai Tala.misra_chapu mempty $
    map section $ map (nadai 3)
    [ repeat 7 takita
    , repeat 2 takita . tri (group p5)
    , repeat 1 takita . tri (group p6)
    , repeat 7 takita . tri (group p7)
    , repeat 6 takita . tri (group p8)
    , repeat 5 takita . tri (group p9)
    ]

-- * adi

adi_tani :: [Part] -- realizeParts realizep adi_tani
adi_tani =
    [ K adi_tani1 All
    , K trikalam1 All
    , K e_sarva1_tisra All
    , K e_adi_tisra All
    , K c_18_08_03 All
    , K adi_tani2 All
    , Comment "mohra"
    , K adi_muktayi All
    ]

adi_tani1 :: Korvai
adi_tani1 = date 2018 8 3 $ sudhindra $ korvai adi mridangam
    [ section $ repeat 6 (tam.__4) . tri_ din tkdn
    , section $ repeat 2 $ repeat 2 (tam.__4) . tri_ din tkdn
    , section $ tri_ (din.__4) (tri_ din tkdn)
    , section $ sd (na.din.din.na) . na.na.sd (din.din.na)
        . kita.taka. sd (din.din) . talang_gu
    , x2 $ section $ repeat 2 $ sd (na.din.din.na) . talang_gu
    , section $ tri_ (din.__4) talang_gu
    , x2 $ section $ repeat 3 sarva2 . su (faran1.nakatiku)
    , section $ repeat 2 (sarva2 . su (faran1.nakatiku))
        . repeat 4 (su (faran1.nakatiku))
        . tri_ (din.__4) (su (faran1.nakatiku))
    , section $ repeat 2 (sarva3a.sarva3')
        . repeat 2 (sarva3b.sarva3')
        . repeat 2 (sarva3c.sarva3')
        . repeat 2 (sarva3d.sarva3')
    , section $ sarva3b.sarva3' . sarva3b . su (faran2.nakatiku)
        . repeat 2 (sarva3b . su (faran2.nakatiku))
    , endOn 4 $ section $
        repeat 2 (su (faran2.nakatiku))
        . tri_ (din.__4) (su (faran2.nakatiku))
    ]
    where
    talang_gu = su $ talang.__.gu.talang.__.gu.taka.taka.talang.__.gu
    sarva2 = taka.ta.din.__.ta.din.__
    faran1 = gu.gu.taka.tari.kita
    faran2 = kita.gu.gu.tari.kita
    sarva3' = 10^sarva3a
    sarva3a = repeat 2 $ naka.na.din
    sarva3b = 2^sarva3a
    sarva3c = 3^sarva3a
    sarva3d = su (naka.naka).na.din . naka.na.din
    mridangam = makeMridangam
        [ (tam, [p&u])
        , (din, [od])
        , (na, [n])
        , (kita.taka, [k, t, p, k])
        , (talang.gu, [p, u, k])
        , (taka.taka, [o, k, o, k])
        , (sarva2, [on, y, on, od, on, od])
            -- TODO sarva
            -- o_oo_oo_ __oo_oo_ o______
        , (faran1, [o, o, k, t, o, k, t, k])
        , (faran2, [t, k, o, o, k, t, p, k])
            -- TODO HSS plays first one as ktoo... rest as tkoo...
        , (sarva3', [on, y, n, d, n, y, n, d])
        , (sarva3a, [on, y, on, od, n, o&y, on, d])
        , (sarva3b, [on, y, on, od, n, y, on, od])
        , (sarva3c, [on, on, on, od, n, y, on, od])
        , (sarva3d, [on, y, on, y, on, od, n, y, on, od])
        ]

e_sarva1_tisra :: Korvai
e_sarva1_tisra = exercise $ date 2018 7 25 $ sudhindra $
    korvai adi mridangam $ map section $ map (nadai 6)
    [ repeat 5 din_gutakita . din.__.gu . tri p5
    , repeat 5 din_gutakita . tri p6
    , repeat 4 din_gutakita . din.__.gu . tri p7
    , repeat 4 din_gutakita . tri p8
    , repeat 3 din_gutakita . din.__.gu . tri p9
    ]
    where
    din_gutakita = din.__.gu.takita
    mridangam = makeMridangam
        [ (din_gutakita, [od, y, o&j, y, od])
        , (din.gu, [od, k])
        ]

-- takadimi takajonu
-- where l is only slighly on saddam
-- n.l.d.d
-- increase the speed
-- also n.d.d.d

e_adi_tisra :: Korvai
e_adi_tisra = exercise $ date 2018 7 30 $ sudhindra $
    korvai adi mridangam $ map (smap (nadai 6))
    [ section $ sarva 8
    , x2 $ section $ sarva 7 . tarikitadiku
    , section $ repeat 2 $ sarva 3 . tarikitadiku
    , section $ sarva_.tarikitadiku.sarva_.tarikitadiku
        . tri_ (tang.__.gu) tarikitadiku
    , x2 $ section $ sarva 6 . tri dinna
    , section $ repeat 2 (sarva 2 . tri dinna)
    , section $ sarva 2 . repeat 3 (tri dinna)
    , x2 $ section $ sarva 6 . tari3
    , section $ repeat 3 (sarva 2 . tari3) . tari3 . tari3
    , x2 $ section $ sarva 6 . repeat 2 takadinna
    , section $ repeat 2 $ sarva 2 . repeat 2 takadinna
    , section $ repeat 8 takadinna
        . repeat 6 takadinna . tri dinna
    , section $ repeat 6 takadinna . tri dinna
    , section $ repeat 6 takadinna . tari3
    , section $ repeat 6 takadinna . din_trktkt
    , section $ repeat 2 $ repeat 2 takadinna . din_trktkt
    , x2 $ section $ repeat 4 $ din_trktkt
    , x2 $ section $ repeat 2 $ din.__ . repeat 4 trktkt . tarikitadiku
    , section $ repeat 4 tarikitadiku
        . tri (tri_ (tam.__3) tarikitadiku)
    ]
    where
    sarva n = repeat n sarva_
    sarva_ = taka.ta.ta.dim.__
    tarikitadiku = group $ su $ tari.kita.taka.diku.kita.taka
    dinna = group $ din.na.kttk
    tari3 = tri trktkt
    trktkt = group $ trkt.kttk
    takadinna = group $ taka.dinna
    din_trktkt = din.__.trktkt.tarikitadiku
    mridangam = makeMridangam0
        [ (sarva_, [n, k, n, n, d])
            -- o_ooo_ __ooo_ o___ ____
            --  _ooo__ _ooo_o ____ ___o
        , (tarikitadiku, [n, p, k, t, p, k, t, p, k, t, p, k])
        , (tang.__.gu, [od, o])
        , (dinna, [o, n, k, t, o, k])
        , (taka, [o, k])
        , (trktkt, [n, p, k, t, k, t, p, k])
        , (din.trktkt, [od, on, __, k, t, k, t, p, k])
            -- high speed variant of trktkt, drops the tha
            -- say tari.kita.kita.taka, play nang.__.kita.kita.taka
        , (din, [od])
        , (tam, [od])
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
    mridangam = makeMridangam0
        [ (dit.tang.kita, [k, u, p, k])
        , (din, [od])
        , (tat, [k])
        , (din.gu, [od, lt o])
        ]

adi_tani2 :: Korvai
adi_tani2 = date 2018 8 3 $ sudhindra $ korvai adi mridangam $ map section
    [ restD 4 . su (tri_ (tam.__4) nakatiku)
    , repeat 2 $ repeat 2 (din.__.gu.trkt.tk) . dim.__.tat.__
    , repeat 2 $ din.__.gu . tri_ (dim.__) (trkt.tk)
    , repeat 2 $ repeat 2 (din.__.gu.trkt.tk) . su nakatiku
    , repeat 2 $ repeat 2 (dim.su (taka.tikutarikitataka)) . su dikutarikitataka
    , su $ repeat 4 (faran1.dikutarikitataka)
        . repeat 2 (repeat 4 (takeM 4 faran1) . faran1.dikutarikitataka)
        . repeat 2 (tri_ taka (takeM 4 faran1) . faran1.dikutarikitataka)
        . repeat 2 (repeat 2 faran2 . takeM 4 faran1 . faran1.dikutarikitataka)
    , su $ repeat 4 (faran1.dikutarikitataka)
        . repeat 4 dikutarikitataka
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
        [ (tam, [od])
        , (dim, [od])
        , (din.gu, [od, lt o])
        , (tari.kita.taka, [k, t, k, t, p, k])
        , (tat, [k])
        , (dim.taka, [on, p, k])
        , (faran1, [o, o, k, t, p, k, t, k])
        , (faran2, [o, o, k, t, k])
        , (taka, [p, k])
        ]

adi_muktayi :: Korvai
adi_muktayi = date 2018 8 3 $ sudhindra $ korvai1 adi mridangam $
    x3 $ section $ su $
    t0 . oknp.t0 . ktktoknp.t0
        . tri (sd p6) .__.__. oknp.tri (sd (tat.__.p6))
        .__.__. ktktoknp.tri (sd (tat.__.di.__.p6))
    where
    t0 = dit.__4.tang.__.taka.dikutarikitataka.din.__4.tat.__4.din.__4
    mridangam = makeMridangam
        [ (dit.tang.taka, [k, u, p, k])
        , (din, [od])
        , (tat, [k])
        , (tat.di, [k, t])
        ]
