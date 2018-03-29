-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
-- | Solkattu scores from 2018.
module Solkattu.Score.Solkattu2018 where
import Prelude hiding ((.), (^), repeat)

import Solkattu.SolkattuGlobal
import qualified Solkattu.Tala as Tala


yt_mannargudi1 :: Korvai
yt_mannargudi1 = source "Mannargudi Easwaran" $
        recording "https://www.youtube.com/watch?v=E7PLgnsFBaI"
            (Just ((0, 4, 05), (0, 5, 22))) $
        date 2018 1 12 $
        korvai adi mridangam $ map (smap su)
    [ devel $ endOn 4 $ section $
        sarvaM 8 . theme.din.__4 . in3 (1^theme) . theme.din.__8 . sarvaD 4
        . theme.din.__4 . in3 (1^theme) . din.__6
        . su tarikita.theme.tat.__6
        . su (kitataka.tarikita) . in3 theme . din.__2
        . restD 1 . su tarikita.theme.din.__2
        . su (kitataka.tarikita) . in3 theme
    , ending $ startOn 4 $ eddupu (3/4) $ section $
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
    [ devel $ section $
        sarva 6 . taka.naka.p8 . sarva 2 . __.__.tat.__.p8
        . sarva 1 .__.__. tktu . in3 p8
    , devel $ section $
        sarva 2 . tang.__.tang.__ . p8 . nadai 4 p8 . din.__3 . p9
            . din.__.na.__
        . tri_ (din.__.na.__) p8 . in3 p8
    , devel $ scomment "gradual transition to nadai 4" $ section $
        din.__4 . join (din.__4)
        (replicate 2 (p8 . in3 p8) ++ replicate 2 (p8 . nadai 4 p8))

    , ending $ eddupu (3/4) $ section $
        repeat 3 $ suffixes (in3 p8 . din.__.ga)
            [tat_din_, repeat 2 tat_din_, repeat 3 tat_din_]
    , ending $ eddupu (3/4) $ section $
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
    [ devel $ section $
        sarvaD (4+1/2) . theme . tat.__ . sarvaD (1/2)
        . sarvaD (4+1/2) . theme . din.__4
    , devel $ local $ section $
        sarvaD 5 . theme.nakadinna
        . sarvaD 5 . __ . dropM 1 theme.nakadinna
        . tam.__4 . theme.nakadinna
        . tam.__4 . __ . dropM 1 theme.nakadinna
        -- . theme . dropM 1 theme . dropM 2 theme . p5
        -- . theme . dropM 2 theme . dropM 4 theme . tri_ tam nakadinna

        . theme.nakadinna.tam.__3 . dropM 1 theme . tri_ tam nakadinna
    , ending $ section $ purvangam . nadai 6 (
        tri kitatakatam
        . p5
        . tri kitatakatam
        . p5.p5
        . tri kitatakatam
        . p5.p5.p5
        )
    -- simple versions for dummies like me
    , local $ ending $ section $
        let t = ta.din.__.p6
        in purvangam . nadai 6 (trin (ta.__3.din.__3) t (t.t) (t.t.t))
    , local $ ending $ section $
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
    kitatakatam = su (kita.taka) . tam.__
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
c_18_03_19 = date 2018 3 19 $ ganesh $ korvaiS Tala.misra_chapu mridangam
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
    , tat.__.tat.__4.kitataka . dindinna
        . tat.__4.kitataka . dindinna
        . tat.__.kitataka . dindinna
        . tri_ (sd2 (ta.din)) p5
    ]
    where
    p5 = group $ kita.taka.tari.kita.taka
    kitataka = group $ kita.taka
    dindinna = sd2 (din.din) . sd (na.__3)
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
        ]
