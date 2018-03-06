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
        eddupu (3/2/2) $
        korvai adi mridangam $ map su
    [ sarvaM 8 . theme.din.__4 . group3 (1^theme) . theme.din.__8 . sarvaD 4
        . theme.din.__4 . group3 (1^theme) . din.__6
        . su tarikita.theme.tat.__6
        . su (kitataka.tarikita) . group3 theme . din.__2
        . restD 1 . su tarikita.theme.din.__2
        . su (kitataka.tarikita) . group3 theme
        -- Korvai starts here, but it starts on arudi.
        -- TODO so I need arbitrary section boundaries.
        . repeat 3 (theme . spread 3 tdgnt . theme . spread 2 tdgnt . theme
            . tri_ __3 (tri p5))
            -- Reduce __3 karvai in utarangam to __2 to arrive on sam.
    ]
    where
    theme = takita.ta.takadinna
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
e_18_02_26 = ganesh $ exercise $ date 2018 2 26 $ korvai adi mridangam $
    map (nadai 6)
    [ rest 2 . tri (group3 p8)
        . rest 2 . mconcat fwd . rest 2 . mconcat bwd
    , join (din.__6) (replicate 3 (group3 p8))
        . join (din.__6) fwd . join (din.__6) bwd
    -- TODO technique: kt_ kn_ ko_ ok_ D -> kt_pkn_pko_ook_oD
    ]
    where
    p8 = kita.ki.na.takadinna
    fwd = [p8, group3 p8, group4 p8]
    bwd = [group4 p8, group3 p8, p8]
    rest n = restM (n*6)
    mridangam = makeMridangam
        [ (p8, [k, t, k, n, k, o, o, k])
        , (din, [od])
        ]

yt_mannargudi2 :: Korvai
yt_mannargudi2 = source "Mannargudi Easwaran" $
        recording "https://www.youtube.com/watch?v=E7PLgnsFBaI"
            (Just ((0, 9, 30), (0, 10, 30))) $
        korvai adi mridangam $ map (nadai 6 • su)
    [ sarva 6 . taka.naka.p8 . sarva 2 . __.__.tat.__.p8
        . sarva 1 .__.__. tktu . group3 p8
    , sarva 2 . tang.__.tang.__ . p8 . nadai 4 p8 . din.__3 . p9 . din.__.na.__
        . tri_ (din.__.na.__) p8 . group3 p8
    , din.__4 . join (din.__4)
        (replicate 2 (p8 . group3 p8) ++ replicate 2 (p8 . nadai 4 p8))
        -- This is actually a gradual transition from group3 to nadai 4.

    -- eddupu changes to din.__.ga
    ,     nadai 4 p8 . din.__ . tat_din_
        . nadai 4 p8 . din.__ . repeat 2 tat_din_
        . nadai 4 p8 . din.__ . repeat 3 tat_din_
    ]
    where
    p8 = kita.ki.na.takadinna
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
        korvai Tala.triputa_tala mridangam $
    [ __.__.sar1.sar2.__.na.na.din.__
        . __. theme1 . din.__.na.na
        . din.__7 . sar3 . __.__. theme1.din.__.na.na.din.__
        . su (__.ka).theme1.din.__.na.na.din.__
        . su (__.ka).theme1
    , theme1.din.__ . su __ . fill1 . theme2.din.__ . su __ . fill1
        . theme1 . theme2 . utarangam
    ]
    where
    sar1 = ta.ta.dit.ta.tang.ga . su (ta.lang.__.ga)
    sar2 = din.tat.din.tat.tam
    sar3 = din.din.__.na.ka.na.na.din

    theme1 = group $ su (tat.__3.dit.__.ka.taka) . din.din.na
    theme2 = group $ su (ka.tat.__.dit.__.ka.taka) . din.din.na

    utarangam = su __ . tri (group $ su $ tat.dit.tat . __ . kita.taka.din)

    fill1 = tat.din . su tdgnt
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
        korvai adi mridangam
    [ sarvaD (4+1/2) . theme . tat.__ . sarvaD (1/2)
        . sarvaD (4+1/2) . theme . din.__4
    -- My own development, not from the recording.
    , sarvaD 5 . theme.nakadinna
        . sarvaD 5 . __ . dropM 1 theme.nakadinna
        . tam.__4 . theme.nakadinna
        . tam.__4 . __ . dropM 1 theme.nakadinna
        -- . theme . dropM 1 theme . dropM 2 theme . p5
        -- . theme . dropM 2 theme . dropM 4 theme . tri_ tam nakadinna

        . theme.nakadinna.tam.__3 . dropM 1 theme . tri_ tam nakadinna
    , purvangam . nadai 6 (
        tri kitatakatam
        . p5
        . tri kitatakatam
        . p5.p5
        . tri kitatakatam
        . p5.p5.p5
        )
    -- simple versions for dummies like me
    , let t = ta.din.__.p6
        in purvangam . nadai 6 (trin (ta.__3.din.__3) t (t.t) (t.t.t))
    , let tadin = repeat 3 (ta.din.__)
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

-- TODO these are awkward because they don't work in a group,
-- see TODO '- I need a way to transform inside a group'
group3 :: Sequence -> Sequence
group3 (a:b:cs) = [a] . [b] . __ . group3 cs
group3 xs = xs

group4 :: Sequence -> Sequence
group4 (a:b:cs) = [a] . [b] . __ . __ . group4 cs
group4 xs = xs
