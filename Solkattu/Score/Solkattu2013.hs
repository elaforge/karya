-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
-- | Korvais expressed in "Solkattu.Dsl".
module Solkattu.Score.Solkattu2013 where
import Prelude hiding ((.), (^), repeat)

import qualified Util.Seq as Seq
import qualified Solkattu.Instrument.KendangTunggal as KendangTunggal
import qualified Solkattu.Instrument.Mridangam as Mridangam
import qualified Solkattu.Korvai as Korvai
import Solkattu.SolkattuGlobal

import Global


-- * chatusra nadai

c_13_07_23 :: Korvai
c_13_07_23 = date 2013 7 23 $ ganesh $ korvaiS1 adi mridangam $
    trin tat_din__ (tri p5) (tri p6) (tri p7)
    where
    tat_din__ = tat.__.din.__3
    mridangam = makeMridangam [(tat_din__, k.od)]

c_13_08_14 :: Korvai
c_13_08_14 = ganesh $ date 2013 8 14 $ korvaiS adi (mridangam <> kendang)
    [     group (theme 2 1) . p5
      . dropM 2 (theme 2 2) . p6 . p6
      . dropM 4 (theme 2 3) . tri p7 . tri p6 . tri p5

    , let ta_din__ = ta.__.din.__.__ in
          group (theme 3 1) . p5 . ta_din__
      . dropM 3 (theme 3 2) . p6 . p6 . ta_din__
      . dropM 6 (theme 3 3) . trin ta_din__ (tri p7) (tri p6) (tri p5)

    ,     group (theme 4 1) . pat7 . tam.__4
      . dropM 4 (theme 4 2) . repeat 2 pat8 . tam.__4
      . dropM 8 (theme 4 3)
        . trin (dheem . __4) (tri pat9) (tri pat8) (tri pat7)
    ]
    where
    theme gap1 gap2 = ta . __n gap1 . dit . __n gap1 . takadinna.din
        . tri (ta . __n gap2 . din . __n gap1)
    mridangam = makeMridangam
        [ (ta.dit, k.t)
        , (takadinna.din, k.o.o.k.o)
        , (ta.din, k.od)
        -- for pat7 -- pat9
        , (taka, k.p)
        , (tam, u)
        , (dheem, i)
        ]
    kendang = makeKendang1
        [ (ta.dit, p.t)
        , (takadinna.din, p.a.o.p.a)
        , (ta.din, o.a)
        -- for pat7 -- pat9
        , (taka, p.k)
        , (tam, a)
        , (dheem, a)
        ] where KendangTunggal.Strokes {..} = KendangTunggal.notes
    pat7 = taka.p5
    pat8 = taka.__.p5
    pat9 = ta.__.ka.__.p5

c_yt1 :: Korvai
c_yt1 = source "youtube" $ korvaiS1 adi mridangam $
    -- TODO ... `replace` (tat.__.tam.__4) (taka.tam.__4)
    reduceTo 4 2 theme . tri_ (tam.__) (tri p6)
    where
    theme = tat.__.dit.__.takadinna.din.__.tat.__.tam.__4
    mridangam = makeMridangam
        [ (theme, k.t.k.o.o.k.on.k.od)
        , (tam, u)
        , (mid^tam, i)
        ]

c_13_10_29 :: Korvai
c_13_10_29 = date 2013 10 29 $ ganesh $ korvaiS adi mridangam
    [ sequence
    , nadai 6 sequence
    ]
    where
    sequence = reduce3 2 ø (tat.__.dit.__.takadinna.dheem.__4)
        . tri_ (tam.__6) (p6.p6.p6)
    mridangam = makeMridangam
        [ (tat.dit, k.t)
        , (dheem, od)
        , (tam, u)
        ]

c_13_11_05 :: Korvai
c_13_11_05 = date 2013 11 5 $ ganesh $ korvaiS1 adi mridangam $
    tri_ (tam.__4) theme
    . theme . tam.__4 . theme . tam.__3 . su p6.tam.__3 . su p6
    where
    theme = su $ p5.p5.p6
    mridangam = makeMridangam [(tam, u)]

c_13_11_12 :: Korvai
c_13_11_12 = date 2013 11 12 $ ganesh $ korvaiS adi mridangam
    [ sequence, nadai 6 sequence
    ]
    where
    sequence =
        theme . dropM 2 theme . taka.dheem.__4 . dropM 4 theme
            . repeat 2 (taka.dheem.__4)
            . spread 3 tdgnt . spread 2 tdgnt . tri_ __ tdgnt
    theme = tat.__.dit.__.takadinna.taka.dheem.__4
    mridangam = makeMridangam
        [ (tat.dit, k.t)
        , (taka, p.k)
        , (dheem, od)
        ]

c_13_12_11 :: Korvai
c_13_12_11 = date 2013 12 11 $ ganesh $ korvaiS adi mridangam
    -- development for theme14
    [ sarvaD_ 8 . sarvaSam adi theme14
        . sarvaSam adi theme14, sarvaSam adi theme14
        . __a 4 theme14 . __a 4 theme14
        . __a 4 theme14 . repeat 2 (__.__.tat.__.ka.din.na.__)
    -- development for three together
    , mconcat [sarvaD_ 8 . sarvaSam adi t | t <- themes]
        . mconcat [sarvaSam adi t | t <- themes]
        . repeat 2 (__a 4 theme14 . __a 4 theme16)
    , structure theme14 (din.__6)
    , structure theme16 (din.__4)
    , structure theme18 (din.__2)
    , structure theme20 ø
    ]
    where
    structure theme karvai =
                theme . karvai . p5
         . kp.  theme . karvai . p5.p5
         . kpnp.theme . karvai . p5.p5.p5
    themes = [theme14, theme16, theme18]
    theme14 = group $ ta.di.__.kita.kita.ta.tat.__.ka.din.tat.__
    theme16 = group $ ta.di.__.kita.__.kita.ta.tat.__.ka.din.__.tat.__
    theme18 = group $ ta.di.__.ki.__.ta.__.kita.ta.tat.__.ka.__.din.__.tat.__
    -- theme20 is on 2014 1 1
    theme20 = ta.di.__3.ki.__.ta.__.kita.ta.tat.__3.ka.__.din.__.tat.__
    mridangam = makeMridangam
        [ (theme14, k.t.p.k.t.k.t.k.o.od.k)
        , (tat.ka.din.na, k.o.od.k)
        , (din, od)
        ]

-- * kanda nadai

make_k1 :: [Sequence] -> Korvai
make_k1 = ganesh • korvaiS adi k1_mridangam • map (nadai 5)

k1_1 :: Korvai
k1_1 = make_k1 [sequence p g | g <- gaps, p <- [p5, p6, p7]]
    where
    gaps = [thom.__.ta.__, thom.__3, thom.__, thom, ø]
    sequence pt gap =
          sam . k1_a  . __ . ta . din . __n (10 - pdur) . pt
              . k1_a' . __ . ta . din . __n (10 - pdur) . pt
        . sam . ta . __ . di . __ . ki . ta . __ . gap
        .       ta . ka . di . __ . ki . ta . __ . gap
        . case pdur of
            5 -> p567 end_gap
            6 -> p666 end_gap
            _ -> p765 end_gap
        where
        end_gap = __n (5 - matrasOfI gap)
        pdur = matrasOfI pt

k1_2 :: Korvai
k1_2 = make_k1 $ (:[]) $
      sam . k1_a  . __ . ta_din_ . p7
          . k1_a' . __ . ta_din_ . p7
    . sam . k1_a . __ . k1_a' . __
          . ta_din_ . tri p7
          . ta_din_ . tri (tadin_ . p7)
          . ta_din_ . tri (tadin_ . tadin_ . p7)
    where
    p7 = kp.p5
    ta_din_ = ta.__.din.__
    tadin_ = ta.din.__

k1_3 :: Korvai
k1_3 = make_k1 $ (:[]) $
      k1_a  . __ . tata_dindin_ . p6 . __
    . k1_a' . __ . tata_dindin_ . kp.p6 . __
    . k1_a . __ . k1_a' . __ . tata_dindin_
    . tri_ __ (kpnp.p6)
    . tri_ __ (kp.p6)
    . tri_ __ p6
    where
    tata_dindin_ = ta.__.ta.__3.din.__.din.__3

k1_a, k1_a' :: Sequence
k1_a  = ta.__.di.__.ki.ta.__.thom
k1_a' = ta.ka.di.__.ki.ta.__.thom

k1_mridangam :: Korvai.StrokeMaps
k1_mridangam = makeMridangam
    [ (ta, k)
    , (ta.di.ki.ta, k.t.k.n)
    , (ta.ka.di.ki.ta, k.p.t.k.n)
    , (din, od)
    ]

k2 :: Bool -> Korvai
k2 chatusram_transition = korvaiS1 adi k1_mridangam $ nadai 5 $
      din.__3 . p5.tam.__4.p6.ta.__
    . din.__3 . p5.tam.__4.p6.ta.__.ta.__
    . din.__3 . p5
    . if chatusram_transition
        then nadai 4 (tri (ta.ta.__.p5))
        else tam.__4 . tri_ __5 p6
    -- p6 can also be k-t---ktkto-
    -- development is din_3.p5.ta.__.din

k3s :: Korvai
k3s = korvaiS adi mridangam $ map (nadai 5)
    [   dit . __  . tangkita . dit   . tat . din§2 . __
      . dit . tat . tangkita . dit§4 . tat . din . __
      . ta . __ . dit . tat . din . __
      . ta§6.ka.dit.tat.din.__
      . ta.ki.ta.ta.ki§0.ta
      . p6.__.p6.p6.__.p6.p6.p6 -- utarangam

    ,   dit . __  . tangkita . din . __4
      . dit . tat . tangkita . din . __4
      . dit . __  . tangkita . din . __
      . dit . tat . tangkita . din . __
      . dit . __  . tangkita
      . dit . tat . tangkita
      . tri_ __ p6
    ]
    where
    tangkita = su (tang . __ . kitataka . tarikitataka)
    kitataka = ki.ta.tha.ka
    tarikitataka = ta.ri.kitataka
    mridangam = makeMridangam
        [ (dit, p&k)
        , (ta.ki.ta, p.k.od)
        , (taka, p.k)
        , (dit.tat, p.k)
        , (kitataka, p.k.n.p)
        , (tarikitataka, u.p.k.t.p.k)
        , (din, od)
        , (ta, k)
        ]

-- * tisra nadai

t_sarva1 :: (Sequence, Sequence)
t_sarva1 =
    ( dhom.ka.na.na.di.mi . na.mi.na.na.di.mi
      . na.mi.na.na.di.mi . na.mi.na.na.di.mi
    , ta  .__.ta.ta.ta.__ . ta.__.ta.ta.ta.__
    . ta  .__.__.__.__.__ . __.__.ta.ta.ta.__
    )
    -- dhom is either o or o.t
    -- TODO I need a better way to write sarva laghu.  The problem is the thoms
    -- are implicit.

t1s :: Korvai
t1s = ganesh $ korvaiS adi mridangam $ map (nadai 6)
    -- TODO if I replace takadinna.din with a reduceR, then I get nested groups
    [ reduce (tat.__.dit.__.takadinna.din.__.__)   . utarangam p5
    , reduce (tat.__.dit.__.takadinna.din.__)      . utarangam p6
    -- TODO takadinna.din!p
    , reduce (tat.__.dit.__.takadinna.din)         . utarangam p7
    , reduce (tat.__.dit.__.takadinna)             . utarangam p8
    , reduce (tat.__.dit.__.taka.din)              . utarangam p9
    ]
    where
    utarangam = tri_ (tang.__.ga)
    reduce = reduce3 2 ø
    mridangam = makeMridangam
        [ (tat.dit, k.t)
        , (taka.din, k.o.o)
        , (din, od)
        , (tang.ga, u.__)
        ]

t2s :: Korvai
t2s = ganesh $ korvaiS adi mridangam $ map (nadai 6)
    [ reduce (tat.__.dit.__.takadinna.dheem.__5) . tri p5
    , reduce (tat.__.dit.__.takadinna.dheem.__4) . tri p6
    , reduce (tat.__.dit.__.takadinna.dheem.__3) . tri p7
    , reduce (tat.__.dit.__.takadinna.dheem.__)  . tri (ta.din.__.p5)
    -- TODO takadinna.din!p
    , reduce (tat.__.dit.__.takadinna.din)       . tri (ta.__.din.__.p5)
    , reduce (tat.__.dit.__.takadinna)           . tri (taka.ta.din.__.p5)
    , reduce (tat.__.dit.__.taka.din)            . tri (taka.__.ta.din.__.p5)

    , reduce (tat.__.dit.__.taka.din) . tri (ta.dinga.p7)
    , let tadin_ n = repeat n (ta.din.__) in
      reduce (tat.__.dit.__.taka.din)
        . tadin_ 1 . p5 . tadin_ 2 . p5 . tadin_ 3 . p5

    , reduce (tat.__.dit.__.taka.din) . tri (ta.din.__.p8)
    ]
    where
    reduce = reduce3 2 ø
    mridangam = makeMridangam
        [ (tat.dit, k.t)
        , (taka.din, k.o.o)
        , (taka, k.p)
        , (dheem, od)
        , (din, od)
        , (ta.din.ga, k.od.__)
        , (ta.din, k.od)
        ]

t3s :: Korvai
t3s = ganesh $ korvaiS adi mridangam $ map (nadai 6)
    [ reduce (tat.__.dit.__.takadinna.__.ka.din.na.dinga) . utarangam p5
    , reduce (tat.__.dit.__.takadinna.__.dinga) . utarangam p6
    , reduce (tat.__.dit.__.takadinna.__) . utarangam p7

    , variation (5, 5, 5) (6, 6, 6) (7, 7, 7)
    , variation (7, 7, 7) (6, 6, 6) (5, 5, 5)
    , variation (5, 6, 7) (5, 6, 7) (5, 6, 7)
    , variation (7, 6, 5) (7, 6, 5) (7, 6, 5)
    , variation (5, 6, 7) (6, 6, 6) (7, 6, 5)
    ]
    -- tat.__.dit.__.takadinna.__.ka.din.na.dinga
    --       .dit.__.takadinna.__.ka.din.na.dinga
    --              .takadinna.__.ka.din.na.dinga
    -- tat.__.dit.__.takadinna.__.dinga
    --       .dit.__.takadinna.__.dinga
    --              .takadinna.__.dinga
    where
    utarangam p = trin (tang.__.ga) (tri p) (tri_ __ p) (tri_ __3 p)
    variation (a1, a2, a3) (b1, b2, b3) (c1, c2, c3) =
        reduce (tat.__.dit.__.takadinna.__.dinga)
        . trin (tang.__.ga)
            (trin ø (pat a1) (pat a2) (pat a3))
            (trin __ (pat b1) (pat b2) (pat b3))
            (trin __3 (pat c1) (pat c2) (pat c3))
    reduce = reduce3 2 ø
    mridangam = makeMridangam
        [ (tat.dit, k.t)
        , (ka.din.na, o.o.k)
        , (dinga, od.__)
        , (tang.ga, u.__)
        ]

t4s2 :: Korvai
t4s2 = ganesh $ korvaiS adi mridangam $ map (nadai 6)
    [ purvangam1 . tri (ta.din.__.p5)
    , purvangam1 . taka.p5 . ta.din.__.p5 . taka.din.__.p5

    , purvangam2 6 . tri (taka.ta.din.__.p5)
    , purvangam2 4 . tri (tat.__4.ta.din.__.p5)
    , purvangam2 2 . tri (ta.__.ga.din.__.ga.p8)
    ]
    where
    purvangam1 =
        ta_katakita.takadinna . takita . tam.__6
        . ta_katakita.takadinna . repeat 2 (takita) . tam.__6
        . ta_katakita.takadinna . repeat 3 (takita) . tam.__6
    purvangam2 gap = tri (ta_katakita.takadinna . takita.__2 . tam.__n gap)
    mridangam = makeMridangam $
        [ (takita, on.p.k)
        , (tam, od)
        , (ta.din, k.od)
        , (taka, k.p)
        , (taka.din, k.o.od)
        , (tat, k)
        , (ta.ga.din.ga, k.p.t.p)
        ] ++ m_ta_katakita

t4s3 :: Korvai
t4s3 = ganesh $ korvaiS adi mridangam $ map (nadai 6)
    [ ta_katakita.takadinna . tri (tat.__3.din.__3) . __7
        . ta_katakita.takadinna . tri (tat.__.din.__3) . __7
        . ta_katakita.takadinna . tri (tat.din.__3) . __7
    . trin (lang.__6)
        (tri (tat.__3.din.__.p5))
        (tri (tat.__.din.__.p5))
        (tri (tat.din.__.p5))
    ]
    where
    mridangam = makeMridangam $
        [ (tat.din, k.od)
        ] ++ m_ta_katakita

ta_katakita :: Sequence
ta_katakita = ta.__.ka.ta.kitataka

m_ta_katakita :: StrokeMap Mridangam.Stroke
m_ta_katakita =
    [ (ta_katakita, k.p.k.t.k.t.k)
    ]

t5s :: Korvai
t5s = ganesh $ korvaiS adi mridangam $ map (nadai 6 • (purvangam.))
    [ tri123 (tat.__6.din.__6) p6
    , tri123 (tat.__.ka.din.__.ta.din.__.ka) p7
    , tri123 (tat.__3.din.__3) (ta.din.__2.p5)
    , tri123 (tang.__.ga) (tat.__.din.__.p5)
    , let tadin = ta.din.__.ta.thom.__
        in trin (tang.__.ga) (tadin.p6) (tadin.p6.p6) (tadin.p6.p6.p6)
    , trin (tang.__)
        (taka.ta.din.__ . p5)
        (taka.__.ta.din.__ . p6.p6)
        (ta.__.ka.__.ta.din.__ . p7.p7.p7)
    , trin (tang.__4)
        (ta.__.ka.__.ta.din.__ . p7)
        (taka.__.ta.din.__ . p6.p6)
        (taka.ta.din.__ . p5.p5.p5)
    ]
    where
    purvangam = mconcat $ expand 3 3 (tat.__3.dit.__3.tat.__3.ta.ki.ta.thom.__3)
    mridangam = makeMridangam
        [ (tat, k)
        , (tat.dit, k.t)
        , (ta.ki.ta.thom, k.p.k.od)
        , (tat.ka.din.ta.din.ka, k.o.od.k.d.p)
        , (ta.din, k.od)
        , (tang.ga, u.p)
        , (ta.din.ta.thom, k.p.k.od)

        , (taka.ta, k.p.k)
        , (din, od)
        ]

-- * koraippu

-- lead to misra:
-- tisram: tri (ta ka takitataka nakadit thom) spread 3 tdgnt ...
-- wait 2, repeat until tan7: od.__.p.k.n.o.o.k
--
-- wait 2, tri_ (thom.__0) (takadinna)
--      . repeat 2 (ta.__3.ta.takadinna)

misra_lead :: Korvai -- but add one akshara, so it lands on 1.
misra_lead = korvaiS1 adi mridangam $ su $
    __M 8 . tri_ (tam.__) takadinna
    . repeat 2 (ta.__3.ta.takadinna)
    . trin (tam.__3) (ta.din.na) (repeat 2 (ta.din.na)) (repeat 3 (ta.din.na))
    where
    mridangam = makeMridangam
       [ (ta.ta.takadinna, od.k.n.o.o.k)
       , (ta.din.na, on.on.k)
       , (tam, od)
       ]

koraippu_misra_no_karvai :: Korvai
koraippu_misra_no_karvai = koraippu $ ganesh $ korvaiS adi mridangam $ map su
    [ mconcatMap long [1..7] -- 2 avartanam
    , mconcatMap (mconcatMap short) [[1, 2], [3, 4], [5, 6], [7, 7]] -- 1 avart
    , group2 [half n . half (min 7 (n+1)) | n <- [1,3..7]] -- 1/2 avartanam
    , repeat 8 (__.p7)
    , __ . repeat 5 p7 . nadai 3 (tri p7)
    -- to mohra korvai sequence
    ]
    where
    group2 seq = concatMap mconcat (Seq.chunked 2 seq)
    -- 8 + 8*7 (3+2 + 3)
    long n = din.__8 . tri_ (gap n . fill n) tan7
        . gap n . tri_ (karvai n) (fill n)
    -- 4 + 4*7 (1 + 3)
    short n = din.__4 . tan7 . gap n . tri_ (karvai n) (fill n)
    half n = din.__2 . tan7 . gap n . fill n

    gap n = __n (7-n+1)
    fill n = (!! (n-1))
        [ ta
        , taka
        , ta.din.na
        , takadinna
        , p5
        , p6
        , p7
        ]
    karvai n_
        | n > 1 = din . __n n
        | otherwise = __n (n+1)
        where n = 7 - n_

    tan7 = tang.__.ga.din.__.ga.din
    mridangam = makeMridangam
        [ (tang.ga, on.k)
        , (din.ga, od.k)
        , (din, od)
        , (ta, k)
        , (taka, p.k)
        , (ta.din.na, on.on.k)
        ]

koraippu_misra :: Korvai
koraippu_misra = koraippu $ ganesh $ korvaiS adi mridangam $ map su
    [ mconcatMap long [1..7] -- 2 avartanam
    , mconcatMap (mconcatMap short) [[1, 2], [3, 4], [5, 6], [7, 7]] -- 1 avart
    , group2 [half n . half (min 7 (n+1)) | n <- [1,3..7]] -- 1/2 avartanam
    , repeat 8 (__.p7)
    , __ . repeat 5 p7 . nadai 3 (tri p7)
    -- to mohra korvai sequence
    ]
    where
    -- 8 + 8*7 (3+2 + 3)
    long n = __M 8 . tri_ (fill n) tan7 . tri (fill n)
    -- 4 + 4*7 (1 + 3)
    short n = __M 4 . tan7 . tri (fill n)
    half n = __M 2 . tan7 . fill n
    group2 seq = mconcatMap mconcat (Seq.chunked 2 seq)
    fill n = fills !! (n-1) . karvai din
    fills = zipWith (\n p -> __n (n+1) . p) [6, 5..]
        [ ta
        , taka
        , ta.din.na
        , takadinna
        , p5
        , p6
        , p7
        ]
    tan7 = tang.__.ga.din.__.ga.din
    mridangam = makeMridangam
        [ (tang.ga, on.k)
        , (din.ga, od.k)
        , (din, od)
        , (ta, k)
        , (taka, p.k)
        , (ta.din.na, on.on.k)
        ]

-- * tirmanam

tir_18 :: Korvai
tir_18 = tirmanam $ korvaiS1 adi mridangam $
    __sam adi $ su $ reduce3 2 ø (dhom.ka.dhom.ka.ta.lang.__.ga)
    where
    mridangam = makeMridangam
        [ (dhom.ka, o.k)
        , (ta.lang.ga, p.u.k)
        ]
