-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
-- | Korvais expressed in "Derive.Solkattu.Dsl".
module Derive.Solkattu.Score.Solkattu2013 where
import Prelude hiding ((.), (^), repeat)

import qualified Util.Seq as Seq
import qualified Derive.Solkattu.Instrument.KendangTunggal as KendangTunggal
import qualified Derive.Solkattu.KendangTunggalStrokes as K
import qualified Derive.Solkattu.Korvai as Korvai
import Derive.Solkattu.SolkattuGlobal

import Global


-- * chatusra nadai

c_13_07_23 :: Korvai
c_13_07_23 = date 2013 7 23 $ ganesh $ korvai1 adi mridangam $
    trin tat_din__ (tri p5) (tri p6) (tri p7)
    where
    tat_din__ = tat.__.din.__3
    mridangam = make_mridangam [(tat_din__, [k, od])]

c_13_08_14 :: Korvai
c_13_08_14 = ganesh $ date 2013 8 14 $ korvai adi (mridangam <> kendang)
    [            theme 2 1 . p5
      . dropM 2 (theme 2 2) . p6 . p6
      . dropM 4 (theme 2 3) . tri p7 . tri p6 . tri p5

    , let ta_din__ = ta.__.din.__.__ in
                 theme 3 1 . p5 . ta_din__
      . dropM 3 (theme 3 2) . p6 . p6 . ta_din__
      . dropM 6 (theme 3 3) . trin ta_din__ (tri p7) (tri p6) (tri p5)

    ,            theme 4 1 . pat7 . dheem ! (u <+> K.u) . __4
      . dropM 4 (theme 4 2) . repeat 2 pat8 . dheem ! (u <+> K.u) . __4
      . dropM 8 (theme 4 3)
        . trin (dheem . __4) (tri pat9) (tri pat8) (tri pat7)
    ]
    where
    theme gap1 gap2 = ta . __n gap1 . dit . __n gap1 . ta.ka.din.na.din
        . tri (ta . __n gap2 . din . __n gap1)
    mridangam = make_mridangam
        [ (ta.dit, [k, t])
        , (dit, [k])
        , (ta.ka.din.na.din, [k, o, o, k, o])
        , (ta.din, [k, od])
        -- for pat7 -- pat9
        , (ta.ka, [k, p])
        ]
    kendang = make_kendang1
        [ (ta.dit, [p, t])
        , (dit, [t])
        , (ta.ka.din.na.din, [p, a, a, p, a])
        , (ta.din, [o, a])
        -- for pat7 -- pat9
        , (ta.ka, [p, k])
        ] where KendangTunggal.Strokes {..} = KendangTunggal.notes
    pat7 = ta.ka.p5
    pat8 = ta.ka.__.p5
    pat9 = ta.__.ka.__.p5

c_yt1 :: Korvai
c_yt1 = source "youtube" $ korvai1 adi mridangam $
    -- TODO should be tat dit takadanna din tat tam
    -- tat.__.dit.__.ta.ka.din.na.thom.__.tat.__.din.__4
    --       .dit.__.ta.ka.din.na.thom.__.tat.__.din.__4
    --              .ta.ka.din.na.thom.__.tat.__.din.__4
    --                    .din.na.thom.__.tat.__.din.__4
    --                           .thom.__.tat.__.din.__4
    --                                   .tat.__.din.__4 -- ta ka din
    --                                          .din.__4
    -- TODO ... `replace` (tat.__.din.__4) (ta.ka.din.__4)
    reduceTo 4 2 (tat.__.dit.__.takadinna.thom.__.tat.__.din.__4)
    . tri p6 . tam.__ . tri p6 . tam!i.__ . tri p6
    where
    mridangam = make_mridangam
        [ (tat.dit, [k, t])
        , (din.na, [o, k])
        , (thom.tat.din, [on, k, od])
        , (dit, [k])
        , (tat, [k])
        , (din, [od])
        ]

c_13_10_29 :: Korvai
c_13_10_29 = date 2013 10 29 $ ganesh $ korvai adi mridangam
    [ sequence
    , nadai 6 sequence
    ]
    where
    sequence =
        reduce3 2 mempty (tat.__.dit.__.ta.ka.din.na.dheem.__4)
        . tri_ (tam.__6) (p6.p6.p6)
    mridangam = make_mridangam
        [ (tat.dit, [k, t])
        , (dit, [k])
        ]

c_13_11_05 :: Korvai
c_13_11_05 = date 2013 11 5 $ ganesh $ korvai1 adi mridangam $
    tri_ (tam.__4) theme
    . theme . tam.__4 . theme . tam.__3 . su p6.tam.__3 . su p6
    where
    theme = su $ p5.p5.p6
    mridangam = make_mridangam []

c_13_11_12 :: Korvai
c_13_11_12 = date 2013 11 12 $ ganesh $ korvai adi mridangam
    [ sequence, nadai 6 sequence
    ]
    where
    sequence =
        theme . dropM 2 theme . ta.ka.dheem.__4 . dropM 4 theme
            . repeat 2 (ta.ka.dheem.__4)
            . spread 3 tdgnt . spread 2 tdgnt . tri_ __ tdgnt
    theme = tat.__.dit.__.ta.ka.din.na.ta.ka.dheem.__4
    mridangam = make_mridangam
        [ (tat.dit, [k, t])
        , (dit, [k])
        , (ta.ka, [p, k])
        ]

c_13_12_11 :: Korvai
c_13_12_11 = date 2013 12 11 $ ganesh $ korvai adi mridangam
    -- development for theme14
    [ sarvaD 8 . sarvaSam adi theme14
        . sarvaSam adi theme14, sarvaSam adi theme14
        . __a 4 theme14 . __a 4 theme14
        . __a 4 theme14 . repeat 2 (__.__.tat.__.ka.din.na.__)
    -- development for three together
    , mconcat [sarvaD 8 . sarvaSam adi t | t <- themes]
        . mconcat [sarvaSam adi t | t <- themes]
        . repeat 2 (__a 4 theme14 . __a 4 theme16)
    , structure theme14 (din.__6)
    , structure theme16 (din.__4)
    , structure theme18 (din.__2)
    , structure theme20 mempty
    ]
    where
    structure theme karvai =
                theme . karvai . p5
         . tk.  theme . karvai . p5.p5
         . tknk.theme . karvai . p5.p5.p5
    themes = [theme14, theme16, theme18]
    theme14 = ta.di.__.kita.kita.ta.tat.__.ka.din.tat.__
    theme16 = ta.di.__.kita.__.kita.ta.tat.__.ka.din.__.tat.__
    theme18 = ta.di.__.ki.__.ta.__.kita.ta.tat.__.ka.__.din.__.tat.__
    -- theme20 is on 2014 1 1
    theme20 = ta.di.__3.ki.__.ta.__.kita.ta.tat.__3.ka.__.din.__.tat.__
    mridangam = make_mridangam
        [ (theme14, [k, t, p, k, t, k, t, k, o, od, k])
        , (tat.ka.din.na, [k, o, od, k])
        , (din, [od])
        ]

c_nnnd :: Korvai
c_nnnd = korvai adi mridangam
    [ make (na.na.na.din.__)
    , make (su (dhom.ka.ta.ka.na.ka) . din.__)
    ]
    where
    make theme = tri_ (tam.__3) (tri theme . tri p5)
    mridangam = make_mridangam
        [ (na.na.na.din, [on, on, on, od])
        , (tam, [i])
        , (dhom.ka.ta.ka.na.ka, [o, n, p, k, n, o])
        , (din, [od])
        ]

-- * kanda nadai

make_k1 :: [Sequence] -> Korvai
make_k1 = ganesh • korvai adi k1_mridangam • map (nadai 5)

k1_1 :: Korvai
k1_1 = make_k1 [sequence p g | g <- gaps, p <- [p5, p6, p7]]
    where
    gaps = [thom.__.ta.__, thom.__3, thom.__, thom, mempty]
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
        end_gap = __n (5 - matrasOf gap)
        pdur = matrasOf pt

k1_2 :: Korvai
k1_2 = make_k1 $ (:[]) $
      sam . k1_a  . __ . ta_din_ . p7
          . k1_a' . __ . ta_din_ . p7
    . sam . k1_a . __ . k1_a' . __
          . ta_din_ . tri p7
          . ta_din_ . tri (tadin_ . p7)
          . ta_din_ . tri (tadin_ . tadin_ . p7)
    where
    p7 = tk.p5
    ta_din_ = ta.__.din.__
    tadin_ = ta.din.__

k1_3 :: Korvai
k1_3 = make_k1 $ (:[]) $
      k1_a  . __ . tata_dindin_ . p6 . __
    . k1_a' . __ . tata_dindin_ . tk.p6 . __
    . k1_a . __ . k1_a' . __ . tata_dindin_
    . tri_ __ (tknk.p6)
    . tri_ __ (tk.p6)
    . tri_ __ p6
    where
    tata_dindin_ = ta.__.ta.__3.din.__.din.__3

k1_a, k1_a' :: Sequence
k1_a  = ta.__.di.__.ki.ta.__.thom
k1_a' = ta.ka.di.__.ki.ta.__.thom

k1_mridangam :: Korvai.Instruments
k1_mridangam = make_mridangam
    [ (ta, [k])
    , (ta.di.ki.ta, [k, t, k, n])
    , (ta.ka.di.ki.ta, [k, p, t, k, n])
    , (din, [od])
    ]

k2 :: Bool -> Korvai
k2 chatusram_transition = korvai1 adi k1_mridangam $ nadai 5 $
      din.__3 . p5.tam.__4.p6.ta.__
    . din.__3 . p5.tam.__4.p6.ta.__.ta.__
    . din.__3 . p5
    . if chatusram_transition
        then nadai 4 (tri (ta.ta.__.p5))
        else tam.__4 . tri_ __5 p6
    -- p6 can also be k-t---ktkto-
    -- development is din_3.p5.ta.__.din

k3s :: Korvai
k3s = korvai adi mridangam $ map (nadai 5)
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
    mridangam = make_mridangam
        [ (dit, [p&k])
        , (ta.ki.ta, [p, k, od])
        , (ta.ka, [p, k])
        , (dit.tat, [p, k])
        , (kitataka, [p, k, n, p])
        , (tarikitataka, [u, p, k, t, p, k])
        , (din, [od])
        , (ta, [k])
        ]

-- * tisra nadai

t_sarva1 :: (Seq stroke, Seq stroke)
t_sarva1 =
    ( dhom.ka.na.na.di.mi . na.mi.na.na.di.mi
      . na.mi.na.na.di.mi . na.mi.na.na.di.mi
    , ta  .__.ta.ta.ta.__ . ta.__.ta.ta.ta.__
    . ta  .__.__.__.__.__ . __.__.ta.ta.ta.__
    )
    -- dhom is either [o] or [o, t]
    -- TODO I need a better way to write sarva laghu.  The problem is the thoms
    -- are implicit.

t1s :: Korvai
t1s = ganesh $ korvai adi mridangam $ map (nadai 6)
    [ reduce (tat.__.dit.__.takadinna.din.__.__)   . utarangam p5
    , reduce (tat.__.dit.__.takadinna.din.__)      . utarangam p6
    , reduce (tat.__.dit.__.takadinna.din!p)       . utarangam p7
    , reduce (tat.__.dit.__.takadinna)             . utarangam p8
    , reduce (tat.__.dit.__.ta.ka.din)             . utarangam p9
    ]
    where
    utarangam = tri_ (tang.__.ga)
    reduce = reduce3 2 mempty
    mridangam = make_mridangam
        [ (tat.dit, [k, t])
        , (dit, [k])
        , (takadinna, [k, o, o, k])
        , (ta.ka.din, [k, o, o])
        , (din, [od])
        , (tang.ga, [u, __])
        ]

t2s :: Korvai
t2s = ganesh $ korvai adi mridangam $ map (nadai 6)
    [ reduce (tat.__.dit.__.takadinna.dheem.__5) . tri p5
    , reduce (tat.__.dit.__.takadinna.dheem.__4) . tri p6
    , reduce (tat.__.dit.__.takadinna.dheem.__3) . tri p7
    , reduce (tat.__.dit.__.takadinna.dheem.__)  . tri (ta.din.__.p5)
    , reduce (tat.__.dit.__.takadinna.din!p)     . tri (ta.__.din.__.p5)
    , reduce (tat.__.dit.__.takadinna)           . tri (ta.ka.ta.din.__.p5)
    , reduce (tat.__.dit.__.ta.ka.din)           . tri (ta.ka.__.ta.din.__.p5)

    , reduce (tat.__.dit.__.ta.ka.din) . tri (ta.dinga.p7)
    , let tadin_ n = repeat n (ta.din.__) in
      reduce (tat.__.dit.__.ta.ka.din)
        . tadin_ 1 . p5 . tadin_ 2 . p5 . tadin_ 3 . p5

    , reduce (tat.__.dit.__.ta.ka.din) . tri (ta.din.__.p8)
    ]
    where
    reduce = reduce3 2 mempty
    mridangam = make_mridangam
        [ (tat.dit, [k, t])
        , (dit, [k])
        , (takadinna, [k, o, o, k])
        , (ta.ka.din, [k, o, o])
        , (tha, [p])
        , (ta.ka, [k, p])
        , (dheem, [od])
        , (din, [od])
        , (ta.din.ga, [k, od, __])
        , (ta.din, [k, od])
        ]

t3s :: Korvai
t3s = ganesh $ korvai adi mridangam $ map (nadai 6)
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
            (trin mempty (pat a1) (pat a2) (pat a3))
            (trin __ (pat b1) (pat b2) (pat b3))
            (trin __3 (pat c1) (pat c2) (pat c3))
    reduce = reduce3 2 mempty
    mridangam = make_mridangam
        [ (tat.dit, [k, t])
        , (dit, [k])
        , (takadinna, [k, o, o, k])
        , (ka.din.na, [o, o, k])
        , (din, [od])
        , (dinga, [od, __])
        , (tang.ga, [u, __])
        ]

t4s :: Korvai
t4s = ganesh $ korvai adi mridangam $ map (nadai 6 • (purvangam.))
    [ spread 3 tdgnt . spread 2 tdgnt . tri_ __ tdgnt
    , spread 3 tdgnt . tri (ta.__.din.__.gin.__.na.__.thom)
    , tri_ (dheem.__3) (ta.din.__.ta.__.din.__.p5)
    , tri_ (dheem.__3) (p5.ta.__.din.__.ta.din.__)
    , p123 p6 (dheem.__3)

    , p123 p5 (tat.__3.din.__3)
    , let dinga s = din!s . __ . ga
        in p5.dinga u . tk.p5.p5. dinga i . tknk.p5.p5.p5
    , tri (tat.dinga . tat.__.dinga.p5)
    ]
    where
    p123 p sep = trin sep p (p.p) (p.p.p)
    purvangam = tri (ta_katakita . din.__6)
    mridangam = make_mridangam $
        [ (ta.din.gin.na.thom, [k, t, k, n, o])
        , (ta.din, [k, od])
        , (dheem, [u])
        , (din, [od])
        , (tat, [k])
        , (dinga, [od, p])
        ] ++ m_ta_katakita

t4s2 :: Korvai
t4s2 = ganesh $ korvai adi mridangam $ map (nadai 6)
    [ purvangam1 . tri (ta.din.__.p5)
    , purvangam1 . ta.ka.p5 . ta.din.__.p5 . ta.ka.din.__.p5

    , purvangam2 6 . tri (ta.ka.ta.din.__.p5)
    , purvangam2 4 . tri (tat.__4.ta.din.__.p5)
    , purvangam2 2 . tri (ta.__.ga.din.__.ga.p8)
    ]
    where
    purvangam1 =
        ta_katakita . takita . tam.__6
        . ta_katakita . repeat 2 (takita) . tam.__6
        . ta_katakita . repeat 3 (takita) . tam.__6
    purvangam2 gap = tri (ta_katakita . takita.__2 . tam.__n gap)
    mridangam = make_mridangam $
        [ (takita, [on, p, k])
        , (tam, [od])
        , (ta.din, [k, od])
        , (ta.ka, [k, p])
        , (ta.ka.din, [k, o, od])
        , (tat, [k])
        , (ta.ga.din.ga, [k, p, t, p])
        ] ++ m_ta_katakita

t4s3 :: Korvai
t4s3 = ganesh $ korvai adi mridangam $ map (nadai 6)
    [ ta_katakita . tri (tat.__3.din.__3) . __7
        . ta_katakita . tri (tat.__.din.__3) . __7
        . ta_katakita . tri (tat.din.__3) . __7
    . trin (lang.__6)
        (tri (tat.__3.din.__.p5))
        (tri (tat.__.din.__.p5))
        (tri (tat.din.__.p5))
    ]
    where
    mridangam = make_mridangam $
        [ (tat.din, [k, od])
        ] ++ m_ta_katakita

ta_katakita :: Seq stroke
ta_katakita = ta.__.ka.ta.ki.ta.ta.ka.takadinna

m_ta_katakita :: MStrokes
m_ta_katakita =
    [ (ta.ka.takita.(ta.ka), [k, p, k, t, k, t, k])
    , (takadinna, [k, o, o, k])
    ]

t5s :: Korvai
t5s = ganesh $ korvai adi mridangam $ map (nadai 6 • (purvangam.))
    [ t123 p6 (tat.__6.din.__6)
    , t123 p7 (tat.__.ka.din.__.ta.din.__.ka)
    , t123 (ta.din.__2.p5) (tat.__3.din.__3)
    , t123 (tat.__.din.__.p5) (tang.__.ga)
    , let tadin = ta.din.__.ta.thom.__
        in trin (tang.__.ga) (tadin.p6) (tadin.p6.p6) (tadin.p6.p6.p6)
    , trin (tang.__)
        (ta.ka.ta.din.__ . p5)
        (ta.ka.__.ta.din.__ . p6.p6)
        (ta.__.ka.__.ta.din.__ . p7.p7.p7)
    , trin (tang.__4)
        (ta.__.ka.__.ta.din.__ . p7)
        (ta.ka.__.ta.din.__ . p6.p6)
        (ta.ka.ta.din.__ . p5.p5.p5)
    ]
    where
    purvangam = mconcat $ expand 3 3 (tat.__3.dit.__3.tat.__3.ta.ki.ta.thom.__3)
    t123 seq sep = trin sep seq (seq.seq) (seq.seq.seq)
    mridangam = make_mridangam
        [ (tat, [k])
        , (dit, [k])
        , (tat.dit, [k, t])
        , (ta.ki.ta.thom, [k, p, k, od])
        , (din, [od])
        , (tat.ka.din.ta.din.ka, [k, o, od, k, d, p])
        , (ta.din, [k, od])
        , (tang.ga, [u, p])
        , (ta.din.ta.thom, [k, p, k, od])

        , (ta.ka.ta, [k, p, k])
        ]

-- * koraippu

-- lead to misra:
-- tisram: tri (ta ka takitataka nakadit thom) spread 3 tdgnt ...
-- wait 2, repeat until tan7: od.__.p.k.n.o.o.k
--
-- wait 2, tri_ (thom.__0) (takadinna)
--      . repeat 2 (ta.__3.ta.takadinna)

misra_lead :: Korvai -- but add one akshara, so it lands on 1.
misra_lead = korvai1 adi mridangam $ su $
    rest 8 . tri_ (tam.__) takadinna
    . repeat 2 (ta.__3.ta.takadinna)
    . trin (tam.__3) (ta.din.na) (repeat 2 (ta.din.na)) (repeat 3 (ta.din.na))
    where
    mridangam = make_mridangam
       [ (takadinna, [k, o, o, k])
       , (ta.ta.takadinna, [od, k, n, o, o, k])
       , (ta.din.na, [on, on, k])
       , (tam, [od])
       ]

koraippu_misra :: Korvai
koraippu_misra = koraippu $ ganesh $ korvai adi mridangam $
    map su $ concat
    [ map long [1..7] -- 2 avartanam
    , map (mconcatMap short) [[1, 2], [3, 4], [5, 6], [7, 7]] -- 1 avartanam
    , group2 [half n . half (min 7 (n+1)) | n <- [1,3..7]] -- 1/2 avartanam
    , [ repeat 8 (__.p7) ]
    , [ __ . repeat 5 p7 . nadai 3 (tri p7) ]
    -- to mohra korvai sequence
    ]
    where
    group2 seq = map mconcat (Seq.chunked 2 seq)
    -- 8 + 8*7 (3+2 + 3)
    long n = rest 8 . tan7 . fill n . tan7 . fill n . tan7 . tri (fill n)
    -- 4 + 4*7 (1 + 3)
    short n = rest 4 . tan7 . tri (fill n)
    half n = rest 2 . tan7 . fill n
    fill n = fills !! (n-1) . karvai din
    fills = zipWith (\n p -> __n (n+1) . p) [6, 5..]
        [ ta
        , ta.ka
        , ta.din.na
        , ta.ka.din.na
        , p5
        , p6
        , p7
        ]
    tan7 = tang.__.ga.din.__.ga.din
    mridangam = make_mridangam
        [ (tang.ga, [on, k])
        , (din.ga, [od, k])
        , (din, [od])
        , (ta, [k])
        , (ta.ka, [p, k])
        , (ta.din.na, [on, on, k])
        , (ta.ka.din.na, [k, o, o, k])
        ]

-- * tirmanam

tir_18 :: Korvai
tir_18 = tirmanam $ korvai1 adi mridangam $
    __sam adi $ su $ reduce3 2 mempty (dhom.ka.dhom.ka.ta.lang.__.ga)
    where
    mridangam = make_mridangam
        [ (dhom.ka, [o, k])
        , (ta.lang.ga, [p, u, k])
        , (din, [od])
        ]

-- * util

rest :: Matra -> Seq stroke
rest dur = repeat dur __
