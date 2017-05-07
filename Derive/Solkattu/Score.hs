-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
-- | Korvais expressed in "Derive.Solkattu.Dsl".
module Derive.Solkattu.Score where
import Prelude hiding ((.), (^), repeat)

import qualified Util.CallStack as CallStack
import qualified Util.Seq as Seq
import Derive.Solkattu.Dsl
import qualified Derive.Solkattu.KendangTunggal as KendangTunggal
import qualified Derive.Solkattu.KendangTunggalStrokes as K
import qualified Derive.Solkattu.Korvai as Korvai
import qualified Derive.Solkattu.Mridangam as Mridangam
import qualified Derive.Solkattu.Reyong as Reyong
import qualified Derive.Solkattu.Sequence as Sequence
import qualified Derive.Solkattu.Solkattu as Solkattu
import qualified Derive.Solkattu.Tala as Tala

import Global


type Seq = Sequence Korvai.Stroke -- TODO get rid of this?
type Sequence stroke = [Sequence.Note (Solkattu.Note stroke)]

Mridangam.Strokes {..} = Mridangam.notes

-- * adi talam

-- ** chatusra nadai

c1s :: [Korvai]
c1s = korvais adi (mridangam <> kendang)
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

c2_yt1 :: Korvai
c2_yt1 = korvai adi mridangam $
    -- tat.__.dit.__.ta.ka.din.na.thom.__.tat.__.din.__4
    --       .dit.__.ta.ka.din.na.thom.__.tat.__.din.__4
    --              .ta.ka.din.na.thom.__.tat.__.din.__4
    --                    .din.na.thom.__.tat.__.din.__4
    --                           .thom.__.tat.__.din.__4
    --                                   .tat.__.din.__4 -- ta ka din
    --                                          .din.__4
    -- TODO ... `replace` (tat.__.din.__4) (ta.ka.din.__4)
    reduceTo 2 4 (tat.__.dit.__.ta.ka.din.na.thom.__.tat.__.din.__4)
    . tri p6 . tam.__ . tri p6 . tam!i.__ . tri p6
    where
    mridangam = make_mridangam
        [ (tat.dit, [k, t])
        , (ta.ka.din.na, [k, o, o, k])
        , (din.na, [o, k])
        , (thom.tat.din, [o&n, k, od])
        , (dit, [k])
        , (tat, [k])
        , (din, [od])
        ]

-- | 3 avartanams of chatusram -> 2 avartanams of tisram.
c_13_10_29 :: Korvai
c_13_10_29 = korvai adi mridangam $
    reduce3 2 mempty (tat.__.dit.__.ta.ka.din.na.dheem.__4)
        . tri_ (tam.__6) (p6.p6.p6)
    where
    mridangam = make_mridangam $ standard_strokes ++
        [ (tat.dit, [k, t])
        , (dit, [k])
        , (tam, [u])
        ]

c_13_11_12 :: Korvai
c_13_11_12 = korvai adi mridangam $
    seq . dropM 2 seq . ta.ka.dheem.__4 . dropM 4 seq
        . repeat 2 (ta.ka.dheem.__4)
        . spread 3 tdgnt . spread 2 tdgnt . tri_ __ tdgnt
    where
    seq = tat.__.dit.__.ta.ka.din.na.ta.ka.dheem.__4
    mridangam = make_mridangam $ standard_strokes ++
        [ (tat.dit, [k, t])
        , (dit, [k])
        , (ta.ka, [p, k])
        ]

standard_strokes :: [(Sequence stroke, [Mridangam.SNote])]
standard_strokes =
    [ (ta.ka.din.na, [k, o, o, k])
    , (ta.din.gin.na.thom, [k, t, k, n, o])
    , (dheem, [od])
    ]

c_16_09_28 :: Korvai
c_16_09_28 = korvai adi mridangam $
    tat.__.dit.__.kitakina . nakatiku . tri_ __ (na.ka.ta.ka.din.na.dheem) . __6
          .dit.__.kitakina . nakatiku . tri_ __       (ta.ka.din.na.dheem) . __6
                 .kitakina . nakatiku . tri_ __              (ta.ka.dheem) . __6
    . tri (p6 . ta.ka.p6 . ta.ka.na.ka.p6)
    where
    kitakina = ki.ta.ki.na.ta.ki.ta.ka
    mridangam = make_mridangam $ standard_strokes ++
        [ (tat.dit, [k&p, t])
        , (dit, [k])
        , (kitakina, [k, t, k, n, o, k, t&o, k])
        , (na.ka, [n, p])
        , (ta.ka.dheem, [p, k, o&d])
        , (ta.ka, [k, p])
        , (ta.ka.na.ka, [k, p, n, p])
        ]

-- TODO: reduction pattern, forwards and backwards
-- c_14_01_14 -- 14_02_20

c_nnnd :: [Korvai]
c_nnnd = korvais adi mridangam
    [ make (na.na.na.din.__)
    , make (faster (dhom.ka.ta.ka.na.ka) . din.__)
    ]
    where
    make theme = tri_ (tam.__3) (tri theme . tri p5)
    mridangam = make_mridangam
        [ (na.na.na.din, [o&n, o&n, o&n, od])
        , (tam, [i])
        , (dhom.ka.ta.ka.na.ka, [o, n, p, k, n, o])
        , (din, [od])
        ]

c_17_02_06 :: Korvai
c_17_02_06 = korvai adi mridangam $
    tri_ (din.__.p6.p6) (ta.ki.ta.dinga.din.__.ta.__.ka.__)
    where
    mridangam = make_mridangam
        [ (ta.ki.ta.dinga.din, [k, p, k, od, k, od])
        , (ta.ka, [o&n, k])
        , (din, [od])
        ]

c_17_03_20 :: Korvai
c_17_03_20 = korvai adi (mridangam <> kendang <> reyong) $ faster $
    reduceTo 2 4 (tat.__.ta.ka.ta.ka.din.na.na.ka.dit.__.ta.lang.__.ga)
        -- . slower (slower td_gnt) . slower td_gnt . tri_ (__2.ga) td_gnt
        . slower (slower p6) . slower p6 . tri_ (__2.ga) p6
        -- . spread 4 td_gnt . spread 2 td_gnt
    where
    mridangam = make_mridangam $ standard_strokes ++
        [ (tat, [k])
        , (ta.ka, [k, t])
        , (na.ka.dit, [n, o, k])
        , (dit, [k])
        , (ta.lang.ga, [o, u, k])
        , (ga, [o]) -- TODO soft
        , (din.na, [o, k])
        ]
    kendang = make_kendang1
        [ (tat, [p])
        , (ta.ka, [p, k])
        , (na.ka.dit, [t, o, p])
        , (dit, [p])
        , (ta.lang.ga, [o, u, p])
        , (ga, [a])
        , (ta.ka.din.na, [p, a, o, p])
        , (din.na, [o, p])
        ] where KendangTunggal.Strokes {..} = KendangTunggal.notes
    reyong = make_reyong
        [ (tat, [b])
        , (ta.ka, [k, k])
        , (na.ka.dit, [i, r2, r3])
        , (dit, [r3])
        , (ta.lang.ga, [b, o, b])
        , (ga, [b])
        , (ta.ka.din.na, [x, k, k, k])
        , (din.na, [k, k])
        ] where Reyong.Strokes {..} = Reyong.notes

c_17_04_23 :: [Korvai]
c_17_04_23 = korvais adi mridangam $ map slower -- remove for melkalam
    [ purvangam . utarangam (ta.ka.tdgnt) (ta.ka.tdgnt)
    , purvangam . utarangam (faster (ta.__3.din.__3.gin.__3.na.__3.thom.__2)) p7
    , purvangam . faster (r32111 tdgnt . r32111 (ta.ka.tdgnt)
        . r32111 (ta.ka.na.ka.tdgnt))
    ]
    where
    r32111 ns = spread 3 ns . spread 2 ns . ns . ns . ns
    purvangam = tri_ (din.__3) (ta.__3.ta.ta.ka.din.na)
        -- dropM 5 is because ta.ta.ka.din.na.din is elided with the previous
        . dropM 5 (tri_ (din.__2) (ta.ta.ka.din.na))
    utarangam p7 p7' = mconcat
        [ slower p7 . p7 . faster end
        | end <- [p7', p7'.p7', p7'.p7'.p7']
        -- TODO some kind of x, xx, xxx function
        ]
    mridangam = make_mridangam $ standard_strokes ++
        [ (ta, [k])
        , (din, [od])
        , (ta.ka.tdgnt, [k, p, k, t, k, n, o])
        , (ta.ka.na.ka, [k, p, n, p])
        ]

chatusrams :: [Korvai]
chatusrams = concat
    [ c1s, [c2_yt1, c_13_10_29, c_16_09_28, c_17_02_06]
    , [c_17_03_20], c_17_04_23
    ]

-- ** kanda nadai

make_k1 :: Seq -> Korvai
make_k1 = korvai adi k1_mridangam • nadai 5

make_k1_1 :: Seq -> Seq -> Korvai
make_k1_1 pt gap = make_k1 $
      sam . k1_a  . __ . ta . din . __n (10 - pdur) . pt
          . k1_a' . __ . ta . din . __n (10 - pdur) . pt
    . sam . ta . __ . di . __ . ki . ta . __ . gap
    .       ta . ka . di . __ . ki . ta . __ . gap
    . case pdur of
        5 -> p567 end_gap
        6 -> p666 end_gap
        _ -> p765 end_gap
    where
    pdur = matrasOf pt
    end_gap = __n (5 - matrasOf gap)

k1_1s :: [Korvai]
k1_1s = [make_k1_1 p g | g <- gaps, p <- [pat 5, pat 6, pat 7]]
    where gaps = [thom.__.ta.__, thom.__3, thom.__, thom, mempty]

k1_2 :: Korvai
k1_2 = make_k1 $
      sam . k1_a  . __ . ta_din_ . p7
          . k1_a' . __ . ta_din_ . p7
    . sam . k1_a . __ . k1_a' . __
          . ta_din_ . tri p7
          . ta_din_ . tri (tadin_ . p7)
          . ta_din_ . tri (tadin_ . tadin_ . p7)
    where
    p7 = ta.ka.p5
    ta_din_ = ta.__.din.__
    tadin_ = ta.din.__

k1_3 :: Korvai
k1_3 = make_k1 $
      k1_a  . __ . tata_dindin_ . p6 . __
    . k1_a' . __ . tata_dindin_ . ta.ka.p6 . __
    . k1_a . __ . k1_a' . __ . tata_dindin_
    . tri_ __ (ta.ka.ti.ku.p6)
    . tri_ __ (ta.ka.p6)
    . tri_ __ p6
    where
    tata_dindin_ = ta.__.ta.__3.din.__.din.__3

k1_a, k1_a' :: Sequence stroke
k1_a  = ta.__.di.__.ki.ta.__.thom
k1_a' = ta.ka.di.__.ki.ta.__.thom

k1_mridangam :: Korvai.Instruments
k1_mridangam = make_mridangam
    [ (ta, [k])
    , (ta.ka, [k, p])
    , (ta.ka.ti.ku, [k, p, n, p])
    , (ta.di.ki.ta, [k, t, k, n])
    , (ta.ka.di.ki.ta, [k, p, t, k, n])
    , (din, [od])
    ]

k1_test = make_k1 $ nadai 5 (ta.__.di.__.ki.ta .__.thom.__.ta.din)

k2 :: Bool -> Korvai
k2 chatusram_transition = korvai adi k1_mridangam $ nadai 5 $
      din.__3 . p5.tam.__4.p6.ta.__
    . din.__3 . p5.tam.__4.p6.ta.__.ta.__
    . din.__3 . p5
    . if chatusram_transition
        then nadai 4 (tri (ta.ta.__.p5))
        else tam.__4 . tri_ __5 p6
    -- p6 can also be k-t---ktkto-
    -- development is din_3.p5.ta.__.din

k3s :: [Korvai]
k3s = korvais adi mridangam $ map (nadai 5)
    [   dit . __  . tangkita . dit   . tat . din^2 . __
      . dit . tat . tangkita . dit^4 . tat . din . __
      . ta . __ . dit . tat . din . __
      . ta^6.ka.dit.tat.din.__
      . ta.ki.ta.ta.ki^0.ta
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
    tangkita = faster (tang . __ . kitataka . tarikitataka)
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

kandams :: [Korvai]
kandams = concat
    [ k1_1s
    , [k1_2, k1_3]
    , [k2 False, k2 True]
    , k3s
    ]

-- ** tisra nadai

t_sarva1 :: (Sequence stroke, Sequence stroke)
t_sarva1 =
    ( dhom.ka.na.na.di.mi . na.mi.na.na.di.mi
      . na.mi.na.na.di.mi . na.mi.na.na.di.mi
    , ta  .__.ta.ta.ta.__ . ta.__.ta.ta.ta.__
    . ta  .__.__.__.__.__ . __.__.ta.ta.ta.__
    )
    -- dhom is either [o] or [o, t]
    -- TODO I need a better way to write sarva laghu.  The problem is the thoms
    -- are implicit.

t1s :: [Korvai]
t1s = korvais adi mridangam $ map (nadai 6)
    [ reduce (tat.__.dit.__.ta.ka.din.na.din.__.__)   . utarangam p5
    , reduce (tat.__.dit.__.ta.ka.din.na.din.__)      . utarangam p6
    , reduce (tat.__.dit.__.ta.ka.din.na.din!p)       . utarangam p7
    , reduce (tat.__.dit.__.ta.ka.din.na)             . utarangam p8
    , reduce (tat.__.dit.__.ta.ka.din)                . utarangam p9
    ]
    where
    utarangam = tri_ (tang.__.ga)
    reduce = reduce3 2 mempty
    mridangam = make_mridangam
        [ (tat.dit, [k, t])
        , (dit, [k])
        , (ta.ka.din.na, [k, o, o, k])
        , (ta.ka.din, [k, o, o])
        , (din, [od])
        , (tang.ga, [u, __])
        ]

t2s :: [Korvai]
t2s = korvais adi mridangam $ map (nadai 6)
    [ reduce (tat.__.dit.__.ta.ka.din.na.dheem.__5) . tri p5
    , reduce (tat.__.dit.__.ta.ka.din.na.dheem.__4) . tri p6
    , reduce (tat.__.dit.__.ta.ka.din.na.dheem.__3) . tri p7
    , reduce (tat.__.dit.__.ta.ka.din.na.dheem.__)  . tri (ta.din.__.p5)
    , reduce (tat.__.dit.__.ta.ka.din.na.din!p)     . tri (ta.__.din.__.p5)
    , reduce (tat.__.dit.__.ta.ka.din.na)           . tri (ta.ka.ta.din.__.p5)
    , reduce (tat.__.dit.__.ta.ka.din)             . tri (ta.ka.__.ta.din.__.p5)

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
        , (ta.ka.din.na, [k, o, o, k])
        , (ta.ka.din, [k, o, o])
        , (tha, [p])
        , (ta.ka, [k, p])
        , (dheem, [od])
        , (din, [od])
        , (ta.din.ga, [k, od, __])
        , (ta.din, [k, od])
        ]

t3s :: [Korvai]
t3s = korvais adi mridangam $ map (nadai 6)
    [ reduce (tat.__.dit.__.ta.ka.din.na.__.ka.din.na.dinga) . utarangam p5
    , reduce (tat.__.dit.__.ta.ka.din.na.__.dinga) . utarangam p6
    , reduce (tat.__.dit.__.ta.ka.din.na.__) . utarangam p7

    , variation (5, 5, 5) (6, 6, 6) (7, 7, 7)
    , variation (7, 7, 7) (6, 6, 6) (5, 5, 5)
    , variation (5, 6, 7) (5, 6, 7) (5, 6, 7)
    , variation (7, 6, 5) (7, 6, 5) (7, 6, 5)
    , variation (5, 6, 7) (6, 6, 6) (7, 6, 5)
    ]
    -- tat.__.dit.__.ta.ka.din.na.__.ka.din.na.dinga
    --       .dit.__.ta.ka.din.na.__.ka.din.na.dinga
    --              .ta.ka.din.na.__.ka.din.na.dinga
    -- tat.__.dit.__.ta.ka.din.na.__.dinga
    --       .dit.__.ta.ka.din.na.__.dinga
    --              .ta.ka.din.na.__.dinga
    where
    utarangam p = trin (tang.__.ga) (tri p) (tri_ __ p) (tri_ __3 p)
    variation (a1, a2, a3) (b1, b2, b3) (c1, c2, c3) =
        reduce (tat.__.dit.__.ta.ka.din.na.__.dinga)
        . trin (tang.__.ga)
            (trin mempty (pat a1) (pat a2) (pat a3))
            (trin __ (pat b1) (pat b2) (pat b3))
            (trin __3 (pat c1) (pat c2) (pat c3))
    reduce = reduce3 2 mempty
    mridangam = make_mridangam
        [ (tat.dit, [k, t])
        , (dit, [k])
        , (ta.ka.din.na, [k, o, o, k])
        , (ka.din.na, [o, o, k])
        , (din, [od])
        , (dinga, [od, __])
        , (tang.ga, [u, __])
        ]

t4s :: [Korvai]
t4s = korvais adi mridangam $ map (nadai 6 • (purvangam.))
    [ spread 3 tdgnt . spread 2 tdgnt . tri_ __ tdgnt
    , spread 3 tdgnt . tri (ta.__.din.__.gin.__.na.__.thom)
    , tri_ (dheem.__3) (ta.din.__.ta.__.din.__.p5)
    , tri_ (dheem.__3) (p5.ta.__.din.__.ta.din.__)
    , p123 p6 (dheem.__3)

    , p123 p5 (tat.__3.din.__3)
    , let dinga s = din!s . __ . ga
        in p5.dinga u . ta.ka . p5.p5. dinga i . ta.ka.ti.ku . p5.p5.p5
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
        , (ta.ka.ti.ku, [k, p, n, p])
        , (ta.ka, [k, p])
        , (dinga, [od, p])
        ] ++ m_ta_katakita

t4s2 :: [Korvai]
t4s2 = korvais adi mridangam $ map (nadai 6)
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
    takita = ta.ki.ta
    mridangam = make_mridangam $
        [ (ta.ki.ta, [o&n, p, k])
        , (tam, [od])
        , (ta.din, [k, od])
        , (ta.ka, [k, p])
        , (ta.ka.din, [k, o, od])
        , (tat, [k])
        , (ta.ga.din.ga, [k, p, t, p])
        ] ++ m_ta_katakita

t4s3 :: [Korvai]
t4s3 = korvais adi mridangam $ map (nadai 6)
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

ta_katakita :: Sequence stroke
ta_katakita = ta.__.ka.ta.ki.ta.ta.ka.ta.ka.din.na

m_ta_katakita :: MStrokes
m_ta_katakita =
    [ (ta.ka.(ta.ki.ta).(ta.ka), [k, p, k, t, k, t, k])
    , (ta.ka.din.na, [k, o, o, k])
    ]

t5s :: [Korvai]
t5s = korvais adi mridangam $ map (nadai 6 • (purvangam.))
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

tisrams :: [Korvai]
tisrams = concat
    [ t1s, t2s, t3s, t4s, t4s2, t4s3, t5s
    ]

-- * misra chapu

c_17_04_04 :: [Korvai]
c_17_04_04 = korvais Tala.misra_chapu mridangam $ map slower
    [   tat.__3 . din.__3 . tadimi
      . ta.ta.ka. din.__3 . tadimi
      . utarangam 3 (ta.ki.ta) (ta.ta.ka)
    , utarangam 4 (ta.ka.din.na) (ta.ka.din.na)
    , utarangam 5 tdgnt tdgnt
    , utarangam 6 td_gnt td_gnt
    ]
    where
    utarangam n p p2 =
        spread 4 p . spread 3 p . spread 2 p . tri_ (din.__n n) p2
    tadimi = ta.di.mi.ta.ta.ka.din.na
    mridangam = make_mridangam
        [ (tat.din, [k, od])
        , (ta.ta.ka.din, [o&n, o&n, k, od])
        , (tadimi, [o&n, od, k, p&d, n, o, od, k])

        , (din, [od])
        , (tdgnt, [k, t, k, n, o])
        , (ta.ki.ta, [p, k, od])
        , (ta.ta.ka, [o&n, o&n, k])
        , (ta.ka.din.na, [k, od, od, k])
        ]

-- * koraippu

-- lead to misra:
-- tisram: tri (ta ka takitataka nakadit thom) spread 3 tdgnt ...
-- wait 2, repeat until tan7: od.__.p.k.n.o.o.k
--
-- wait 2, tri_ (thom.__0) (ta.ka.din.na)
--      . repeat 2 (ta.__3.ta.ta.ka.din.na)

misra_lead :: Korvai -- but add one akshara, so it lands on 1.
misra_lead = korvai adi mridangam $ faster $
    rest 8 . tri_ (tam.__) (ta.ka.din.na)
     . repeat 2 (ta.__3.ta.ta.ka.din.na)
     . trin (tam.__3) (ta.din.na) (repeat 2 (ta.din.na)) (repeat 3 (ta.din.na))
     where
     mridangam = make_mridangam
        [ (ta.ka.din.na, [k, o, o, k])
        , (ta.ta.ta.ka.din.na, [od, k, n, o, o, k])
        , (ta.din.na, [o&n, o&n, k])
        , (tam, [od])
        ]

koraippu_misra :: [Korvai]
koraippu_misra = korvais adi mridangam $ map faster $ concat
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
    fill n = fills !! (n-1) . karv din
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
        [ (tang.ga, [o&n, k])
        , (din.ga, [od, k])
        , (din, [od])
        , (ta, [k])
        , (ta.ka, [p, k])
        , (ta.din.na, [o&n, o&n, k])
        , (ta.ka.din.na, [k, o, o, k])
        ]

koraippu_janahan :: Korvai
koraippu_janahan = korvai adi mridangam $ s2 $
    let seq = sequence (ta.ki.ta) (ta.ka.din.na)
    in mconcat
        [ seq 4 . ta.ka.ta.lang.__.ga.ta.ka.din.__.tat.__.thom.__4
        , seq 3 . tri p5 . thom.__4
        , seq 2 . tri p6 . thom.__4
        , seq 1 . tri p7 . thom.__4
        ]
    ++ let seq = sequence (s2 (nang.__.ki.ta.ta.ka)) (s2 nakatiku)
    in mconcat
        [ seq 4 . s2 nang_kita_nakatiku . ta.ka.din.__.tat.__.thom.__4
        , seq 3 . tri (s2 (thom.ki.ta.ka.na.ka.ki.ta.ta.ka)) . thom.__4
        , seq 2 . tri (s2 nang_kita_nakatiku) . thom.__4
        , seq 1 . tri (s2 (nang.__.ki.ta.ta.ka.nakatiku)) . thom.__4
        ]
    ++ let kitakita = s2 (ki.ta.ki.ta.ta.ka)
        in sam.tam.__3 . kitakita . tam.__3
            . kitakita . s2 (nakatiku . nang_kita_nakatiku) . tam.__3
            . kitakita . s2 (nakatiku . repeat 2 nang_kita_nakatiku . nakatiku)
            . s2 nang_kita_nakatiku
            . ta.ka.din.__.tat.__.thom.__4
    where
    -- problems:
    -- Realization varies based on context:
    -- . First takitas are ktk, rest are npk.  Likewise, first takadinnas are
    -- nook, but the last is kook.
    -- . Some soft p in tam.__3.
    -- . Maybe make the whole thing s2, but tam3 = s0 (tam.__3), where s0 sets
    -- absolute speed.
    -- . Variations, like ta.ka.ta.lang.__.ga, ga can be k or o.
    --   . Emphasize ktkno with pk t k n o
    sequence takita takadinna takitas =
        sam.tam.__3.takita.tam.__3.takita.takadinna.takita.takita.tam.__3
            . takita.takadinna
        . repeat takitas takita . takadinna
    mridangam = make_mridangam $ standard_strokes ++
        [ (tam, [od])
        , (ta.ki.ta, [n, p, k])
        , (ta.ka.ta.lang.ga, [p, k, p, u, k])
        , (ta.ka.din.tat, [p, k, o, k])
        , (thom, [od])
        , (nang.ki.ta.ta.ka, [n, k, t, p, k])
        , (nang.ki.ta, [o&n, p, k])

        , (thom.ki.ta.ka.na.ka.ki.ta.ta.ka, [o, k, t, p, u, p, k, t, p, k])
        , (ki.ta.ki.ta.ta.ka, [k, t, k, t, p, k])
        ]
    janahan_mridangam = make_mridangam
        [ (ta.ki.ta, [k, p, k])
        , (ta.ka.din.na, [k, o, o, k])
        , (ta.ki.ta . ta.ki.ta, [o, t, k, n, o, k])
        , (ta.ki.ta, [k, t, k])
        ]

koraippus :: [Korvai]
koraippus = concat [koraippu_misra, [koraippu_janahan]]

nang_kita_nakatiku :: Sequence a
nang_kita_nakatiku = nang . __ . ki.ta.nakatiku

-- * tirmanam

tir_18 :: [Korvai]
tir_18 = korvais adi mridangam $ map (faster • (pad 18 .))
    [ reduce3 2 mempty (dhom.ka.dhom.ka.ta.lang.__.ga)
    ]
    where
    mridangam = make_mridangam
        [ (dhom.ka, [o, k])
        , (ta.lang.ga, [p, u, k])
        , (din, [od])
        ]

-- * exercise

e_spacing :: [Korvai]
e_spacing = korvais adi mridangam $ map (align adi) $ map faster $ concat
    [ map arithmetic [p5, p6, p7, p8, p9]
    , map geometric [p5, p6, p7, p8, p9]
    ]
    where
    p5 = tdgnt
    p6 = td_gnt
    p7 = t_d_gnt
    p8 = ta.din.__.gin.__.na.__.thom
    p9 = ta.__.din.__.gin.__.na.__.thom
    arithmetic seq = spread 3 seq . spread 2 seq . tri seq
    geometric seq = spread 4 seq . spread 2 seq . tri seq
    mridangam = make_mridangam standard_strokes

-- * util

vary :: Korvai -> [Korvai]
vary = Korvai.vary $ Solkattu.vary (Solkattu.variations [Solkattu.standard])

rest :: Matra -> Sequence stroke
rest dur = repeat dur __

pad :: Matra -> Sequence stroke
pad dur = repeat (64 - dur) __

tdgnt, td_gnt, t_d_gnt :: Sequence stroke
tdgnt = ta.din.gin.na.thom
td_gnt = ta.din.__.gin.na.thom
t_d_gnt = ta.__.din.__.gin.na.thom

s2 :: [Sequence.Note stroke] -> [Sequence.Note stroke]
s2 = faster

s0 :: [Sequence.Note stroke] -> [Sequence.Note stroke]
s0 = slower

-- * realize

type MStrokes = [(Sequence Mridangam.Stroke, [Mridangam.SNote])]

make_mridangam :: CallStack.Stack => MStrokes -> Korvai.Instruments
make_mridangam strokes = mempty
    { Korvai.inst_mridangam =
        check $ Mridangam.instrument strokes Mridangam.default_patterns
    }

make_kendang1 :: CallStack.Stack =>
    [(Sequence KendangTunggal.Stroke, [KendangTunggal.SNote])]
    -> Korvai.Instruments
make_kendang1 strokes = mempty
    { Korvai.inst_kendang_tunggal = check $
        KendangTunggal.instrument strokes KendangTunggal.default_patterns
    }

make_reyong :: CallStack.Stack => [(Sequence Reyong.Stroke, [Reyong.SNote])]
    -> Korvai.Instruments
make_reyong strokes = mempty
    { Korvai.inst_reyong = check $
        Reyong.instrument strokes Reyong.rhythmic_patterns
    }

korvais :: CallStack.Stack => Tala.Tala -> Korvai.Instruments -> [Seq]
    -> [Korvai]
korvais tala realizations = map (korvai tala realizations)

korvai :: Tala.Tala -> Korvai.Instruments -> Seq -> Korvai.Korvai
korvai = Korvai.korvai

adi :: Tala.Tala
adi = Tala.adi_tala

realize, realizep :: Korvai.Korvai -> IO ()
realize = realize_ True
realizep = realize_ False

realize_ :: Bool -> Korvai.Korvai -> IO ()
realize_ = realize_instrument Korvai.mridangam

realize_k1 :: Bool -> Korvai.Korvai -> IO ()
realize_k1 = realize_instrument Korvai.kendang_tunggal

realize_r :: Bool -> Korvai.Korvai -> IO ()
realize_r = realize_instrument Korvai.reyong
