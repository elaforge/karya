-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
-- | Korvais expressed in "Derive.Solkattu.Dsl".
module Derive.Solkattu.Score.Score where
import Prelude hiding ((.), (^), repeat)

import qualified Util.CallStack as CallStack
import qualified Util.Seq as Seq
import Derive.Solkattu.Dsl
import Derive.Solkattu.DslSollu
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

c1s :: Korvai
c1s = ganesh $ korvai adi (mridangam <> kendang)
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
c2_yt1 = source "youtube" $ korvai1 adi mridangam $
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
    mridangam = make_mridangam $ standard_strokes ++
        [ (tat.dit, [k, t])
        , (din.na, [o, k])
        , (thom.tat.din, [o&n, k, od])
        , (dit, [k])
        , (tat, [k])
        , (din, [od])
        ]

-- | 3 avartanams of chatusram -> 2 avartanams of tisram.
c_13_10_29 :: Korvai
c_13_10_29 = date 2013 10 29 $ ganesh $ korvai1 adi mridangam $
    reduce3 2 mempty (tat.__.dit.__.ta.ka.din.na.dheem.__4)
        . tri_ (tam.__6) (p6.p6.p6)
    where
    mridangam = make_mridangam $ standard_strokes ++
        [ (tat.dit, [k, t])
        , (dit, [k])
        , (tam, [u])
        ]

c_13_11_12 :: Korvai
c_13_11_12 = date 2013 11 12 $ ganesh $ korvai1 adi mridangam $
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

c_16_09_28 :: Korvai
c_16_09_28 = date 2016 9 28 $ ganesh $ korvai1 adi mridangam $
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

c_16_12_06_sriram1 :: Korvai
c_16_12_06_sriram1 = date 2016 12 6 $ source "sriram" $ korvai adi mridangam $
    map su $ map (purvangam.)
    [ tri_ (dheem.__4) ta_takadin
    , tri_ (dheem.__4) (su $ p6 . ta.ka.p6 . ta.ka.na.ka.p6)
    -- All variations can get taka and takanaka in the karvai, and
    -- 345 345 345 can become 345 345 3333
    ]
    where
    purvangam =
        1^tat.__.1^dit.__ . su (kitakina.nakatiku . tri (dinnaka.takadinna))
            . din.din.__.tat.tat.__.thom.__
                .1^dit.__ . su (kitakina.nakatiku . tri dinnaka)
            . din.__.tat.__.thom.__
                          . su (kitakina.nakatiku . tri takadinna)
            . din.tat.thom.__

    dinnaka = din.na.ka.din.na.ka.ta.ka
    kitakina = ki.ta.ki.na.ta.ki.ta.ka

    ta_takadin = mconcat $ expand 3 1 (tat.dit.ta . su (ta.ka) . din)
    mridangam = make_mridangam $ standard_strokes ++
        [ (1^tat, [p&k])
        , (1^dit, [p&t])
        , (kitakina, [k, t, k, n, o, k, t&o, k])
        , (dinnaka, [o, k, t, o, hv k, t, o, k])
        , (din, [o])
        -- reduction
        , (tat, [k])
        , (dit, [t])
        , (dit.ta, [k, k])
        , (tat.dit, [k, t])
        , (ta, [k])
        , (ta.ka, [k, t])
        , (thom, [od])
        -- TODO taka, takanaka in that context should be kp kpnp
        , (ta.ka.na.ka, [k, p, n, p])
        ]

-- TODO: reduction pattern, forwards and backwards
-- c_14_01_14 -- 14_02_20

c_nnnd :: Korvai
c_nnnd = korvai adi mridangam
    [ make (na.na.na.din.__)
    , make (su (dhom.ka.ta.ka.na.ka) . din.__)
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
c_17_02_06 = date 2017 2 6 $ ganesh $ korvai1 adi mridangam $
    tri_ (din.__.p6.p6) (takita.dinga.din.__.ta.__.ka.__)
    where
    mridangam = make_mridangam
        [ (takita.dinga.din, [k, p, k, od, k, od])
        , (ta.ka, [o&n, k])
        , (din, [od])
        ]

c_17_03_20 :: Korvai
c_17_03_20 = date 2017 3 20 $ ganesh $
    korvai1 adi (mridangam <> kendang <> reyong) $ su $
        reduceTo 4 2 (tat.__.ta.ka.ta.ka.din.na.na.ka.dit.__.ta.lang.__.ga)
        . sd (sd p6) . sd p6 . tri_ (__2.ga) p6
    where
    mridangam = make_mridangam $ standard_strokes ++
        [ (tat, [k])
        , (ta.ka, [k, t])
        , (na.ka.dit, [n, o, k])
        , (dit, [k])
        , (ta.lang.ga, [o, u, k])
        , (ga, [lt o])
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

c_17_04_23 :: Korvai
c_17_04_23 = date 2017 4 23 $ ganesh $ korvai adi mridangam $
    map sd -- remove for melkalam
    [ purvangam . utarangam (ta.ka.tdgnt) (ta.ka.tdgnt)
    , purvangam . su (r32111 tdgnt . r32111 (ta.ka.tdgnt)
        . r32111 (ta.ka.na.ka.tdgnt))
    , purvangam . utarangam (su (ta.__3.din.__3.gin.__3.na.__3.thom.__2)) p7
    ]
    where
    r32111 ns = spread 3 ns . spread 2 ns . ns . ns . ns
    purvangam = tri_ (din.__3) (ta.__3.ta.ta.ka.din.na)
        -- dropM 5 is because ta.ta.ka.din.na.din is elided with the previous
        -- TODO an elide directive?
        . dropM 5 (tri_ (din.__2) (ta.ta.ka.din.na))
    utarangam p7 p7' = mconcat
        [ sd p7 . p7 . su end
        | end <- [p7', p7'.p7', p7'.p7'.p7']
        -- TODO some kind of x, xx, xxx function
        ]
    mridangam = make_mridangam $ standard_strokes ++
        [ (ta, [k])
        , (din, [od])
        , (ta.ka.tdgnt, [k, p, k, t, k, n, o])
        , (ta.ka.na.ka, [k, p, n, p])
        ]

c_17_05_10 :: Korvai
c_17_05_10 = date 2017 5 10 $ ganesh $ korvai1 adi mridangam $
    map (\n -> ta.__n n) [4, 3, 2, 1] `prefixes` (ta.__.ki.ta.takadinna.dinga)
        . ta.__.ki.ta.takadinna.dinga
    .  map (\n -> ta.__n n) [3, 2, 1] `prefixes` (takadinna.dinga)
        . reduceTo 3 1 (takadinna.dinga)
    . tri (spread 4 tdgnt . tdgnt)
    -- TODO an alternate way to this is a reduceTo that makes a list,
    -- then zipWith a replacePrefix, e.g.:
    -- reduceTo 3 1 (ta.__4 . ta.__.ki.ta.takadinna.dinga) `with`
    --     [ ø, ta.__3, ta.__2, ta.__, ta, ø
    --     , ø, ta.__3 , ta.__
    --     ]
    -- This is less code, but maybe not very obvious?

    -- ta.__4 . ta.__.ki.ta.takadinna.dinga
    -- ta.__3 . ta.__.ki.ta.takadinna.dinga
    -- ta.__2 . ta.__.ki.ta.takadinna.dinga
    -- ta.__  . ta.__.ki.ta.takadinna.dinga
    --     ta . ta.__.ki.ta.takadinna.dinga
    --          ta.__.ki.ta.takadinna.dinga
    --             ta.__.__.takadinna.dinga
    --                ta.__.takadinna.dinga
    --                   ta.takadinna.dinga
    where
    mridangam = make_mridangam $ standard_strokes ++
        [ (ta, [k])
        , (ki.ta, [t, k])
        , (dinga, [od, __])
        -- TODO these are only needed because realize doesn't understand
        -- reduction.
        , (ka.din.na, [o, o, k])
        , (din.na, [o, k])
        , (na, [k])
        ]

c_17_05_19_janahan :: Korvai
c_17_05_19_janahan = date 2017 5 15 $ source "janahan" $ korvai1 adi mridangam $
    1^tat_din_din_tam 4 3 . tat_din_din_tam 4 2 . tat_din_din_tam 3 2
        . repeat 2 (tat.__4.tam.__2.ta) . tat.__3
        . tri (takadinna.takita) -- TODO p7
    where
    tat_din_din_tam a b =
          tat.__4         .    din.__4.din.__n a . tam.__n b . ta
        . tat.__2.kum.__2 . 1^(din.__4.din.__n a . tam.__n b . ta)
        -- TODO with thom
    mridangam = make_mridangam
        [ (tat, [k])
        , (din, [d])
        , (tam, [n])
        , (1^tat, [o&k])
        , (1^din, [od])
        , (1^tam, [o&n])
        , (ta, [k])
        , (kum, [o])
        , (takadinna, [k, o, o&t, k])
        , (takita, [n, p, k])
        ]

chatusrams :: [Korvai]
chatusrams =
    [ c1s, c2_yt1, c_13_10_29, c_16_09_28, c_17_02_06
    , c_17_03_20, c_17_04_23
    ]

-- ** kanda nadai

make_k1 :: [Seq] -> Korvai
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
    p7 = ta.ka.p5
    ta_din_ = ta.__.din.__
    tadin_ = ta.din.__

k1_3 :: Korvai
k1_3 = make_k1 $ (:[]) $
      k1_a  . __ . tata_dindin_ . p6 . __
    . k1_a' . __ . tata_dindin_ . ta.ka.p6 . __
    . k1_a . __ . k1_a' . __ . tata_dindin_
    . tri_ __ (ta.ka.ti.ku.p6)
    . tri_ __ (ta.ka.p6)
    . tri_ __ p6
    where
    tata_dindin_ = ta.__.ta.__3.din.__.din.__3

k1_a, k1_a' :: Seq
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

kandams :: [Korvai]
kandams =
    [ k1_1
    , k1_2, k1_3
    , k2 False, k2 True
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

t1s :: Korvai
t1s = ganesh $ korvai adi mridangam $ map (nadai 6)
    [ reduce (tat.__.dit.__.takadinna.din.__.__)   . utarangam p5
    , reduce (tat.__.dit.__.takadinna.din.__)      . utarangam p6
    , reduce (tat.__.dit.__.takadinna.din!p)       . utarangam p7
    , reduce (tat.__.dit.__.takadinna)             . utarangam p8
    , reduce (tat.__.dit.__.ta.ka.din)                . utarangam p9
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
        [ (takita, [o&n, p, k])
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

ta_katakita :: Sequence stroke
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

tisrams :: [Korvai]
tisrams =
    [ t1s, t2s, t3s, t4s, t4s2, t4s3, t5s
    ]

-- ** misra nadai

m_sriram1 :: Korvai
m_sriram1 = source "sriram" $ date 2017 5 11 $ korvai1 adi mridangam $
    nadai 7 $
    circum (repeat 2 (takadinna.takita)) (accumulate
        [ din.__.ta.din.__.tat.__
        , takita.din.__.tat.__
        , thom.thom.ka.din.__.tat.__
        ]) (tam.__7)
    . tri (p5.tam.__ . p5.tam.__.tam.__ . p5)
    where
    mridangam = make_mridangam $ standard_strokes ++
        [ (takita, [n, p, k])
        , (din.ta.din.tat, [o&n, k, d, k])
        , (din.tat, [d, k])
        , (thom.thom.ka, [o, o, k])
        ]

-- * misra chapu

c_17_04_04 :: Korvai
c_17_04_04 = source "subash chandran" $ date 2017 4 4 $
    korvai Tala.misra_chapu mridangam $ map (sd • (purvangam.))
    [ utarangam 3 takita (ta.ta.ka)
    , utarangam 4 takadinna takadinna
    , utarangam 5 tdgnt tdgnt
    , utarangam 6 td_gnt td_gnt
    ]
    where
    purvangam = tat.__3 . din.__3 . tadimi
              . ta.ta.ka. din.__3 . tadimi
    utarangam n p p2 =
        spread 4 p . spread 3 p . spread 2 p . tri_ (din.__n n) p2
    tadimi = ta.di.mi.ta.takadinna
    mridangam = make_mridangam
        [ (tat.din, [k, od])
        , (ta.ta.ka.din, [o&n, o&n, k, od])
        , (tadimi, [o&n, od, k, p&d, n, o, od, k])

        , (din, [od])
        , (tdgnt, [k, t, k, n, o])
        , (takita, [p, k, od])
        , (ta.ta.ka, [o&n, o&n, k])
        , (takadinna, [k, od, od, k])
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
       , (ta.din.na, [o&n, o&n, k])
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
        [ (tang.ga, [o&n, k])
        , (din.ga, [od, k])
        , (din, [od])
        , (ta, [k])
        , (ta.ka, [p, k])
        , (ta.din.na, [o&n, o&n, k])
        , (ta.ka.din.na, [k, o, o, k])
        ]

koraippu_janahan :: Korvai
koraippu_janahan =
    koraippu $ source "janahan" $ korvai1 adi mridangam $ su $
    let seq = sequence takita takadinna
    in mconcat
        [ seq 4 . ta.ka.ta.lang.__.ga.ta.ka.din.__.tat.__.thom.__4
        , seq 3 . tri p5 . thom.__4
        , seq 2 . tri p6 . thom.__4
        , seq 1 . tri p7 . thom.__4
        ]
    ++ let seq = sequence (su (nang.__.ki.ta.ta.ka)) (su nakatiku)
    in mconcat
        [ seq 4 . su nang_kita_nakatiku . ta.ka.din.__.tat.__.thom.__4
        , seq 3 . tri (su (thom.ki.ta.ka.na.ka.ki.ta.ta.ka)) . thom.__4
        , seq 2 . tri (su nang_kita_nakatiku) . thom.__4
        , seq 1 . tri (su (nang.__.ki.ta.ta.ka.nakatiku)) . thom.__4
        ]
    ++ let kitakita = su (ki.ta.ki.ta.ta.ka)
        in sam.tam.__3 . kitakita . tam.__3
            . kitakita . su (nakatiku . nang_kita_nakatiku) . tam.__3
            . kitakita . su (nakatiku . repeat 2 nang_kita_nakatiku . nakatiku)
            . su nang_kita_nakatiku
            . ta.ka.din.__.tat.__.thom.__4
    where
    -- problems:
    -- . Some soft p in tam.__3.
    -- . Maybe make the whole thing s2, but tam3 = s0 (tam.__3), where s0 sets
    -- absolute speed.
    -- . Variations, like ta.ka.ta.lang.__.ga, ga can be k or o.
    --   . Emphasize ktkno with pk t k n o
    sequence takita takadinna takitas = sam
        .tam.__3 . 1^takita.tam.__3 . 1^takita.takadinna.takita.takita.tam.__3
            . 1^takita.takadinna
        . repeat takitas takita . takadinna
    mridangam = make_mridangam $ strokes ++
        [ (1^takita, [k, t, k])
        , (takita, [n, p, k])
        , (takadinna, [n, o, o, k])
        ]
    janahan_mridangam = make_mridangam $ strokes ++
        [ (1^takita, [k, p, k])
        , (takita . takita, [o, t, k, n, o, k])
        , (takita, [k, t, k])
        , (takadinna, [k, o, o, k])
        ]
    strokes =
        [ (tam, [od])
        , (ta.ka.ta.lang.ga, [p, k, p, u, k])
        , (ta.ka.din.tat, [p, k, o, k])
        , (thom, [od])
        , (nang.ki.ta.ta.ka, [n, k, t, p, k])
        , (nang.ki.ta, [o&n, p, k])
        , (thom.ki.ta.ka.na.ka.ki.ta.ta.ka, [o, k, t, p, u, p, k, t, p, k])
        , (ki.ta.ki.ta.ta.ka, [k, t, k, t, p, k])
        ]

koraippus :: [Korvai]
koraippus = [koraippu_misra, koraippu_janahan]

-- * tirmanam

tir_18 :: Korvai
tir_18 = tirmanam $ korvai adi mridangam $ map (su • (pad 18 .))
    [ reduce3 2 mempty (dhom.ka.dhom.ka.ta.lang.__.ga)
    ]
    where
    mridangam = make_mridangam
        [ (dhom.ka, [o, k])
        , (ta.lang.ga, [p, u, k])
        , (din, [od])
        ]

-- * exercise

e_spacing :: Korvai
e_spacing = exercise $ korvai adi mridangam $ map (align adi) $
    map su $ concat
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

e_17_05_19 :: Korvai
e_17_05_19 = exercise $ date 2017 5 15 $ korvai1 adi mridangam $
    tri (tri p8 . tri (ki.ta.thom)) . tri p8 . p5
    where
    mridangam = make_mridangam [(ki.ta.thom, [k, n, o])]

-- * fragments

nang_kita_nakatiku :: Sequence stroke
nang_kita_nakatiku = nang . __ . ki.ta.nakatiku

tdgnt, td_gnt, t_d_gnt :: Sequence stroke
tdgnt = ta.din.gin.na.thom
td_gnt = ta.din.__.gin.na.thom
t_d_gnt = ta.__.din.__.gin.na.thom

takadinna :: Sequence stroke
takadinna = ta.ka.din.na

takita :: Sequence stroke
takita = ta.ki.ta

-- * util

vary :: Korvai -> Korvai
vary = Korvai.vary $ Solkattu.vary (Solkattu.variations [Solkattu.standard])

rest :: Matra -> Sequence stroke
rest dur = repeat dur __

pad :: Matra -> Sequence stroke
pad dur = repeat (64 - dur) __

ganesh :: Korvai -> Korvai
ganesh = source "ganesh"

-- * realize

standard_strokes :: [(Sequence stroke, [Mridangam.SNote])]
standard_strokes =
    [ (ta.ka.din.na, [k, o, o, k])
    , (ta.din.gin.na.thom, [k, t, k, n, o])
    , (dheem, [od])
    ]

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

korvai1 :: CallStack.Stack => Tala.Tala -> Korvai.Instruments -> Seq -> Korvai
korvai1 tala inst seq = Korvai.korvai tala inst [seq]

korvai :: CallStack.Stack => Tala.Tala -> Korvai.Instruments -> [Seq] -> Korvai
korvai = Korvai.korvai

adi :: Tala.Tala
adi = Tala.adi_tala