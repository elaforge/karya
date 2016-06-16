-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Korvais expressed in SolkattuDsl.
module Derive.Solkattu.Score where
import Prelude hiding ((.), (^), repeat)
import qualified Data.List as List
import qualified Data.Text.IO as Text.IO

import qualified Util.Log as Log
import Derive.Solkattu.Dsl
import qualified Derive.Solkattu.Solkattu as Solkattu
import Global


-- * chatusra nadai

c1_2 :: Korvai
c1_2 = check $ Solkattu.korvai (adi 4) c1_mridangam $
      theme 0 . p5
    . dropM 2 (theme 1) . p6 . p6
    . dropM 4 (theme 2) . tri p7 . tri p6 . tri p5
    where
    theme gap = ta.__.dit.__.ta.ka.din.na.din
        . tri (ta . __n (gap+1) . din . __)

c1_3 :: Korvai
c1_3 = check $ Solkattu.korvai (adi 4) c1_mridangam $
      theme 0 . p5 . tadin
    . dropM 3 (theme 1) . p6 . p6 . tadin
    . dropM 6 (theme 2) . trin tadin (tri p7) (tri p6) (tri p5)
    where
    theme gap = ta.__3.dit.__3.ta.ka.din.na.din
        . tri (ta . __n (gap+1) . din . __3)
    tadin = ta.__.din.__.__

c1_4 :: Korvai
c1_4 = check $ Solkattu.korvai (adi 4) mridangam $
      theme 0 . pat7 . dheem!u . __4
    . dropM 4 (theme 1) . repeat 2 pat8 . dheem!u . __4
    . dropM 8 (theme 2)
        . trin (dheem!i . __4) (tri pat9) (tri pat8) (tri pat7)
    where
    theme gap = ta.__4.dit.__4.ta.ka.din.na.din
        . tri (ta . __n (gap+1) . din . __4)
    pat7 = ta.ka.p5
    pat8 = ta.ka.__.p5
    pat9 = ta.__.ka.__.p5
    mridangam = c1_mridangam ++
        [ (ta.ka, [k, p])
        ]

c1_mridangam :: [(Sequence, [MNote])]
c1_mridangam =
    [ (ta.dit, [k, t])
    , (dit, [k])
    , (ta.ka.din.na.din, [k, o, o, k, o])
    , (ta.din, [k, od])
    ]

chatusrams :: [Korvai]
chatusrams =
    [ c1_2, c1_3, c1_4
    ]

-- * kanda nadai

make_k1 :: Sequence -> Either Text Korvai
make_k1 = Solkattu.korvai (adi 5) k1_mridangam

make_k1_1 :: Sequence -> Sequence -> Korvai
make_k1_1 pt gap = check $ make_k1 $
      at0 . k1_a  . __ . ta . din . __n (10 - pdur) . pt
    . atX . k1_a' . __ . ta . din . __n (10 - pdur) . pt
    . at0 . ta . __ . di . __ . ki . ta . __ . gap
    .       ta . ka . di . __ . ki . ta . __ . gap
    . case pdur of
        5 -> p567 end_gap
        6 -> p666 end_gap
        _ -> p765 end_gap
    where
    pdur = duration_of pt
    end_gap = __n (5 - duration_of gap)

k1_1s :: [Korvai]
k1_1s = [make_k1_1 p g | g <- gaps, p <- [pat 5, pat 6, pat 7]]
    where gaps = [thom.__.ta.__, thom.__3, thom.__, thom, mempty]

k1_2 :: Korvai
k1_2 = check $ make_k1 $
      at0 . k1_a  . __ . ta_din_ . p7
    . atX . k1_a' . __ . ta_din_ . p7
    . at0 . k1_a . __ . k1_a' . __
          . ta_din_ . tri p7
          . ta_din_ . tri (tadin_ . p7)
          . ta_din_ . tri (tadin_ . tadin_ . p7)
    where
    p7 = ta.ka.p5
    ta_din_ = ta.__.din.__
    tadin_ = ta.din.__

k1_3 :: Korvai
k1_3 = check $ make_k1 $
      k1_a  . __ . tata_dindin_ . p6 . __
    . k1_a' . __ . tata_dindin_ . ta.ka.p6 . __
    . k1_a . __ . k1_a' . __ . tata_dindin_
    . tri_ __ (ta.ka.ti.ku.p6)
    . tri_ __ (ta.ka.p6)
    . tri_ __ p6
    where
    tata_dindin_ = ta.__.ta.__3.din.__.din.__3

k1_a, k1_a' :: Sequence
k1_a  = ta.__.di.__.ki.ta.__.thom
k1_a' = ta.ka.di.__.ki.ta.__.thom

k1_mridangam :: [(Sequence, [MNote])]
k1_mridangam =
    [ (ta, [k])
    , (ta.ka, [k, p])
    , (ta.ka.ti.ku, [k, p, n, p])
    , (ta.di.ki.ta, [k, t, k, n])
    , (ta.ka.di.ki.ta, [k, p, t, k, n])
    , (din, [od])
    ]

k2 :: Bool -> Korvai
k2 chatusram_transition = check $ Solkattu.korvai (adi 5) k1_mridangam $
      din.__3 . p5.tam.__4.p6.ta.__
    . din.__3 . p5.tam.__4.p6.ta.__.ta.__
    . din.__3 . p5
    . if chatusram_transition
        then nadai 4 . tri (ta.ta.__.p5)
        else tam.__4 . tri_ __5 p6
    -- p6 can also be k-t---ktkto-
    -- development is din_3.p5.ta.__.din

k3s :: [Korvai]
k3s = korvais (adi 5) mridangam
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
    tangkita = s2 (tang . kitataka . tarikitataka)
    kitataka = ki.ta.tha.ka
    tarikitataka = ta.ri.kitataka
    mridangam =
        [ (dit, [pk])
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

-- * tisra nadai

t_sarva1 :: (Sequence, Sequence)
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
t1s = korvais (adi 6) mridangam
    [ -- tat.__.dit.__.ta.ka.din.na.__.ka.din.na.dinga
      --       .dit.__.ta.ka.din.na.__.ka.din.na.dinga
      --              .ta.ka.din.na.__.ka.din.na.dinga
      reduce3 2 dinga (tat.__.dit.__.ta.ka.din.na.__.ka.din.na) . dinga
      . tri p5 . dinga!u . tri_ __ p5 . dinga!u . tri_ __2 p5
    , -- .tat.__.dit.__.ta.ka.din.na.__.dinga
      --        .dit.__.ta.ka.din.na.__.dinga
      --               .ta.ka.din.na.__.dinga
      reduce3 2 dinga (tat.__.dit.__.ta.ka.din.na.__) . dinga
      . tri p6 . dinga!u . tri_ __ p6 . dinga!u . tri_ __2 p6
    ]
    where
    mridangam =
        [ (tat.dit, [k, t])
        , (dit, [k])
        , (ta.ka.din.na, [k, o, o, k])
        , (ka.din.na, [o, o, k])
        , (din, [od])
        , (dinga, [od, ___])
        ]

tisrams :: [Korvai]
tisrams = concat
    [ -- t1s, t2s, t3s
    ]

-- * realize

korvais :: Log.Stack => Solkattu.Tala -> [(Sequence, [MNote])] -> [Sequence]
    -> [Korvai]
korvais tala mridangam sollus = zipWith korvai1 [0..] sollus
    where
    korvai1 i s = Solkattu.check_msg (show i) $ Solkattu.korvai tala mridangam s

adi :: Matras -> Solkattu.Tala
adi = Solkattu.adi_tala

realizes :: [Solkattu.Korvai] -> IO ()
realizes ks = sequence_ $ List.intersperse (putStrLn "\n----") (map realize ks)

realize :: Solkattu.Korvai -> IO ()
realize korvai = Text.IO.putStrLn $ case result of
    Left err -> "ERROR:\n" <> err
    Right notes -> Solkattu.pretty_strokes_tala tala notes
    where
    result = Solkattu.realize_korvai default_patterns korvai
    tala = Solkattu.korvai_tala korvai
