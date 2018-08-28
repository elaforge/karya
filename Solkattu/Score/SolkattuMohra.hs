-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
-- | Mohras.
module Solkattu.Score.SolkattuMohra where
import Prelude hiding ((.), (^), repeat)

import qualified Solkattu.Korvai as Korvai
import Solkattu.SolkattuGlobal
import qualified Solkattu.Tala as Tala


makeMohras :: Tala.Tala -> Korvai.StrokeMaps
    -> [((Sequence, Sequence, Sequence), (Sequence, Sequence, Sequence))]
    -> Korvai
makeMohras tala smaps = mohra • korvai tala smaps • map (section • make)
    where
    make ((a1_, a2_, a3_), (b1_, b2_, b3_)) =
          a123.b1 . a123.b1
        . a123.b2
        . a1.b2 . a3.b3
        where
        a123 = a1.a2.a3
        (a1, a2, a3) = (group a1_, group a2_, group a3_)
        (b1, b2, b3) = (group b1_, group b2_, group b3_)

makeMohra :: Tala.Tala -> Korvai.StrokeMaps -> (Sequence, Sequence, Sequence)
    -> (Sequence, Sequence, Sequence) -> Korvai
makeMohra tala smaps as bs = makeMohras tala smaps [(as, bs)]

-- | Alternate melkalam and kirkalam.
makeMohras2 :: Tala.Tala -> Korvai.StrokeMaps
    -> [((Sequence, Sequence, Sequence), (Sequence, Sequence, Sequence))]
    -> Korvai
makeMohras2 tala smaps = mohra • korvai tala smaps • map (section • make)
    where
    make ((a1_, a2_, a3_), (b1_, b2_, b3_)) =
        a123.b1 . su (a123.b1) . a123.b1 . su (a123.b1)
        . a123.b2 . su (a123.b2)
        . a1.b2 . su (a1.b2)
        . a3.b3 . su (a3.b3)
        where
        (a1, a2, a3) = (group a1_, group a2_, group a3_)
        (b1, b2, b3) = (group b1_, group b2_, group b3_)
        a123 = a1.a2.a3

-- | Alternate melkalam and kirkalam.
makeMohra2 :: Tala.Tala -> Korvai.StrokeMaps -> (Sequence, Sequence, Sequence)
    -> (Sequence, Sequence, Sequence) -> Korvai
makeMohra2 tala smaps as bs = makeMohras2 tala smaps [(as, bs)]

c_mohra :: Korvai
c_mohra = ganesh $ makeMohra adi mridangam (a1, a2, a1) (b1, b2, b3)
    where
    a1 = su $ dit.__4      .tang.__.kita.nakatiku
    a2 = su $ na.ka.dit.__2.tang.__.kita.nakatiku
    b1 = su $ ta.langa.din.__.tat.__.din.__.tat.__.dheem.__4
    b2 = su $ ta.langa.dheem.__4
    b3 = su $ tri_ (dheem.__4) (ta.langa.din.__.tat.__)
    mridangam = makeMridangam
        [ (dit, k)
        , (tang.kita, u.p.k)
        , (ta.langa, p.u.__.k)
        , (din.tat, o.k)
        , (dheem, od)
        , (na.ka, n.p)
        ]

c_mohra2 :: Korvai
c_mohra2 = janahan $ makeMohra adi mridangam (a1, a2, a3) (b1, b2, b3)
    where
    a_ = kitataka.nakatiku
    a1 = su $ dit.__4.tang.__ . a_
    a2 = su $ dit.__2.tang.__ . a_
    a3 = su $ dit.tang . a_
    b1 = su $ repeat 3 (ta.ga.ta.ga) . dhom.__4
    b2 = su $ ta.ga.ta.ga . dhom.__4
    b3 = su $ tri_ (dhom.__4) $ repeat 2 (ta.ga.ta.ga)
    mridangam = makeMridangam
        [ (dit, t)
        , (tang, o)
        , (kitataka, k.t.p.k)
        , (ta.ga, o.u)
        , (dhom, o)
        ]

c_mohra_youtube :: Korvai
c_mohra_youtube = source "Melakkaveri Balaji" $
    recording "https://www.youtube.com/watch?v=eq-DZeJi8Sk"
            (Just ((0, 0, 59), (0, 2, 7))) $
    makeMohra2 adi mridangam (a1, a2, a3) (b1, b2, b3)
    where
    -- he says "tikutaka tarikita" instead of "nakatiku tarikita"
    a1 =  su $ __.dhom.ta.ka.ta .__.ki.ta . nakatiku
    a2 =  su $ ka.din.__.din.__. ta.ki.ta . nakatiku
    a3 =  su $ ka.dhom.ta.ka.ta .__.ki.ta . nakatiku
    b1 = su $ taka . tang.__3.ga . tang.__3.ga . tang.__3.ga . tang.__
    b2 = su $ taka . tang.__3.ga.tang.__
    b3 = su $ taka . tri_ (tang.__.kitataka) (tang.__3.ga.din.__)
    mridangam = makeMridangam
        [ (dhom.ta.ka.ta, o.k.p.u)
        , (ki.ta, p.k)
        , (ka.din.din, p.i.i)
        , (ta.ki.ta, k.t.k)
        , (ka, k)
        , (taka.tang, n.o.od)
        , (ga.tang, o.od)
        , (ga.din.tang, o.od.u)
        , (kitataka, p.k.k.o)
        , (ga.din, o.od)
        ]
