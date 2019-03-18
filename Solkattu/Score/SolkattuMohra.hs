-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
-- | Mohras.
module Solkattu.Score.SolkattuMohra where
import           Prelude hiding ((.), (^), repeat)

import qualified Solkattu.Instrument.KendangTunggal as KendangTunggal
import qualified Solkattu.Korvai as Korvai
import qualified Solkattu.Tala as Tala

import           Solkattu.Dsl.Solkattu


makeMohras :: Tala.Tala -> Korvai.StrokeMaps -> (Sequence -> Sequence)
    -> [((Sequence, Sequence, Sequence), (Sequence, Sequence, Sequence))]
    -> Korvai
makeMohras tala smaps transform =
    mohra • korvai tala smaps • map (section • make)
    where
    make ((a1_, a2_, a3_), (b1_, b2_, b3_)) =
          a123.b1 . a123.b1
        . a123.b2
        . a1.b2 . a3.b3
        where
        a123 = a1.a2.a3
        (a1, a2, a3) = (t a1_, t a2_, t a3_)
        (b1, b2, b3) = (t b1_, t b2_, t b3_)
        t = group • transform

makeMohra :: Tala.Tala -> Korvai.StrokeMaps -> (Sequence -> Sequence)
    -> (Sequence, Sequence, Sequence)
    -> (Sequence, Sequence, Sequence) -> Korvai
makeMohra tala smaps transform as bs =
    makeMohras tala smaps transform [(as, bs)]

-- | Alternate melkalam and kirkalam.
makeMohras2 :: Tala.Tala -> Korvai.StrokeMaps -> (Sequence -> Sequence)
    -> [((Sequence, Sequence, Sequence), (Sequence, Sequence, Sequence))]
    -> Korvai
makeMohras2 tala smaps transform =
    mohra • korvai tala smaps • map (section • make)
    where
    make ((a1_, a2_, a3_), (b1_, b2_, b3_)) =
        a123.b1 . su (a123.b1) . a123.b1 . su (a123.b1)
        . a123.b2 . su (a123.b2)
        . a1.b2 . su (a1.b2)
        . a3.b3 . su (a3.b3)
        where
        (a1, a2, a3) = (t a1_, t a2_, t a3_)
        (b1, b2, b3) = (t b1_, t b2_, t b3_)
        a123 = a1.a2.a3
        t = group • transform

-- | Alternate melkalam and kirkalam.
makeMohra2 :: Tala.Tala -> Korvai.StrokeMaps -> (Sequence -> Sequence)
    -> (Sequence, Sequence, Sequence) -> (Sequence, Sequence, Sequence)
    -> Korvai
makeMohra2 tala smaps transform as bs =
    makeMohras2 tala smaps transform [(as, bs)]

c_mohra :: Korvai
c_mohra = ganesh $ makeMohra adi (mridangam<>kendang)
    su (a1, a2, a1) (b1, b2, b3)
    where
    a1 = dit.__4     .tang.__.kita.nakatiku
    a2 = na.ka.dit.__.tang.__.kita.nakatiku
    b1 = ta.langa.din.__.tat.__.din.__.tat.__.dheem.__4
    b2 = ta.langa.dheem.__4
    b3 = tri_ (dheem.__4) (ta.langa.din.__.tat.__)
    mridangam = makeMridangam
        [ (dit, k)
        , (tang.kita, u.p.k)
        , (na.ka, n.p)
        , (ta.langa, p.u.__.k)
        , (din.tat, o.k)
        , (dheem, od)
        ]
    kendang = makeKendang1
        [ (dit, pk)
        , (tang.kita, o.p.k)
        , (na.ka, t.k)
        , (ta.langa, u.u.__.p)
        , (din.tat, o.p)
        , (dheem, a)
        ] where KendangTunggal.Strokes {..} = KendangTunggal.notes

c_mohra2 :: Korvai
c_mohra2 = janahan $ makeMohra adi mridangam su (a1, a2, a3) (b1, b2, b3)
    where
    a_ = kitataka.nakatiku
    a1 = dit.__4.tang.__ . a_
    a2 = dit.__2.tang.__ . a_
    a3 = dit.tang . a_
    b1 = repeat 3 (ta.ga.ta.ga) . dhom.__4
    b2 = ta.ga.ta.ga . dhom.__4
    b3 = tri_ (dhom.__4) $ repeat 2 (ta.ga.ta.ga)
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
    makeMohra2 adi mridangam su (a1, a2, a3) (b1, b2, b3)
    where
    -- he says "tikutaka tarikita" instead of "nakatiku tarikita"
    a1 = __.dhom.ta.ka.ta .__.ki.ta . nakatiku
    a2 = ka.din.__.din.__. ta.ki.ta . nakatiku
    a3 = ka.dhom.ta.ka.ta .__.ki.ta . nakatiku
    b1 = taka . tang.__3.ga . tang.__3.ga . tang.__3.ga . tang.__
    b2 = taka . tang.__3.ga.tang.__
    b3 = taka . tri_ (tang.__.kitataka) (tang.__3.ga.din.__)
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

-- | Misra version of 'c_mohra'.
misra1 :: Korvai
misra1 = date 2019 4 14 $ ganesh $ makeMohra Tala.misra_chapu mridangam id
    (a1, a2, a1) (b1, b2, b3)
    where
    a1 = tam.__.taka.nakatiku
    a2 = na.ka.dit.__.tang.__.kita.nakatiku
    b1 = ta.langa.din.__.tat.__.din.__.tat.__.dheem.__4
    b2 = ta.langa.dheem.__4
    b3 = tri_ (dheem.__4) (ta.langa.din.__.tat.__)
    mridangam = makeMridangam
        [ (tam.taka, on.p.k)
        , (dit, k)
        , (tang.kita, u.p.k)
        , (na.ka, n.p)
        , (ta.langa, p.u.__.k)
        , (din.tat, o.k)
        , (dheem, od)
        ]
