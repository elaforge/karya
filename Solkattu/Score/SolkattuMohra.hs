-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
-- | Mohras.
module Solkattu.Score.SolkattuMohra where
import           Prelude hiding ((.), (^), repeat)

import qualified Solkattu.Instrument.KendangTunggal as KendangTunggal
import qualified Solkattu.Korvai as Korvai
import qualified Solkattu.Score.Mohra as Mohra
import qualified Solkattu.Tala as Tala

import           Solkattu.Dsl.Solkattu hiding (korvai1)


-- | Bundle multiple mohra korvais.  This is convenient for variations.
korvais :: Tala.Tala -> Korvai.StrokeMaps -> (Sequence -> Sequence)
    -> [((Sequence, Sequence, Sequence), (Sequence, Sequence, Sequence))]
    -> Korvai
korvais tala smaps transform = Mohra.korvais (korvai tala smaps) transform

korvai1 :: Tala.Tala -> Korvai.StrokeMaps -> (Sequence -> Sequence)
    -> (Sequence, Sequence, Sequence)
    -> (Sequence, Sequence, Sequence) -> Korvai
korvai1 tala smaps transform as bs = korvais tala smaps transform [(as, bs)]

-- | Mohra with a matching korvai.
mohraKorvai :: Tala.Tala -> Korvai.StrokeMaps -> (Sequence -> Sequence)
    -> (Sequence, Sequence, Sequence)
    -> (Sequence, Sequence, Sequence)
    -> Sequence
    -> Korvai
mohraKorvai tala smaps transform as bs korvai_ =
    mohra $ korvai tala smaps
        [ withTypeS "mohra" $ s $ Mohra.make transform as bs
        , withTypeS "korvai" $ s korvai_
        ]

-- | Alternate melkalam and kirkalam.
makeMohras2 :: Tala.Tala -> Korvai.StrokeMaps -> (Sequence -> Sequence)
    -> [((Sequence, Sequence, Sequence), (Sequence, Sequence, Sequence))]
    -> Korvai
makeMohras2 tala smaps transform =
    mohra • korvai tala smaps • map (section • Mohra.make2 transform)

-- | Alternate melkalam and kirkalam.
makeMohra2 :: Tala.Tala -> Korvai.StrokeMaps -> (Sequence -> Sequence)
    -> (Sequence, Sequence, Sequence) -> (Sequence, Sequence, Sequence)
    -> Korvai
makeMohra2 tala smaps transform as bs =
    makeMohras2 tala smaps transform [(as, bs)]

c_mohra :: Korvai
c_mohra = ganesh $ korvais adi (mridangam<>kendang) su
    [ ((a1, a2, a1), (b1, b2, b3))
    , ((a1, a2, a1), (b12, b22, b32))
    ]
    where
    a1 = dit.__4     .tang.__.kita.nakatiku
    a2 = na.ka.dit.__.tang.__.kita.nakatiku
    b1 = ta.langa.din.__.tat.__.din.__.tat.__.dheem.__4
    b2 = ta.langa.dheem.__4
    b3 = tri_ (dheem.__4) (ta.langa.din.__.tat.__)

    b12 = nadai 3 $ na.kita.takita.takita.dheem.__3
    b22 = nadai 3 $ takita.dheem.__3
    b32 = nadai 3 $ tri_ tanga (na.kita.takita)
    mridangam = makeMridangam
        [ (dit, k)
        , (tang.kita, u.p.k)
        , (na.ka, n.p)
        , (ta.langa, p.u.__.k)
        , (din.tat, o.k)
        , (dheem, od)

        , (na.kita, p.k.n)
        , (takita, o.o.k)
        , (tanga, od.__.k)
        ]
    kendang = makeKendang1
        [ (dit, pk)
        , (tang.kita, o.p.k)
        , (na.ka, t.k)
        , (ta.langa, u.u.__.p)
        , (din.tat, o.p)
        , (dheem, a)

        , (na.kita, t.a.p)
        , (takita, o.o.p)
        , (tanga, a.__.p)
        ] where KendangTunggal.Strokes {..} = KendangTunggal.notes

c_mohra2 :: Korvai
c_mohra2 = janahan $ korvai1 adi mridangam su (a1, a2, a3) (b1, b2, b3)
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
misra1 = date 2019 4 14 $ ganesh $ korvai1 Tala.misra_chapu mridangam id
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
