-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
-- | Solkattu scores from 2017.
module Derive.Solkattu.Score.Solkattu2017 where
import Prelude hiding ((.), (^), repeat)

import qualified Derive.Solkattu.Instrument.KendangTunggal as KendangTunggal
import qualified Derive.Solkattu.Instrument.Reyong as Reyong
import qualified Derive.Solkattu.Instrument.Sargam as Sargam
import qualified Derive.Solkattu.Solkattu as Solkattu
import Derive.Solkattu.SolkattuGlobal
import qualified Derive.Solkattu.Tala as Tala

import Global


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
    ++ let seq = sequence (su (nang.__.kita.ta.ka)) (su nakatiku)
    in mconcat
        [ seq 4 . su nang_kita_nakatiku . ta.ka.din.__.tat.__.thom.__4
        , seq 3 . tri (su (thom.kita.ka.na.ka.kita.ta.ka)) . thom.__4
        , seq 2 . tri (su nang_kita_nakatiku) . thom.__4
        , seq 1 . tri (su (nang.__.kita.ta.ka.nakatiku)) . thom.__4
        ]
    ++ let kitakita = su (kita.kita.ta.ka)
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
        , (nang.kita.ta.ka, [n, k, t, p, k])
        , (nang.kita, [o&n, p, k])
        , (thom.kita.ka.na.ka.kita.ta.ka, [o, k, t, p, u, p, k, t, p, k])
        , (kita.kita.ta.ka, [k, t, k, t, p, k])
        ]

nang_kita_nakatiku :: Seq stroke
nang_kita_nakatiku = nang.__.kita.nakatiku

e_spacing :: Korvai
e_spacing = exercise $ korvai adi (make_mridangam []) $ map (__sam adi) $
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
    mridangam = make_mridangam
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

c_17_04_04 :: Korvai
c_17_04_04 = date 2017 4 4 $ source "subash chandran" $
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

c_17_04_23 :: Korvai
c_17_04_23 = date 2017 4 23 $ ganesh $ korvai adi mridangam $
    map sd -- remove for melkalam
    [ purvangam . utarangam (tk.tdgnt) (tk.tdgnt)
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
    mridangam = make_mridangam
        [ (ta, [k])
        , (din, [od])
        -- TODO spread doesn't work with standard patterns, but it should
        , (ta.ka.tdgnt, [k, p, k, t, k, n, o])
        , (ta.ka.na.ka, [k, p, n, p])
        ]

c_17_05_10 :: Korvai
c_17_05_10 = date 2017 5 10 $ ganesh $ korvai1 adi insts $
    map (\n -> ta.__n n) [4, 3, 2, 1] `prefixes` (ta.__.kita.takadinna.dinga)
        . ta.__.kita.takadinna.dinga
    .  map (\n -> ta.__n n) [3, 2, 1] `prefixes` (takadinna.dinga)
        . reduceTo 3 1 (takadinna.dinga)
    . tri (spread 4 tdgnt . tdgnt)
    -- TODO an alternate way to this is a reduceTo that makes a list,
    -- then zipWith a replacePrefix, e.g.:
    -- reduceTo 3 1 (ta.__4 . ta.__.kita.takadinna.dinga) `with`
    --     [ ø, ta.__3, ta.__2, ta.__, ta, ø
    --     , ø, ta.__3 , ta.__
    --     ]
    -- This is less code, but maybe not very obvious?

    -- ta.__4 . ta.__.kita.takadinna.dinga
    -- ta.__3 . ta.__.kita.takadinna.dinga
    -- ta.__2 . ta.__.kita.takadinna.dinga
    -- ta.__  . ta.__.kita.takadinna.dinga
    --     ta . ta.__.kita.takadinna.dinga
    --          ta.__.kita.takadinna.dinga
    --             ta.__.__.takadinna.dinga
    --                ta.__.takadinna.dinga
    --                   ta.takadinna.dinga
    where
    insts = mridangam <> kendang <> sargam
    mridangam = make_mridangam
        [ (ta, [k])
        , (kita, [t, k])
        , (dinga, [od, __])
        -- TODO these are only needed because realize doesn't understand
        -- reduction.
        , (ka.din.na, [o, o, k])
        , (din.na, [o, k])
        , (na, [k])
        ]
    kendang = make_kendang1
        [ (ta, [p])
        , (kita, [p, k])
        , (takadinna, [t, o, o, p])
        , (dinga, [a, lt k])
        , (tdgnt, [p, k, t, a, o])
        -- TODO these are only needed because realize doesn't understand
        -- reduction.
        , (ka.din.na, [o, o, p])
        , (din.na, [o, p])
        , (na, [p])
        ] where KendangTunggal.Strokes {..} = KendangTunggal.notes
    sargam = make_sargam
        [ (ta, [s1])
        , (kita, [n, s1])
        , (takadinna, [d, n, p, d])
        , (dinga, [hv g, s])
        , (tdgnt, [hv r, g, hv p, m, n])
        -- TODO these are only needed because realize doesn't understand
        -- reduction.
        , (ka.din.na, [n, p, d])
        , (din.na, [p, d])
        , (na, [d])
        ]
        [
        ] where Sargam.Strokes {..} = Sargam.notes

m_17_05_11 :: Korvai
m_17_05_11 = date 2017 5 11 $ source "sriram" $
    korvai1 adi (mridangam<>sargam) $ nadai 7 $
    circum (repeat 2 (takadinna.takita)) (accumulate
        [ din.__.ta.din.__.tat.__
        , takita.din.__.tat.__
        , thom.thom.ka.din.__.tat.__
        ]) (tam.__7)
    . tri (p5.tam.__ . p5.tam.__.tam.__ . p5)
    where
    mridangam = make_mridangam
        [ (takita, [n, p, k])
        , (din.ta.din.tat, [o&n, k, d, k])
        , (din.tat, [d, k])
        , (thom.thom.ka, [o, o, k])
        ]
    sargam = make_sargam
        [ (takadinna, [hv p, m, r, m])
        , (takita, [hv s, r, d_])
        , (din.ta.din.tat, [s, n, s, d])
        , (din.tat, [r, n])
        , (thom.thom.ka, [hv m_, m_, s])
        , (tam, [hv s])
        ]
        [ ( Solkattu.PatternM 5, [hv n_, s, d_, n_, p_])
        ] where Sargam.Strokes {..} = Sargam.notes

e_17_05_19 :: Korvai
e_17_05_19 = date 2017 5 15 $ exercise $ korvai1 adi mridangam $
    tri (tri p8 . tri (kita.thom)) . tri p8 . p5
    where
    mridangam = make_mridangam [(kita.thom, [k, n, o])]

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

janahan_17_06_02 :: Korvai
janahan_17_06_02 = tirmanam $ date 2017 6 2 $ source "janahan" $
        korvai1 adi mridangam $
    __n 9 . tri_ (su (kita.ta.ka)) (din.din . su (ta.ka) . din.din.tat.din)
    -- TODO use align or pad to sam
    where
    mridangam = make_mridangam
        [ (din, [od])
        , (ta.ka, [p, k])
        , (tat, [k])
        , (kita.ta.ka, [p, k, n, o])
        ]

c_17_06_15 :: Korvai
c_17_06_15 = date 2017 6 15 $ ganesh $ korvai adi mridangam $
    [ mconcat [suffix sequence (thom.__n gap) | gap <- [4, 3, 2]]
    , join (1^ta) [suffix sequence (thom.__n gap) | gap <- [2, 3, 4]]
    ]
    where
    sequence = [takadinna, takita, ta.ka, ta]
    mridangam = make_mridangam
        [ (takita, [o, o, k])
        , (ta.ka, [o, k])
        , (ta, [k])
        , (1^ta, [p])
        , (thom, [od])
        ]

c_17_06_19 :: Korvai
c_17_06_19 = date 2017 6 19 $ ganesh $ korvai1 adi inst $
    reduce3 2 mempty (tat.__.dit.__.takadinna.din.__3.p5)
        -- TODO elide p5
        -- . trin (tam.__3) p5 (tk.p5) (tknk.p5)
        . tam.__3 . join (tam.__3) [tk.p5, tknk.p5]
    where
    inst = mridangam <> kendang <> sargam
    mridangam = make_mridangam
        [ (tat.dit, [k, t])
        , (din, [od])
        -- TODO reduction
        , (dit, [k])
        ]
    kendang = make_kendang1
        [ (tat.dit, [p, t])
        , (din, [a])
        -- TODO reduction
        , (dit, [p])
        -- TODO put in standard stroke map
        , (takadinna, [p, o, a, p])
        ] where KendangTunggal.Strokes {..} = KendangTunggal.notes
    sargam = make_sargam
        [ (tat.dit, [p, m])
        , (takadinna, [p, m, r, m])
        , (din, [hv s])
        , (dit, [m])
        , (tam, [hv s_])
        ]
        [ ( Solkattu.PatternM 5, su [d_, s, d_, n_] . [s, n_, p_])
        , ( Solkattu.Taka, [s, p_])
        , ( Solkattu.Takanaka, [s, p_, r, p_])
        ] where Sargam.Strokes {..} = Sargam.notes

c_17_06_19_koraippu :: Korvai
c_17_06_19_koraippu = date 2017 6 19 $ ganesh $ koraippu $
    korvai adi mridangam $ map (restD 2 .)
    [ repeat 2 $ tanga7 . __ . tat.__4.din.__4.din.__4 . tk.tdgnt
    , repeat 2 $ tri (tat.__4.din.__3) . tk.tdgnt
    , repeat 2 $ tri (tat.__4) . tri (din.__3) . tk.tdgnt
    , repeat 2 $ tri (tat.__3) . tri (din.__4) . tk.tdgnt

    -- 6 + 15
    , repeat 2 $ nadai 6 (tanga7.ga) . tat.__5.din.__5.din.__5 . tk.tdgnt

    -- 334353
    , repeat 2 $ tanga.dinga . ta.tanga.dinga . ta.ka.tanga.dinga . tk.tdgnt
    ]
    where
    tanga7 = tanga.dinga.din.__
    mridangam = make_mridangam
        [ (tanga, [on, k])
        , (dinga, [od, k])
        , (tat, [on])
        , (ta, [k])
        , (ta.ka, [p, k])
        , (din, [od])
        ]

-- trikalam
c_17_07_13 :: Korvai
c_17_07_13 = date 2017 7 13 $ ganesh $ korvai adi (mridangam<>kendang) $ concat
    -- TODO when I can do branches, any dintaka can substitute
    -- purvangam (any dintakas) . utarangam (any dintakas)
    [ map purvangam dintakas
    , map utarangam dintakas
    , [utarangam_gap]

    -- tisram
    , map (nadai 6)
        [ tri (purvangam basic_dintaka)
        , one_avartanam . utarangam basic_dintaka
        ]
    , map su
        [ tri (purvangam basic_dintaka)
        . one_avartanam . tri (utarangam basic_dintaka)
        ]
    ]
    where
    one_avartanam = ta.__4.taka.din.__ . tk.kita.taka.din.__
      . dhom.__.kita.taka.din.__ . dhom.dhom.kita.taka.din.__
    basic_dintaka = 1^(din.taka.din.taka)
    dintakas = -- each is 6 beats -- TODO map assert dur == 6
        [ basic_dintaka
        , din . su (1^taka).din.din.taka
        , din . su (1^taka).din.din . su (1^taka).din
        , n6 (din.1^taka).din.din.taka
        , n6 (din.1^taka.din.din.1^ta) . 1^ka.din
        -- TODO this one can also be slightly swung, so ta is slightly later
        , n6 (din.1^taka).din. su (din.1^ta) . 1^ka.din
        ]
    n6 = nadai 6
    purvangam dintaka =
        ta.__.kita.taka.din.__.tat.__.tat.__.din.__2 . su (1^(kita.taka))
        . dintaka . din.__.tat.__.tat.__.tam.__4
    -- variation: drop kita, so ta.__.kita -> ta.__4
    utarangam dintaka = tri_ (tam.__) $
        ta.__.kita.taka.din.na . dintaka.din.na.tat.__.tat.__
    utarangam_gap = tri $
        ta.__.kita.taka.din.na.__ . 1^takita.taka.din.na.__ . tat.tat.__.tam.__

    mridangam = make_mridangam0
        [ (ta, [k])
        , (kita, [k, t])
        , (taka, [k, o])
        -- TODO this od should be od when slow, and o when fast.  Should I try
        -- to express it with a stroke attribute, or a general
        -- instrument-specific realization heuristic?
        -- TODO also okookook -> nakatiku at high speed
        , (taka.din, [k, o, od])
        , (din.na, [od, k])
        , (na, [k])

        , (tat.tat.din, [on, on, od])
        , (din.tat.tat, [od, on, on])
        , (tat.tat, [on, on])
        , (tam, [v])

        , (1^(kita.taka), [k, t, o, k])

        -- dintakas
        , (coerce basic_dintaka, [o, k, o, o, k, o])
        , (1^taka, [k, k])
        , (din, [o])

        , (1^takita, [k, o, o])
        , (dhom, [o])
        ]

    kendang = make_kendang1
        [ (ta, [p])
        , (kita, [t, t])
        , (taka, [p, a])
        , (taka.din, [p, lt a, a])
        , (din.na, [a, p])
        , (na, [p])

        , (tat.tat.din, [o, o, a])
        , (din.tat.tat, [a, o, o])
        , (tat.tat, [o, o])
        , (tam, [u])

        , (1^(kita.taka), [t, lt a, a, p])

        -- dintakas
        , (coerce basic_dintaka, [a, p, o, o, p, lt a])
        , (1^taka, [p, p])
        , (din, [a])

        , (1^takita, [p, o, o])
        , (dhom, [o])
        ] where KendangTunggal.Strokes {..} = KendangTunggal.notes

c_17_07_19 :: Korvai
c_17_07_19 = date 2017 7 19 $ ganesh $ exercise $ korvai adi mridangam $
    map mconcat
    [ [tri (p6 . p5.p5 . dhom_tat_din 2)]
    , [p6 . p5s . dhom_tat_din 2 | p5s <- [p5, p5.p5, p5.p5.p5]]
    , [p6 . p5.p5 . dhom_tat_din n | n <- [1, 2, 3]]
    ]
    where
    dhom_tat_din gap = dhom.__n gap . tat.__n gap  . din.__
    mridangam = make_mridangam
        [ (dhom.tat.din, [o, k, od])
        ]
