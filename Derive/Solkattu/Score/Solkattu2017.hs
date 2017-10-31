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
        [ seq 4 . taka.talang.__.ga.taka.din.__.tat.__.thom.__4
        , seq 3 . tri p5 . thom.__4
        , seq 2 . tri p6 . thom.__4
        , seq 1 . tri p7 . thom.__4
        ]
    ++ let seq = sequence (su (nang.__.kita.taka)) (su nakatiku)
    in mconcat
        [ seq 4 . su nang_kita_nakatiku . taka.din.__.tat.__.thom.__4
        , seq 3 . tri (su (thom.kita.ka.na.ka.kitataka)) . thom.__4
        , seq 2 . tri (su nang_kita_nakatiku) . thom.__4
        , seq 1 . tri (su (nang.__.kitataka.nakatiku)) . thom.__4
        ]
    ++ let kitakita = su (kita.kita.taka)
        in sam.tam.__3 . kitakita . tam.__3
            . kitakita . su (nakatiku . nang_kita_nakatiku) . tam.__3
            . kitakita . su (nakatiku . repeat 2 nang_kita_nakatiku . nakatiku)
            . su nang_kita_nakatiku
            . taka.din.__.tat.__.thom.__4
    where
    -- problems:
    -- . Some soft p in tam.__3.
    -- . Maybe make the whole thing s2, but tam3 = s0 (tam.__3), where s0 sets
    -- absolute speed.
    -- . Variations, like taka.talang.__.ga, ga can be k or o.
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
        , (taka.talang.ga, [p, k, p, u, k])
        , (taka.din.tat, [p, k, o, k])
        , (thom, [od])
        , (nang.kitataka, [n, k, t, p, k])
        , (nang.kita, [o&n, p, k])
        , (thom.kita.ka.na.ka.kitataka, [o, k, t, p, u, p, k, t, p, k])
        , (kita.kitataka, [k, t, k, t, p, k])
        ]

nang_kita_nakatiku :: Sequence
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
c_17_03_20 = date 2017 3 20 $ ganesh $ comment "Trichy Sankaran plays this a\
    \ lot, see section in Laya Vinyas, adi talam solo." $
    korvai1 adi (mridangam <> kendang <> reyong) $ su $
        reduceTo 4 2 theme . sd (sd p6) . sd p6 . tri_ (__2.ga) p6
    where
    theme = tat.__.taka.takadinna.na.ka.dit.__.talang.__.ga
    -- I'm not sure if I prefer smaller fragments that are easier to read, or
    -- doing the whole theme at once.
    mridangam = make_mridangam
        [ (tat.taka, [k, k, t])
        , (na.ka.dit.talang, [n, o, k, o, u])
        , (ga, [lt k])
        ]
    kendang = make_kendang1
        [ (theme, [p, p, k, p, a, o, p, t, o, p, o, u, p])
        , (ga, [a])
        ] where KendangTunggal.Strokes {..} = KendangTunggal.notes
    reyong = make_reyong
        [ (theme, [b, k, k, x, k, k, k, i, r2, r3, b, o, b])
        , (ga, [b])
        ] where Reyong.Strokes {..} = Reyong.notes

c_17_09_25 :: Korvai
c_17_09_25 = ganesh $ date 2017 9 25 $ similar_to "Solkattu2017" "c_17_03_20" $
    korvai Tala.misra_chapu mridangam
    [ sarvaD 3 . theme
    , mconcat [v `replaceStart` theme_ . __.__. dropM 6 theme | v <- variants]
    , dropM 4 theme . sequence
    , dropM 8 theme . repeat 3 sequence
    , rdropM (6+2+6+2) sequence
    ]
    where
    theme = tat.__.taka.takadinna.na.ka.dit.__.talang.__.ga
    theme_ = tat.__.taka.takadinna.na.ka.dit.__.2^ta.lang.__.__
    variants = [tat.__, diku, thom.thom, thom.__]
    sequence = reduceTo 4 2 theme . sd (sd p6) . sd p6 . tri_ (__2.ga) p6
    mridangam = make_mridangam
        [ (tat, [k])
        , (taka, [k, t])
        , (diku, [k, p])
        , (na.ka.dit, [n, o, k])
        , (talang, [o, u])
        , (2^ta.lang, [p, u])
        , (ga, [lt k])
        , (thom, [o])
        ]

c_17_04_04 :: Korvai
c_17_04_04 = date 2017 4 4 $ source "subash chandran" $
    korvai Tala.misra_chapu mridangam $ map (sd • (purvangam.))
    [ utarangam 3 takita (ta.taka)
    , utarangam 4 takadinna takadinna
    , utarangam 5 tdgnt tdgnt
    , utarangam 6 td_gnt td_gnt
    ]
    where
    purvangam = tat.__3 . din.__3 . tadimi
              . ta.taka. din.__3 . tadimi
    utarangam n p p2 =
        spread 4 p . spread 3 p . spread 2 p . tri_ (din.__n n) p2
    tadimi = ta.di.mi.ta.takadinna
    mridangam = make_mridangam
        [ (tat.din, [k, od])
        , (ta.taka.din, [o&n, o&n, k, od])
        , (tadimi, [o&n, od, k, p&d, n, o, od, k])

        , (din, [od])
        , (tdgnt, [k, t, k, n, o])
        , (takita, [p, k, od])
        , (ta.taka, [o&n, o&n, k])
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
    purvangam = tri_ (din.__3) (ta.__3.ta.takadinna)
        . sandi (ta.takadinna) (tri_ (din.__2) (ta.takadinna))
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
        ]
    kendang = make_kendang1
        [ (ta, [p])
        , (kita, [p, k])
        , (takadinna, [t, o, o, p])
        , (dinga, [a, lt k])
        , (tdgnt, [p, k, t, a, o])
        ] where KendangTunggal.Strokes {..} = KendangTunggal.notes
    sargam = make_sargam
        [ (ta, [s1])
        , (kita, [n, s1])
        , (takadinna, [d, n, p, d])
        , (dinga, [hv g, s])
        , (tdgnt, [hv r, g, hv p, m, n])
        ]
        [
        ] where Sargam.Strokes {..} = Sargam.notes

c_17_05_11 :: Korvai
c_17_05_11 = date 2017 5 11 $ source "sriram" $
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
        , (tam, [u])
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

c_17_05_19 :: Korvai
c_17_05_19 = date 2017 5 15 $ exercise $ korvai1 adi mridangam $
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

c_17_06_02_janahan :: Korvai
c_17_06_02_janahan = tirmanam $ date 2017 6 2 $ source "janahan" $
        korvai1 adi mridangam $
    __n 9 . tri_ (su (kitataka)) (din.din . su taka . din.din.tat.din)
    -- TODO use align or pad to sam
    where
    mridangam = make_mridangam
        [ (din, [od])
        , (taka, [p, k])
        , (tat, [k])
        , (kitataka, [p, k, n, o])
        ]

c_17_06_15 :: Korvai
c_17_06_15 = date 2017 6 15 $ ganesh $ korvai adi mridangam $
    [ mconcat [suffix sequence (thom.__n gap) | gap <- [4, 3, 2]]
    , join (1^ta) [suffix sequence (thom.__n gap) | gap <- [2, 3, 4]]
    ]
    where
    sequence = [takadinna, takita, taka, ta]
    mridangam = make_mridangam
        [ (takita, [o, o, k])
        , (taka, [o, k])
        , (ta, [k])
        , (1^ta, [p])
        , (thom, [od])
        ]

c_17_06_19 :: Korvai
c_17_06_19 = date 2017 6 19 $ ganesh $ korvai1 adi inst $
    reduce3 2 ø (tat.__.dit.__.takadinna.din.__3.p5)
        . sandi p5 (trin (tam.__3) p5 (tk.p5) (tktu.p5))
    where
    inst = mridangam <> kendang <> sargam
    mridangam = make_mridangam
        [ (tat.dit, [k, t])
        , (din, [od])
        , (tam, [u])
        ]
    kendang = make_kendang1
        [ (tat.dit, [p, t])
        , (din, [a])
        ] where KendangTunggal.Strokes {..} = KendangTunggal.notes
    sargam = make_sargam
        [ (tat.dit, [p, m])
        , (takadinna, [p, m, r, m])
        , (din, [hv s])
        , (tam, [hv s_])
        , ( tk, [s, p_])
        , ( tktu, [s, p_, r, p_])
        ]
        [ ( Solkattu.PatternM 5, su [d_, s, d_, n_] . [s, n_, p_])
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
    , repeat 2 $ tanga.dinga . ta.tanga.dinga . taka.tanga.dinga . tk.tdgnt
    ]
    where
    tanga7 = tanga.dinga.din.__
    mridangam = make_mridangam
        [ (tanga, [on, k])
        , (dinga, [od, k])
        , (tat, [on])
        , (ta, [k])
        , (taka, [p, k])
        , (din, [od])
        ]

c_17_07_13 :: Korvai
c_17_07_13 = date 2017 7 13 $ ganesh $ trikalam $
    korvai adi (mridangam<>kendang) $ concat
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
    purvangam dintaka =
        ta.__.kita.taka.din.__.tat.__.tat.__.din.__2 . su (1^(kita.taka))
        . dintaka . din.__.tat.__.tat.__.tam.__4
    -- variation: drop kita, so ta.__.kita -> ta.__4
    utarangam dintaka = tri_ (tam.__) $
        ta.__.kita.taka.din.na . dintaka.din.na.tat.__.tat.__
    utarangam_gap = tri $
        ta.__.kita.taka.din.na.__ . 1^takita.taka.din.na.__ . tat.tat.__.tam.__
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
        , (basic_dintaka, [o, k, o, o, k, o])
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
        , (basic_dintaka, [a, p, o, o, p, lt a])
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

c_17_08_21 :: Korvai
c_17_08_21 = date 2017 8 21 $ sudhindra $ tirmanam $ korvai adi mridangam $
    map (__sam adi)
    [ tri_ (1^tang.__.ga) (su (kitataka.tarikita.taka) . tat.din.na)
    , tri_ (tang.__.ga) (su (tarikita.taka) . tat.din.na)
    , tri_ (tang.__.ga) (su t2)
    , tri_ (1^tang.__.ga) (tri $ su $ talang.__.ga)
    ]
    where
    t2 = takadinna.takadinna.na.ka.din.na
    mridangam = make_mridangam
        [ (kitataka.tarikita.taka, [p, k, t, p, k, t, k, t, p, k])
        , (tarikita.taka, [k, t, k, t, p, k])
        , (tat.din.na, [o, od, k])
        , (1^tang.__.ga, [od, k]) -- avoid double thoppi
        , (tang.__.ga, [od, o])

        , (takadinna.takadinna.na.ka.din.na,
            [k, o, o, k, t, o, o, k, n, o, o, k])
        , (talang.ga, [p, u, k])
        ]

c_17_08_29 :: Korvai
c_17_08_29 = date 2017 8 29 $ ganesh $ korvai Tala.misra_chapu mridangam
    [ sarvaD 7 . sarvaD 3   . theme.na.__.din
    , dit.__4 . sarvaD 2 . theme . na.__
    , repeat 2 $ dit.__4 . sarvaD 2 . tri theme . na.__

    , sequence theme2
    , sequence theme3
    , sequence theme4
    -- (4 3 2) 2 4
    -- (4 3 2) 2 4
    -- (4 3 2) 2 4
    --   (3 2) 2 4
    --   (3 2) 2 2
    --     (2) 2 2
    --     (2) 1 4
    --     (2) 2 2
    --     (2) 2 2
    --     (2) 1 4
    --     (2) 2 2
    --     (2) 2 2
    --     (2) 1
    ]
    where
    sequence t =
        tri_ (tat.__4.tam.__8) (t1.t2.t3)
        . sandi (t2.t3) (tri_ (tat.__4.tam.__8) (t2.t3))
        . sandi t3      (tri_ (tat.__.tam.__8) (tri_m (tat.__4.tam.__4) t3))
        . tat.__.tam
        where [t1, t2, t3] = take 3 $ reduceToL 0 2 t
    theme = group $ na.__.na.dinga.na.dinga.na.dinga -- 7 matras
    theme2 = na.__.na.__.na.dinga
    theme3 = dhom.ka.dhom.ka.din.na . su kitataka
    theme4 = taka.na.ka . su (kita.tarikita) . dhom
    mridangam = make_mridangam
        [ (na, [n])
        , (na.dinga, [n, d, p])
        , (na.din, [on, od])
        , (dit, [n])

        , (theme2, [on, on, on, d, o])
        , (tat, [on])
        , (tam, [u])
        , (mid^tam, [i])

        , (theme3, [o, k, o, k, o, n, k, t, o, k])
        -- TODO sandi breaks groups, fix it and I can delete these
        , (dhom.ka.din.na . su kitataka, [o, k, o, n, k, t, o, k])
        , (din.na . su kitataka, [o, n, k, t, o, k])

        , (theme4, [k, o, n, o, k, t, p, k, p, t, o])
        -- TODO as above
        , (na.ka . su (kita.tarikita) . dhom, [n, o, k, t, p, k, p, t, o])
        , (su (kita.tarikita) . dhom, [k, t, p, k, p, t, o])
        ]

c_17_10_23 :: Korvai
c_17_10_23 = date 2017 10 23 $ ganesh $ korvai adi mridangam
    [ sequence theme
    , sequence (theme2a.theme2b)
    ]
    where
    sequence t = mconcat $ map (sarvaA 8)
        [t, kitataka.t, kitakita.kitakita.t]
    theme = ta.dit.__.ta.__.kita.taka.__.din.__.ta.__
    theme2a = su $ dit.__.taka.na.taka.na.ta.kita.ki.na.thom
    theme2b = ta.__.ka.din.__.ta.__
    kitataka = su $ kita.taka
    kitakita = su $ kita.kita
    mridangam = make_mridangam
        [ (theme, [k, t, k, t, k, k, o, od, k])
        , (kitataka, [p, k, n, p])
        , (kitakita, [k, t, k, t])
        , (theme2a, [k, p, k, n, p, k, n, p, k, t, k, n, o])
        , (theme2b, [k, o, od, k])
        ]
