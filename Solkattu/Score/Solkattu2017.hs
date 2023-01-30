-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
-- | Solkattu scores from 2017.
module Solkattu.Score.Solkattu2017 where
import           Prelude hiding ((.), (^), repeat)

import qualified Solkattu.Instrument.KendangTunggal as KendangTunggal
import qualified Solkattu.Instrument.Reyong as Reyong
import qualified Solkattu.Instrument.Sargam as Sargam
import qualified Solkattu.S as S
import qualified Solkattu.Tala as Tala

import           Solkattu.Dsl.Solkattu


koraippu_janahan :: Korvai
koraippu_janahan =
    koraippu $ source "janahan" $ korvaiS1 adi mridangam $ su $
    let seq = sequence takita takadinna
    in mconcat
        [ seq 4 . taka.talang.__.ga.taka.din.__.tat.__.thom.__4
        , seq 3 . tri p5 . thom.__4
        , seq 2 . tri p6 . thom.__4
        , seq 1 . tri p7 . thom.__4
        ]
    <> let seq = sequence (nang.kttk) (su nakatiku)
    in mconcat
        [ seq 4 . su nang_kita_nakatiku . taka.din.__.tat.__.thom.__4
        , seq 3 . tri (su (thom.kita.ka.na.ka.kitataka)) . thom.__4
        , seq 2 . tri (su nang_kita_nakatiku) . thom.__4
        , seq 1 . tri (su (nang.__.kitataka.nakatiku)) . thom.__4
        ]
    <> let kitakita = su (kita.kita.taka)
        in sam.tam.__3 . kitakita . tam.__3
            . kitakita . su (nakatiku . nang_kita_nakatiku) . tam.__3
            . kitakita . su (nakatiku . r2 nang_kita_nakatiku . nakatiku)
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
    mridangam = makeMridangam $ strokes ++
        [ (1^takita, k.t.k)
        , (takita, n.p.k)
        , (takadinna, n.o.o.k)
        ]
    -- TODO not hooked up
    _janahan_mridangam = makeMridangam $ strokes ++
        [ (1^takita, k.p.k)
        , (takita.takita, o.t.k.n.o.k)
        , (takita, k.t.k)
        , (takadinna, k.o.o.k)
        ]
    strokes =
        [ (tam, od)
        , (taka.talang.ga, p.k.p.u.k)
        , (taka.din.tat, p.k.o.k)
        , (thom, od)
        , (nang.kitataka, n.k.t.p.k)
        , (nang.kita, o&n.p.k)
        , (thom.kita.ka.na.ka.kitataka, o.k.t.p.u.p.k.t.p.k)
        , (kita.kitataka, k.t.k.t.p.k)
        ]

nang_kita_nakatiku :: Sequence
nang_kita_nakatiku = nang.__.kita.nakatiku

e_spacing :: Korvai
e_spacing = exercise $ korvaiS adi (makeMridangam []) $ map (__sam adi) $
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
c_17_02_06 = date 2017 2 6 $ ganesh $ korvaiS1 adi mridangam $
    tri_ (din.__.p6.p6) (takita.dinga.din.__.ta.__.ka.__)
    where
    mridangam = makeMridangam
        [ (takita.dinga.din, k.p.k.od.__.k.od)
        , (taka, o&n.k)
        , (din, od)
        ]

c_17_03_20 :: Korvai
c_17_03_20 = date 2017 3 20 $ ganesh $
    comment "Trichy Sankaran plays in Laya Vinyas, adi talam solo, 1:54." $
    korvaiS1 adi (mridangam <> kendang <> reyong) $ su $
        sarvaD sarva (6 * 2) . theme
        . reduceTo 4 2 theme . sd (sd p6) . sd p6 . tri_ (__.__) p6
    where
    sarva = sd $ na.din.din.na
    theme = strS "tat_taka takadinna nakadit_ talang_ga"
    -- I'm not sure if I prefer smaller fragments that are easier to read, or
    -- doing the whole theme at once.
    mridangam = makeMridangam
        [ (sarva, n.d.d.n)
        , (tat.taka, k.k.t)
        , (na.ka.dit.talang, n.o.k.o.u)
        , (ga, lt k)
        ]
    kendang = makeKendang1
        [ (sarva, t.o.o.t)
        , (theme, p.__.p.k.p.a.o.p.t.o.p.__.o.u.__.p)
        ] where KendangTunggal.Strokes {..} = KendangTunggal.notes
    reyong = makeReyong
        [ (sarva, __.__.__.__)
        , (theme, b.__.k.k.x.k.k.k.i.r2.r3.__.b.o.__.b)
        ] where Reyong.Strokes {..} = Reyong.notes

c_17_09_25 :: Korvai
c_17_09_25 = ganesh $ date 2017 9 25 $ similarTo "Solkattu2017" "c_17_03_20" $
    korvaiS Tala.misra_chapu mridangam
    [ sarvaD_ 3 . theme
    , mconcat [v `replaceStart` theme_ . __.__. dropM 6 theme | v <- variants]
    , dropM 4 theme . sequence
    , dropM 8 theme . r3 sequence
    , rdropM (6+2+6+2) sequence
    ]
    where
    theme = tat.__.taka.takadinna.na.ka.dit.__.talang.__.ga
    theme_ = tat.__.taka.takadinna.na.ka.dit.__.2^ta.lang.__.__
    variants = [tat.__, diku, thom.thom, thom.__]
    sequence = reduceTo 4 2 theme . sd (sd p6) . sd p6 . tri_ (__2.ga) p6
    mridangam = makeMridangam
        [ (tat, k)
        , (taka, k.t)
        , (diku, k.p)
        , (na.ka.dit, n.o.k)
        , (talang, o.u)
        , (2^ta.lang, p.u)
        , (ga, lt k)
        , (thom, o)
        ]

c_17_04_04 :: Korvai
c_17_04_04 = date 2017 4 4 $ source "subash chandran" $
    korvaiS Tala.misra_chapu mridangam $ map (sd • (purvangam.))
    [ utarangam 3 takita (ta.taka)
    , utarangam 4 takadinna takadinna
    , utarangam 5 tdgnt tdgnt
    , utarangam 6 td_gnt td_gnt
    ]
    where
    purvangam = tat.__3 . din.__3 . tadimi
              . ta.taka. din.__3 . tadimi
    utarangam n p p2 = g (spread 4 p) . g (spread 3 p) . g (spread 2 p)
        . tri_ (din.__n n) (g p2)
    tadimi = ta.di.mi.ta.takadinna
    mridangam = makeMridangam
        [ (tat.din, k.od)
        , (ta.taka.din, o&n.o&n.k.od)
        , (tadimi, o&n.od.k.p&d.n.o.od.k)

        , (din, od)
        , (tdgnt, k.t.k.n.o)
        , (takita, p.k.od)
        , (ta.taka, o&n.o&n.k)
        , (takadinna, k.od.od.k)
        ]

c_17_04_23 :: Korvai
c_17_04_23 = date 2017 4 23 $ ganesh $ korvaiS adi mridangam $
    map sd -- remove for melkalam
    [ purvangam . utarangam (g (kp.tdgnt)) (g (kp.tdgnt))
    , purvangam . su (r32111 tdgnt . r32111 (taka.tdgnt)
        . r32111 (taka.na.ka.tdgnt))
    , purvangam . utarangam (su (ta.__3.din.__3.gin.__3.na.__3.thom.__2)) p7
    ]
    where
    r32111 ns = g (spread 3 ns) . g (spread 2 ns) . r3 (g ns)
    purvangam = tri_ (din.__3) (g (ta.__3.ta.takadinna))
        . sandi (g (ta.takadinna)) (tri_ (din.__2) (g (ta.takadinna)))
    utarangam p7 p7' = mconcat
        [ sd p7 . p7 . su end
        | end <- [p7', p7'.p7', p7'.p7'.p7']
        -- TODO some kind of x, xx, xxx function
        ]
    mridangam = makeMridangam
        [ (ta, k)
        , (din, od)
        -- TODO spread doesn't work with standard patterns, but it should
        , (taka.tdgnt, k.p.k.t.k.n.o)
        , (taka.na.ka, k.p.n.p)
        ]

c_17_05_10 :: Korvai
c_17_05_10 = date 2017 5 10 $ ganesh $ korvaiS1 adi insts $
    mconcat (map group $
        for ([ta.__n n | n <- [4, 3, 2, 1]] ++ [ø])
            (.ta.__.kita.takadinna.dinga)
        ++ for ([ta.__n n | n <- [3, 2, 1]]) (.takadinna.dinga)
        ) . reduceTo 3 1 (takadinna.dinga)
    . tri (spread 4 tdgnt . group tdgnt)

    -- TODO an alternate way to this is a reduceTo that makes a list,
    -- then zipWith a replacePrefix, e.g.:
    -- reduceTo 3 1 (ta.__4 . ta.__.kita.takadinna.dinga) `with`
    --     [ ø, ta.__3, ta.__2, ta.__, ta, ø
    --     , ø, ta.__3 , ta.__
    --     ]
    -- This is less code, but maybe not very obvious?

    -- ta.__4 . ta.__.kita.takadinna.dinga
    -- ta.__3 . ta.__.kita.takadinna.dinga
    -- ta.__  . ta.__.kita.takadinna.dinga
    --     ta . ta.__.kita.takadinna.dinga
    --          ta.__.kita.takadinna.dinga
    --            ta.__.__.takadinna.dinga
    --               ta.__.takadinna.dinga
    --                  ta.takadinna.dinga
    where
    insts = mridangam <> kendang <> sargam
    mridangam = makeMridangam
        [ (ta, k)
        , (kita, t.k)
        , (dinga, od.__.__)
        ]
    kendang = makeKendang1
        [ (ta, p)
        , (kita, p.k)
        , (takadinna, t.o.o.p)
        , (dinga, a.__.lt k)
        , (tdgnt, p.k.t.a.o)
        ] where KendangTunggal.Strokes {..} = KendangTunggal.notes
    sargam = makeSargam []
        [ (ta, s1)
        , (kita, n.s1)
        , (takadinna, d.n.p.d)
        , (dinga, hv g.__.s)
        , (tdgnt, hv r.g.hv p.m.n)
        ] where Sargam.Strokes {..} = Sargam.notes

c_17_05_19 :: Korvai
c_17_05_19 = date 2017 5 15 $ exercise $ korvaiS1 adi mridangam $
    tri (tri p8 . tri (kita.thom)) . tri p8 . p5
    where
    mridangam = makeMridangam [(kita.thom, k.n.o)]

c_17_05_19_janahan :: Korvai
c_17_05_19_janahan =
    date 2017 5 15 $ source "janahan" $ korvaiS1 adi mridangam $
    1^tat_din_din_tam 4 3 . tat_din_din_tam 4 2 . tat_din_din_tam 3 2
        . r2 (tat.__4.tam.__2.ta) . tat.__3
        . tri (group (takadinna.takita))
    where
    tat_din_din_tam a b =
          tat.__4         .    din.__4.din.__n a . tam.__n b . ta
        . tat.__2.kum.__2 . 1^(din.__4.din.__n a . tam.__n b . ta)
        -- TODO with thom
    mridangam = makeMridangam
        [ (tat, k)
        , (din, d)
        , (tam, n)
        , (1^tat, o&k)
        , (1^din, od)
        , (1^tam, o&n)
        , (ta, k)
        , (kum, o)
        , (takadinna, k.o.o&t.k)
        , (takita, n.p.k)
        ]

c_17_06_02_janahan :: Korvai
c_17_06_02_janahan = tirmanam $ date 2017 6 2 $ source "janahan" $
        korvaiS1 adi mridangam $
    __D 2 . tri_ (din.kttk) (group (din.din.tk.din.din.tat))
    where
    mridangam = makeMridangam
        [ (din, od)
        , (tk, p.k)
        , (tat, k)
        , (kttk, p.k.n.o)
        ]

c_17_06_15 :: Korvai
c_17_06_15 = date 2017 6 15 $ ganesh $ korvaiS adi mridangam $
    [ mconcat [suffix sequence (thom.__n gap) | gap <- [4, 3, 2]]
    , join (1^ta) [suffix sequence (thom.__n gap) | gap <- [2, 3, 4]]
    ]
    where
    sequence = [takadinna, takita, taka, ta]
    mridangam = makeMridangam
        [ (takita, o.o.k)
        , (taka, o.k)
        , (ta, k)
        , (1^ta, p)
        , (thom, od)
        ]

c_17_06_19 :: Korvai
c_17_06_19 = date 2017 6 19 $ ganesh $ korvaiS1 adi inst $
    reduce3 2 ø (tat.__.dit.__.takadinna.din.__3.p5)
        . sandi p5 (trin (tam.__3) p5 (kp.p5) (kpnp.p5))
    where
    inst = mridangam <> kendang <> sargam
    mridangam = makeMridangam
        [ (tat.dit, k.t)
        , (din, od)
        , (tam, u)
        ]
    kendang = makeKendang1
        [ (tat.dit, p.t)
        , (din, a)
        , (tam, a)
        ] where KendangTunggal.Strokes {..} = KendangTunggal.notes
    sargam = makeSargam
        [ (5, su (d_.s.d_.n_) . s.n_.p_)
        ]
        [ (tat.dit, p.m)
        , (takadinna, p.m.r.m)
        , (din, hv s)
        , (tam, hv s_)
        , (kp, s.p_)
        , (kpnp, s.p_.r.p_)
        ] where Sargam.Strokes {..} = Sargam.notes

c_17_06_19_koraippu :: Korvai
c_17_06_19_koraippu = date 2017 6 19 $ ganesh $ koraippu $
    korvaiS adi mridangam $ map (restD 2 .)
    [ r2 $ tanga7 . __ . tat.__4.din.__4.din.__4 . kp.tdgnt
    , r2 $ tri (tat.__4.din.__3) . kp.tdgnt
    , r2 $ tri (tat.__4) . tri (din.__3) . kp.tdgnt
    , r2 $ tri (tat.__3) . tri (din.__4) . kp.tdgnt

    -- 6 + 15
    , r2 $ nadai 6 (tanga7.ga) . tat.__5.din.__5.din.__5 . kp.tdgnt

    -- 334353
    , r2 $ tanga.dinga . ta.tanga.dinga . taka.tanga.dinga . kp.tdgnt
    ]
    where
    tanga7 = tanga.dinga.din.__
    mridangam = makeMridangam
        [ (tanga, on.__.k)
        , (dinga, od.__.k)
        , (tat, on)
        , (ta, k)
        , (taka, p.k)
        , (din, od)
        ]

-- variations:
-- purvangam . any dintaka
-- utarangam . any dintaka
--
-- purvangam [k_kt, v___]
-- utarangam

c_17_07_13 :: Korvai
c_17_07_13 = date 2017 7 13 $ ganesh $ trikalam $
    korvaiS adi (mridangam<>kendang) $ concat
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
    , (:[]) $ su $ tri (purvangam basic_dintaka)
        . one_avartanam . tri (utarangam basic_dintaka)
    ]
    where
    purvangam dintaka =
        ta.__.kita.taka.din.__.tat.__.tat.__.din.__2 . 1^kttk
        . dintaka . din.__.tat.__.tat.__.tam.__4
    -- variation: drop kita, so ta.__.kita -> ta.__4
    utarangam dintaka = tri_ (tam.__) $
        ta.__.kita.taka.din.na . dintaka.din.na.tat.__.tat.__
    utarangam_gap = tri $
        ta.__.kita.taka.din.na.__ . 1^takita.taka.din.na.__ . tat.tat.__.tam.__
    one_avartanam = ta.__4.taka.din.__ . taka.kita.taka.din.__
      . dhom.__.kita.taka.din.__ . dhom.dhom.kita.taka.din.__
    basic_dintaka = 1^(din.taka.din.taka)
    dintakas = -- each is 6 beats -- TODO map assert dur == 6
        [ basic_dintaka
        , din. 1^tk.din.din.taka
        , din. 1^tk.din.din. 1^tk.din
        , n6 (din.1^taka).din.din.taka
        , n6 (din.1^taka.din.din.1^ta) . 1^ka.din
        -- TODO this one can also be slightly swung, so ta is slightly later
        , n6 (din.1^taka).din. su (din.1^ta) . 1^ka.din
        ]
    n6 = nadai 6

    mridangam = makeMridangam0
        [ (ta, k)
        , (kita, k.t)
        , (taka, k.o)
        -- TODO this od should be od when slow, and o when fast.  Should I try
        -- to express it with a stroke attribute, or a general
        -- instrument-specific realization heuristic?
        -- TODO also okookook -> nakatiku at high speed
        , (taka.din, k.o.od)
        , (din.na, od.k)
        , (na, k)

        , (tat.tat.din, on.on.od)
        , (din.tat.tat, od.on.on)
        , (tat.tat, on.on)
        , (tam, v)

        , (1^(kita.taka), k.t.o.k)

        -- dintakas
        , (basic_dintaka, o.k.o.o.k.o)
        , (1^taka, k.k)
        , (din, o)

        , (1^takita, k.o.o)
        , (dhom, o)
        ]

    kendang = makeKendang1
        [ (ta, p)
        , (kita, t.t)
        , (taka, p.a)
        , (taka.din, p.lt a.a)
        , (din.na, a.p)

        , (tat.tat.din, o.o.a)
        , (din.tat.tat, a.o.o)
        , (tat.tat, o.o)
        , (tam, u)

        , (1^(kita.taka), t.lt a.a.p)

        -- dintakas
        , (basic_dintaka, a.p.o.o.p.lt a)
        , (1^taka, p.p)
        , (din, a)

        , (1^takita, p.o.o)
        , (dhom, o)
        ] where KendangTunggal.Strokes {..} = KendangTunggal.notes

c_17_07_19 :: Korvai
c_17_07_19 = date 2017 7 19 $ ganesh $ exercise $ korvaiS adi mridangam $
    map mconcat
    [ [tri (p6 . p5.p5 . dhom_tat_din 2)]
    , [p6 . p5s . dhom_tat_din 2 | p5s <- [p5, p5.p5, p5.p5.p5]]
    , [p6 . p5.p5 . dhom_tat_din n | n <- [1, 2, 3]]
    ]
    where
    dhom_tat_din gap = dhom.__n gap . tat.__n gap  . din.__
    mridangam = makeMridangam
        [ (dhom.tat.din, o.k.od)
        ]

c_17_08_21 :: Korvai
c_17_08_21 = date 2017 8 21 $ sudhindra $ tirmanam $ korvaiS adi mridangam $
    map (__sam adi)
    [ tri_ (1^tang.__.ga) (kttk.trkt.tk.tat.din.na)
    , tri_ (tang.__.ga) (trkt.tk.tat.din.na)
    , tri_ (tang.__.ga) (su t2)
    , tri_ (1^tang.__.ga) (tri $ su $ talang.__.ga)
    ]
    where
    t2 = takadinna.takadinna.na.ka.din.na
    mridangam = makeMridangam
        [ (kttk.trkt.tk, p.k.t.p.k.t.k.t.p.k)
        , (trkt.tk, k.t.k.t.p.k)
        , (tat.din.na, o.od.k)
        , (1^tang.__.ga, od.__.k) -- avoid double thoppi
        , (tang.__.ga, od.__.o)
        , (takadinna.takadinna.na.ka.din.na, k.o.o.k.t.o.o.k.n.o.o.k)
        , (talang.ga, p.u.k)
        ]

c_17_08_29 :: Korvai
c_17_08_29 = date 2017 8 29 $ ganesh $
    korvaiS Tala.misra_chapu (mridangam<>kendang1)
    [ sarvaD_ 7 . sarvaD_ 3   . develop.na.__.din
    , dit.__4 . sarvaD_ 2 . develop . na.__
    , r2 $ dit.__4 . sarvaD_ 2 . tri develop . na.__

    , sequence theme1
    , sequence theme2
    , sequence theme3
    ]
    where
    sequence t =
        tri_ (tat.__4.tam.__8) (t4.t3.t2)
        . sandi (t3.t2) (tri_ (tat.__4.tam.__8) (t3.t2))
        . sandi t2      (tri_ (tat.__.tam.__8) (tri_nomid (tat.__4.tam.__4) t2))
        . tat.__.tam
        where [t4, t3, t2] = take 3 $ reduceToL 0 2 t
    -- (4  3  2)  2  4
    -- (4  3  2)  2  4
    -- (4{ 3  2)
    --    (3  2)} 2  4
    --    (3  2)  2  4
    --    (3{ 2)
    --       (2)} 2  2  (2)  2   2 (2)
    --            1  4
    --       (2)  2  2  (2)  2  2  (2)
    --            1  4
    --       (2)  2  2  (2)  2  2  (2)
    --            1  4
    develop = group $ na.__.na.dinga.na.dinga.na.dinga -- 7 matras
    theme1 = na.__.na.__.na.dinga
    theme2 = dhom.ka.dhom.ka.din.na . kttk
    theme3 = taka.na.ka.kt.trkt.dhom
    mridangam = makeMridangam
        [ (na, n)
        , (na.dinga, n.d.__.p)
        , (na.din, on.od)
        , (dit, n)

        , (theme1, on.__.on.__.on.d.__.o)
        , (tat, on)
        , (tam, u)
        , (mid^tam, i)

        , (theme2, o.k.o.k.o.n.k.t.o.k)
        , (theme3, k.o.n.o.k.t.p.k.p.t.o)
        ]
    kendang1 = makeKendang1
        [ (na, t)
        , (na.dinga, o.u.__.p)
        , (na.din, o.a)
        , (dit, t)

        , (theme1, t.__.t.__.o.u.__.p)
        , (tat, p)
        , (tam, o)
        , (mid^tam, a)

        , (theme2, a.p.a.p.a.o.p.lt a.a.p)
        , (theme3, p.k.t.p.p.k.p.k.p.lt a.a)
        ] where KendangTunggal.Strokes {..} = KendangTunggal.notes

c_17_10_23 :: Korvai
c_17_10_23 = date 2017 10 23 $ ganesh $ koraippu $
    korvaiS adi (mridangam<>kendang1)
    [ sarvaD_ 8 . sarvaD_ 4.5 . t1
        . sarvaD_ 8 . sarvaD_ 4 . kitataka.t1
        . sarvaD_ 8 . sarvaD_ 3.5 . kitakita.kitataka.t1
        . sarvaD_ 8 . sarvaD_ 3.5 . kitakita.kitataka.t2
    , sarvaD_ 4.5 . t1 . sarvaD_ 4 . kitataka.t1
        . sarvaD_ 3.5 . kitakita.kitataka.t1
        . sarvaD_ 3.5 . kitakita.kitataka.t2
    , din.__8 . r3 (k_pkn.tat.__7) . t1
        . din.__8 . r3 (o_okn.k_pkn) . t1
    , r2 $ din.__4 . o_okn.k_pkn.t1
    , r3 (din.__.t2) . r2 (__.g tend2)
    -- korvai
    , k_pkn.g tend2 . r3 (tat.__3.din.__3)
        . kitataka.k_pkn.g tend2 . r3 (tat.__.din.__3)
        . kitakita.kitataka.k_pkn.g tend2 . r3 (tat.din.__3)
        . g (spread 3 tdgnt) . g (spread 2 tdgnt)
        . trin (tat.__.tat.__3.tam.__.tam.__3) (tri p5) (tri p6) (tri p7)
    -- alternate endings
    , let tkp = tri_ (su kp) in
        restD 7 . __ . spread 3 tdgnt . spread 2 tdgnt
        . trin (tat.__.tat.__.tam.__3) (tkp p5) (tkp p6) (tkp p7)
    , restD 7 . __ . spread 3 tdgnt . spread 2 tdgnt
        . trin (tat.__.tat.__3.tam.__.tam.__3)
            (tri p5) (tri (su kp.p5)) (tri (su kpnp.p5))
    ]
    where
    t1 = g $ t1_sollu.tend
    t1_sollu = ta.dit.__.ta.__.kita
    t2 = g $ k_pkn . tend2
    tend = taka.__.din.__.tat.__
    tend2 = ta.__.ka.din.__.tat.__
    k_pkn = g $ su $ dit .__.taka.na.taka.na.taka.tadikita
    o_okn = g $ su $ thom.__.taka.na.taka.na.taka.tadikita

    kitataka = g $ kttk
    kitakita = g $ kt.kt
    mridangam = makeMridangam
        [ (t1_sollu, k.t.__.k.__.t.k)
        , (tend, hv k.o.__.od.__.k.__)
        , (k_pkn, k.__.p.k.n.p.k.n.p . k.t.k.n.o)
        , (o_okn, k&o.__.o.k.n.o.k.n.o . k.t.k.n.o)
        , (kitataka, p.k.n.p)
        , (kitakita, k.t.k.t)
        , (din, od)
        , (tat, k)
        , (tat.tat.tam.tam, p&k.p&k.od.od)
        , (tat.tat.tam, p&k.p&k.od)
        ]
    kendang1 = makeKendang1
        [ (t1_sollu, p.k.__.p.__.k.p)
        , (tend, p.a.__.o.__.p.__)
        , (k_pkn, p.__.k.p.t.k.p.t.p . o.p.k.t.a)
        , (o_okn, a.__.a.p.t.a.p.t.p . o.p.k.t.a)
        , (kitataka, k.t.t.k)
        , (kitakita, p.k.p.k)
        , (din, a)
        , (tat, p)
        , (tat.tat.tam.tam, pk.pk.a.a)
        , (tat.tat.tam, pk.pk.a)
        ] where KendangTunggal.Strokes {..} = KendangTunggal.notes

c_20_12_12_kanda :: Korvai
c_20_12_12_kanda = date 2020 12 12 $ koraippu $
    korvaiS Tala.kanda_chapu (mridangam<>kendang1) $ map sd
    [ sarvaD_ (13/2) . t1
        . sarvaD_ (12/2) . kitataka.t1
        . sarvaD_ (11/2) . kitakita.kitataka.t1
        . sarvaD_ (3/2) . t1
        . sarvaD_ (2/2) . kitataka.t1
        . sarvaD_ (1/2) . kitakita.kitataka.t1
        -- . sarvaD_ (3/2) . t2
        -- . sarvaD_ (2/2) . kitataka.t2
        -- . sarvaD_ (1/2) . kitakita.kitataka.t2
    , din.__4 . r4 t1 -- practice
    , din.__4 . r3 (k_pkn.tat.__7) . t1
    , din.__4 . r3 (o_okn.k_pkn) . t1
    , r2 $ din.__ . o_okn.k_pkn.t1
    , r2 $ din.t1
    -- korvai
    , k_pkn.g tend2 . r3 (tat.__3.din.__3)
        . kitataka.k_pkn.g tend2 . r3 (tat.__.din.__3)
        . kitakita.kitataka.k_pkn.g tend2 . r3 (tat.din.__3)
        . g (spread 3 tdgnt) . g (spread 2 tdgnt)
        . trin (tat.__.tat.__3.tam.__.tam.__) (tri p5) (tri p6) (tri p7)
    ]
    -- karvai 2 + 7 * 2 = 16 = 4 avartanams
    -- 2 + 7 * 7
    --
    -- 4+4 + 7 7 7 7 7 7 (7 7) = 8 + 7*8 = 64 / 4*8 32 = 4
    -- 4 + 7 7 7 7 = 32
    -- 2 + 7 7 = 16
    -- 1 + 7 = 8
    --
    -- korvai:
    -- 7+7 + 3*6 + (2)+7+7 + 3*5 + (4)+7+7 + 3*4 = 93 = 23 1/4
    -- 5*3 + 5*2 = 25 = 118 = 29 2/4
    -- 5+5+5 + 10 + 6+6+6 + 10 + 7+7+7 = 74
    --      = 192 matra = 48 akshara = 6 avartanam
    -- each avartanam is 4*5 = 20
    --
    --
    -- use 4 + 8*7, 2 + 4*7, (1 + 77) * 4
    -- k k d d _ -> k k _d d
    where
    t1 = g $ t1_sollu.tend
    t1_sollu = ta.dit.__.ta.__.kita
    -- t2 = g $ k_pkn . tend2
    tend = taka.__.din.__.tat.__
    tend2 = ta.__.ka.din.__.tat.__
    k_pkn = g $ su $ dit .__.taka.na.taka.na.taka.tadikita
    o_okn = g $ su $ thom.__.taka.na.taka.na.taka.tadikita

    kitataka = g $ kttk
    kitakita = g $ kt.kt
    mridangam = makeMridangam
        [ (t1_sollu, k.t.__.k.__.t.k)
        , (tend, hv k.o.__.od.__.k.__)
        , (k_pkn, k.__.p.k.n.p.k.n.p . k.t.k.n.o)
        , (o_okn, k&o.__.o.k.n.o.k.n.o . k.t.k.n.o)
        , (kitataka, p.k.n.p)
        , (kitakita, k.t.k.t)
        , (din, od)
        , (tat, k)
        , (tam, od)
        ]
    kendang1 = makeKendang1
        [ (t1_sollu, p.k.__.p.__.k.p)
        , (tend, p.a.__.o.__.p.__)
        , (k_pkn, p.__.p.k.t.p.k.t.p . o.p.k.t.a)
        , (o_okn, a.__.o.k.t.o.k.t.o . o.p.k.t.a)
        , (kitataka, o.k.t.a)
        , (kitakita, p.k.p.k)
        , (din, a)
        , (tat, p)
        , (tam, a)
        ] where KendangTunggal.Strokes {..} = KendangTunggal.notes

{-
  . convert to kanda chapu
    rupaka D pkkoD D N
           D D NND D N
           D d nnd d n
           d D NND D N
    rupaka chop off beat to get kandachapu
           D pkkoD N
           D D NND N
           D d nnd n
           d D NND N
-}

c_17_12_11 :: Korvai
c_17_12_11 = date 2017 12 11 $ ganesh $ korvaiS adi mridangam
    [ __sam adi $ su theme
    ]
    where
    theme = kita.kita.taka.naka
        . kita.kita.gu.gu.na.na
        . taka.tiku.kita.__.ki.na.thom
        . tri (din.__.ta.__.ka.dinga)
        . tat.__.dit
    mridangam = makeMridangam
        [ (kita, k.t)
        , (taka.naka, p.k.n.p)
        , (gu.gu.na.na, o.o.n.n)
        , (taka.tiku, p.k.t.p)
        , (kita.ki.na.thom, k.t.k.n.o)
        , (din.taka.dinga, on.k.o.d.__.o)
        , (tat.dit, on.u)
        ]

speaking1 :: Korvai
speaking1 = ganesh $ exercise $ korvaiS Tala.any_beats mridangam $
    -- 5s, 15 beats
    [ r4 $ t5.t5 . su (t5.t5)
    , r4 $ nadai 6 (in3 (g tdgnt.g tdgnt)) . su (t5.t5)

    -- 7s, 21 beats
    , r4 $ t7.t7 . su (t7.t7)
    , r4 $ nadai 6 (in3 (g (taka.tdgnt) . g (taka.tdgnt))) . su (t7.t7)
    -- 9s, 27 beats
    , r4 $ t9.t9 . su (t9.t9)
    , r4 $ nadai 6 (in3 (g (taka.tiku.tdgnt) . g (taka.tiku.tdgnt)))
        . su (t9.t9)
    ]
    where
    -- sequence t =
    --     [ r4 $ t.t . su (t.t)
    --     , r4 $ nadai 6 (group (in3 t) . group (in3' t)) . su (t.t)
    --     ]
    --     where
    --     in3' (a:b:cs) = [a, b] . __ . in3 cs
    --     in3' xs = xs

    g = id -- should be group but then in3 doesn't work
    -- TODO I need a way to transform inside a group
    in3 seq = case S.toList seq of
        a : b : cs -> S.singleton a . __ . S.singleton b . in3 (S.fromList cs)
        _ -> seq
    t5 = group tdgnt
    t7 = group $ taka.tdgnt
    t9 = group $ taka.tiku.tdgnt
    mridangam = makeMridangam
        [ (taka, k.p)
        , (tiku, n.p)
        ]
