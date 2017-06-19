-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
-- | Solkattu scores from 2017.
module Derive.Solkattu.Score.Solkattu2017 where
import Prelude hiding ((.), (^), repeat)

import qualified Derive.Solkattu.KendangTunggal as KendangTunggal
import qualified Derive.Solkattu.Reyong as Reyong
import Derive.Solkattu.SolkattuGlobal
import qualified Derive.Solkattu.Tala as Tala

import Global


-- * chatusra nadai

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
    mridangam = make_mridangam $ standard_strokes ++
        [ (ta, [k])
        , (din, [od])
        -- TODO spread doesn't work with standard patterns, but it should
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

c_17_06_15 :: Korvai
c_17_06_15 = date 2017 6 15 $ ganesh $ korvai adi mridangam $
    [ mconcat [suffix sequence (thom.__n gap) | gap <- [4, 3, 2]]
    , join (1^ta) [suffix sequence (thom.__n gap) | gap <- [2, 3, 4]]
    ]
    where
    sequence = [takadinna, takita, ta.ka, ta]
    mridangam = make_mridangam $ standard_strokes ++
        [ (takita, [o, o, k])
        , (ta.ka, [o, k])
        , (ta, [k])
        , (1^ta, [p])
        , (thom, [od])
        ]

-- * misra nadai

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

-- * tirmanam

janahan_17_06_02 :: Korvai
janahan_17_06_02 = tirmanam $ date 2017 6 2 $ source "janahan" $
        korvai1 adi mridangam $
    __n 9 . tri_ (din . su (ki.ta.ta.ka)) (din.din . su (ta.ka) . din.din.tat)
    -- TODO actually the din is at sam, I should have a way to allow avartanam
    -- + sam.
    where
    mridangam = make_mridangam
        [ (din, [od])
        , (ta.ka, [p, k])
        , (tat, [k])
        , (ki.ta.ta.ka, [p, k, n, o])
        ]

-- * koraippu

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

nang_kita_nakatiku :: Seq stroke
nang_kita_nakatiku = nang.__.ki.ta.nakatiku

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
