-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
-- | Korvais expressed in "Solkattu.Dsl".
module Solkattu.Score.Solkattu2014 where
import Prelude hiding ((.), (^), repeat)
import qualified Data.List as List

import qualified Solkattu.Instrument.Mridangam as Mridangam

import Solkattu.Dsl.Solkattu


-- 2014-01-08 nadai practice

c_14_01_01 :: Korvai
c_14_01_01 = date 2014 1 1 $ ganesh $ korvaiS adi mridangam
    -- TODO back to sarva with D.__3/2
    [ sarvaSam adi theme . sarvaSam adi (kp.theme) . sarvaSam adi (kpnp.theme)
    , structure (din.__3) (ta.din.__3 . p5)
    , structure (din.__2) (ta.din.__2 . p5.tam.__2)
    , structure dhom (kp.p5.tam.__4)
    -- same, but in tisram
    , nadai 6 $ structure (din.__3) (ta.din.__3 . p5)
    -- etc.
    ]
    where
    structure gap fill =
             theme . gap . fill
         .kp.theme . gap . fill
       .kpnp.theme . gap . tri fill
    theme = ta.dit.__.ta.din.__.ta.__.din.__.ta.__
    mridangam = makeMridangam
        [ (ta.din, k.od)
        , (ta.dit, k.t)
        , (ta, k)
        , (dhom, o)
        , (tam, u)
        ]

c_14_01_14 :: Korvai
c_14_01_14 = date 2014 1 14 $ ganesh $ korvaiS adi mridangam
    -- development
    [ sarvaSam adi theme -- end with tam!u
        . sarvaSam adi (dropM 1 theme)
        . sarvaA_ 4 theme . sarvaA_ 4 (dropM 1 theme)
        . __a 2 theme . __a 2 (dropM 1 theme)
        . __a 2 theme . r2 (__ . dropM 3 theme)
    , tri_ (tam.__) reduce
    , trin (tam.__) reduce reduce expand
    , tri_ (tam.__) expand

    -- date 2014 1 23
    , reduce1 ø . utarangam 4
    , reduce1 __2 . utarangam 3
    , reduce1 __3 . utarangam 2

    , expand1 ø . utarangam 4
    , expand1 __2 . utarangam 3
    , expand1 __3 . utarangam 2
    ]
    where
    theme = g $ tha.ki.ta.ta . su kita . thom
    reduce = reduceTo 2 1 theme
    expand = mconcat $ List.reverse $ reduceToL 2 1 theme

    utarangam gap = r3 (tat.__n gap .di.__n gap . dropM 1 theme . tam.__2)
    reduce1 karv = prefixes (reduceToL 1 1 theme) karv
    expand1 karv = prefixes (List.reverse (reduceToL 1 1 theme)) karv

    mridangam = makeMridangam
        [ (theme, p.k.t.k.k.t.o)
        , (tat.di, k.t)
        , (tam, u)
        , (mid^tam, i)
        ]

c_14_02_05 :: Korvai
c_14_02_05 = date 2014 2 5 $ ganesh $ korvaiS adi mridangam $
    [ utarangam . purvangam (replicate 3 (tadi_ . ta_kitathom.ta_kitathom))
    , utarangam . purvangam [tadi_ . repeat n ta_kitathom | n <- [1, 2, 3]]
    , utarangam . purvangam (replicate 3 (tadi_ . ta.p5))
    , utarangam . purvangam (replicate 3 (p6 . su p6))

    -- 2022-10-10
    , theme . r3 (tat_din 3)
        . dropM 1 theme . r3 (tat_din 2)
        . dropM 2 theme . r3 (tat_din 1)
        . purvangam2
    , prefixes [theme, dropM 1 theme, dropM 2 theme]
        (tat_din 3.tat_din 2.tat_din 1)
        . purvangam2
    -- 2022-10-16
    , theme2.tat_din 3 . dropM 1 theme2.tat_din 2 . dropM 2 theme2.tat_din 1
        . tri_tk (tam.__) (p62.su p62)
    ]
    where
    tat_din n = tat.__n n.din.__3
    tadi_ = ta.di.__
    p5 = pattern $ su $ nang.__.kita.ta.ri.kita.thom.__
    utarangam =   theme . tat_din 3
        . dropM 1 theme . tat_din 2
        . dropM 2 theme . tat_din 1
    purvangam seqs = join (tam.__) $ zipWith (.) [ø, su kp, su kpnp] seqs
    purvangam2 = tri_tk (tam.__3) (p6.su p6)
    tri_tk sep p = p.sep.su kp.p.sep.su kpnp.p
    theme = g $ tha.ki.ta.ta . su kita . thom
    theme2 = g $ su $ taka.kita.taka.tari.kita.taka
    p62 = pattern $ tat.dit.__.su (kita.kita).thom
    ta_kitathom = dropM 3 theme
    mridangam = makeMridangam
        [ (theme, p.k.t.k.k.t.o)
        , (theme2, "upktpkpktkno")
        , (p62, "kt_ktkto")
        , (tat.din, k.od)
        , (ta.di, k.t)
        , (ta, k)
        , (p5, n.__.k.t.p.k.p.t.o.__)
        , (tam, od)
        ]

c_14_02_20 :: Korvai
c_14_02_20 = date 2014 2 20 $ ganesh $ exercise $ korvaiS adi mridangam $
        map (nadai 6)
    [     r7 sarva . din_dhodhoka
        . r2 (r3 sarva . din_dhodhoka)
        . r4 (sarva . din_dhodhoka)
        . r7 din_dhodhoka . r2 dhodhoka
    ,     r6 sarva . ta_katakita.takadinna
        . r2 (r2 sarva . ta_katakita.takadinna)
        . r4 (ta_katakita.takadinna)
    ,     tri_ (din.__6) (ta_katakita.takadinna)
        . ta_katakita.takadinna.din.__6 . ta_katakita.takadinna.din.__3
            . taka.takadinna.din.__3
            . taka.takadinna
    -- TODO sarvaA doesn't work because of nadai 6
    ]
    where
    sarva = tanga.takita
    din_dhodhoka = din.__3.dhodhoka
    dhodhoka = dhom.dhom.ka
    mridangam = makeMridangam $
        [ (sarva, d.__.lt p.k.t.k)
        , (din_dhodhoka, d.__3.o.o.k)
        , (dhodhoka, o.o.k)
        , (din, od)
        , (taka, p.k)
        ] ++ m_ta_katakita

c_14_02_27 :: Korvai
c_14_02_27 = date 2014 2 27 $ ganesh $ korvaiS adi mridangam $
        map (nadai 6 • (purvangam.))
    [ spread 3 tdgnt . spread 2 tdgnt . tri_ __ tdgnt
    , spread 3 tdgnt . tri (ta.__.din.__.gin.__.na.__.thom)
    , tri_ (dheem.__3) (ta.din.__.ta.__.din.__.p5)
    , tri_ (dheem.__3) (p5.ta.__.din.__.ta.din.__)
    , p123 p6 (dheem.__3)

    , p123 p5 (tat.__3.din.__3)
    , p5.dinga . kp.p5.p5. dinga . kpnp.p5.p5.p5
    , tri (tat.dinga . tat.__.dinga.p5)
    , spread 3 (taka.tdgnt) . spread 2 (taka.tdgnt) . taka.tdgnt
    ]
    where
    p123 p sep = trin sep p (p.p) (p.p.p)
    purvangam = tri (ta_katakita.takadinna . din.__6)
    mridangam = makeMridangam $
        [ (ta.din, k.od)
        , (dheem, u)
        , (din, od)
        , (tat, k)
        , (dinga, od.__.p)
        , (taka, k.p)
        ] ++ m_ta_katakita

ta_katakita :: Sequence
ta_katakita = ta.__.ka.takita.taka

m_ta_katakita :: StrokeMap Mridangam.Stroke
m_ta_katakita =
    [ (ta_katakita, k.__.lt p.k.t.k.t.k)
    ]

c_14_03_13 :: Korvai
c_14_03_13 = date 2014 3 13 $ ganesh $ korvaiS adi mridangam
    [ sarvaD_ 4 . t1.din . sarvaD_ 3
    . r2 (t1.din . sarvaD_ 3)
    , mconcatMap sequence [t1, t2, t3]
    ]
    where
    sequence p = p.din . sarvaD_ 3 . p.din . sarvaD_ 1 . p
        . dropM (1/2) p . dropM (1/2) p
    t1 = su $ din.__.kitataka
    t2 = su $ ka.tdgnt
    t3 = su $ takadinna.taka

    -- sarva = d.__.n.d.l.d.n.k .t.k.n.d.l.d.n.l -- TODO sarva
    --         o.__.o.o._.o.o._ .o._.o.o._.o.o._
    mridangam = makeMridangam
        [ (t1, od.__.p.k.n.o)
        , (t2, p.k.t.k.n.o)
        , (t3, k.o.o.k.o.k)
        , (din, od)
        ]

c_14_03_26 :: Korvai
c_14_03_26 = date 2014 3 26 $ ganesh $ similarTo "Solkattu2014" "c_14_03_13" $
        korvaiS adi mridangam $
    [ t1 . sarvaD_ 7 . r2 (t1 . sarvaD_ 3)
        . r2 (t1 . sarvaD_ 1) . t1 . sarvaD_ 3
        . r3 (t1.__) . ka . t1 . sarvaD_ 3
        . t1.__.ka . r2 (t1.__) . t1 . sarvaD_ 3
    ] ++ map sequence [t1, t2, t3, t4, t5]
    where
    -- Same sarva as c_14_03_13.  TODO sarva
    sequence p = tri_ (dheem.__3) (r3 (p.__) . tri p5)
    t1 = group $ na.na.na.din
    t2 = group $ su $ dhom.__.taka.taka.din.__
    t3 = group $ su $ tam.__.taka.na.ka.din.__
    t4 = group $ su $ takadinna.taka.din.__
    t5 = group $ su $ dhom.tdgnt.din.__
    -- 2014-04-02 has transition from c_14_03_12 to this
    mridangam = makeMridangam
        [ (t1, on.on.on.od)
        , (t2, o.__.k.o.k.o.od.__)
        , (t3, on.__.p.k.n.o.od.__)
        , (t4, k.o.o.k.k.o.od.__)
        , (t5, o.k.t.k.n.o.od.__)
        , (ka, k)
        , (dheem, i)
        ]

c_14_04_21 :: Korvai
c_14_04_21 = date 2014 4 21 $ ganesh $ korvaiS adi mridangam $
    [ tri_ (tam.__3) $ tri (su (dhom.p5).din.__) . tri p5_1
    , tri_ (tam.__3) $ tri (su (dhom.p5).din.__) . tri p5_2
    ]
    where
    p5_1 = pattern $ ta.__.tk.tk.din
    p5_2 = pattern $ ta.__.ta.tk.din
    mridangam = makeMridangam
        [ (din, od)
        , (tam, u)
        , (p5_1, k.__.k.t.k.t.o)
        , (p5_2, k.__.k.k.t.o)
        ]

c_14_04_29 :: Korvai
c_14_04_29 = date 2014 4 29 $ ganesh $ korvaiS adi mridangam $
    -- TODO sarva is namita dimita dimi
    [ sarvaSam adi t1
        . sarvaA_ 4 t1 . sarvaA_ 4 t1
        . 1^t1 . t1 . 1^t1 . t1
        -- TODO still not right, t1 after 1^t1 should drop the first thom.
    ] ++ map sequence ts
    where
    sequence t = tri_ (din.__.takita) (takeM 5 t . takeM 5 t . t)
    t1 = group $ nang.kttk.din.na.__.di.mi
    t2 = theme (dhom.ka)
    t3 = theme (su (ta.ki.taka))
    t4 = theme (su (dhom.ki.ta.ki))
    t5 = theme (su (talang.__.ga))
    t6 = theme (su (din.na.tat.__))
    t7 = theme (__.__)
    ts = [t1, t2, t3, t4, t5, t6, t7]

    theme xx = group $ nang.kttk.xx.nang.kttk
    theme_m xx = on.k.t.o.k . xx.n.k.t.o.k
                                 -- khali: p
    mridangam = makeMridangam
        [ (t1,   on.k.t.o.k.od.n.__.od.k)
        , (1^t1, on.k.t.o.k.od.n.__.p&d.k)
        , (t2, theme_m (o&t.k))
        , (t3, theme_m (o.k.o.t))
        , (t4, theme_m (o.k.t.k))
        , (t5, theme_m (o.u.__.k))
        , (t6, theme_m (o.k.k.__))
        , (t7, theme_m (__.__))
        , (din.takita, od.k.p.k)
        ]

-- faran + nadai exercise at 14-04-29.

c_14_06_06 :: Korvai
c_14_06_06 = date 2014 6 6 $ ganesh $ comment "chatusra tisram" $
    korvaiS adi mridangam
    -- sarva is nami dimi nami dimi
    [ r2 (sarvaD_ 5 . nadin4)
        . din.__4 . nadin4.din.__4 . spread 3 tadindintat

    , purvangam (tat.__3) (tat.__3.dit.__3)
        . tri_ (din.__3) (1^ta.ka.__.din.na.__)
    , purvangam (taka.ta) (takita.taka.ta)
        . tri_ dinga (1^ta.ka.__.din.na.__)
    , purvangam (taka.ta) (takita.taka.ta)
        . tri_ dinga (ki.takadinna.__)
    -- Pure chatusra tisram.
    , stride 3 $ tadindintat.din.__3
        . su taka . tadindintat.din.__3
        . su (kita.taka) . tadindintat.din.__3
        . tri_ thom (su takadinna)

    -- Variation.
    , purvangam2 . tri (tam.__7 . p5)
    , purvangam2 . tri (nadai 3 (dheem.__4 . tdgnt))
    ]
    where
    purvangam x1 x2 = spread 3 (tadindintat.din.__3)
        . x1 . spread 3 (tadindintat.din.__3)
        . x2 . spread 3 (tadindintat.din.__3)
    tadindintat = tat.din.din.tat
    purvangam2 = t . tat.__3.t . tat.__3.dit.__3.t
        where t = spread 3 (tat.din.din) . tat.__.din.__6

    nadin4 = r4 (na.din.__)
    mridangam = makeMridangam
        [ (na.din, on.d)
        , (din, od)
        , (dinga, od.__.k)
        , (tadindintat, k.od.od.k)
        , (tat, k)
        , (dit, t)
        , (taka.ta, k.p.k)
        , (takita.taka.ta, k.p.k.t.p.k)
        , (ki.takadinna, t.k.o.o.k)
        , (tam, u)
        , (dheem, i)

        , (taka, k.t)
        , (kita.taka, k.t.p.k)
        , (takadinna, k.o.od.k)
        ]
