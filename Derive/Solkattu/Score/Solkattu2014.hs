-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
-- | Korvais expressed in "Derive.Solkattu.Dsl".
module Derive.Solkattu.Score.Solkattu2014 where
import Prelude hiding ((.), (^), repeat)
import qualified Data.List as List

import qualified Derive.Solkattu.Korvai as Korvai
import Derive.Solkattu.SolkattuGlobal


-- 2014-01-08 nadai practice

c_14_01_01 :: Korvai
c_14_01_01 = date 2014 1 1 $ ganesh $ korvai adi mridangam
    -- TODO back to sarva with D.__3/2
    [ sarvaSam adi theme . sarvaSam adi (tk.theme) . sarvaSam adi (tknk.theme)
    , structure (din.__3) (ta.din.__3 . p5)
    , structure (din.__2) (ta.din.__2 . p5.tam.__2)
    , structure dhom (tk.p5.tam.__4)
    -- same, but in tisram
    , nadai 6 $ structure (din.__3) (ta.din.__3 . p5)
    -- etc.
    ]
    where
    structure gap fill =
             theme . gap . fill
         .tk.theme . gap . fill
       .tknk.theme . gap . tri fill
    theme = ta.dit.__.ta.din.__.ta.__.din.__.ta.__
    mridangam = make_mridangam
        [ (ta.din, [k, od])
        , (ta.dit, [k, t])
        , (ta, [k])
        , (dhom, [o])
        ]

c_14_01_14 :: Korvai
c_14_01_14 = date 2014 1 14 $ ganesh $ korvai adi mridangam
    -- development
    [ sarvaSam adi theme -- end with tam!u
        . sarvaSam adi (dropM 1 theme)
        . sarvaA 4 theme . sarvaA 4 (dropM 1 theme)
        . __a 2 theme . __a 2 (dropM 1 theme)
        . __a 2 theme . repeat 2 (__ . dropM 3 theme)
    , tri_ (tam.__) reduce -- TODO u, i, u substitution
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
    theme = tha.ki.ta.ta . su kita . thom
    reduce = reduceTo 2 1 theme
    expand = mconcat $ List.reverse $ reduceToL 2 1 theme

    utarangam gap = repeat 3 (tat.__n gap .di.__n gap . dropM 1 theme . tam.__2)
    reduce1 karv = prefixes (reduceToL 1 1 theme) karv
    expand1 karv = prefixes (List.reverse (reduceToL 1 1 theme)) karv

    mridangam = make_mridangam
        [ (theme, [p, k, t, k, k, t, o])
        -- TODO necessary because dropM doesn't preserve the original sequence
        , (tha, [p])
        , (ki.ta.ta.kita.thom, [k, t, k, k, t, o])
        -- TODO would be fixed by reduction groups
        , (ta.ta.kita.thom, [k, k, k, t, o]) -- TODO t->k substitution
        , (ta.kita.thom, [k, k, t, o])
        , (kita.thom, [k, t, o])
        , (thom, [o])
        , (tat.di, [k, t])
        ]

c_14_02_05 :: Korvai
c_14_02_05 = date 2014 2 5 $ ganesh $ korvai adi mridangam $
    [ utarangam . purvangam (tam.__)
        (replicate 3 (tadi_ . ta_kitathom.ta_kitathom))
    , utarangam . purvangam (tam.__)
        [tadi_ . repeat n ta_kitathom | n <- [1, 2, 3]]
    , utarangam . purvangam (tam.__) (replicate 3 (tadi_ . ta.nang_kita))
    , utarangam . purvangam (tam.__) (replicate 3 (td_gnt . su td_gnt))

    -- date 2014 2 20
    , utarangam2
        . purvangam (tam.__.ga) (replicate 3 (tadi_ . ta_kitathom.ta_kitathom))
    ]
    where
    tadi_ = ta.di.__
    nang_kita = su $ nang.__.kita.ta.ri.kita.thom.__
    utarangam =   theme . tat.__3.din.__3
        . dropM 1 theme . tat.__2.din.__3
        . dropM 2 theme . tat.din.__3
    utarangam2 =  theme . tri (tat.__3.din.__3)
        . dropM 1 theme . tri (tat.__.din.__3)
        . dropM 2 theme . tri (tat.din.__3)
    purvangam karv seqs =
        join karv $ zipWith (.) [ø, su tk, su tknk] seqs
    theme = tha.ki.ta.ta . su kita . thom
    ta_kitathom = dropM 3 theme
    mridangam = make_mridangam
        [ (tha, [p])
        , (ki.ta.ta.kita.thom, [k, t, k, k, t, o])
        , (ta.di, [k ,t])
        , (ta, [k])
        , (ga, [lt p])
        , (nang_kita, [n, k, t, p, k, p, t, o])
        -- TODO would be fixed by reduction groups
        , (ta.ta.kita.thom, [k, k, k, t, o]) -- TODO t->k substitution
        , (ta.kita.thom, [k, k, t, o])
        , (tat.din, [k, od])
        ]

-- first mentioned on 2013 11 19
c_13_11_19 :: Korvai
c_13_11_19 = date 2013 11 19 $ ganesh $ korvai1 adi mridangam $ mconcat
    [ sarvaSam adi theme
    , sarvaA 4 theme . sarvaA 4 theme
    , theme . 1^theme . theme . 1^theme
    -- TODO this is still not quite right, should be:
    -- n kt kd n _ d k n|kt kd n _ d k n|
    -- o _ o o _ _ p _ _ _ o o _ _ o _ o
    ]
    where
    theme = nang . su (ki.ta.ta.ka) . din.na.__.di.mi
    -- sarva is namita dimita dim
    mridangam = make_mridangam
        [ (theme,   [on, k, t, o, k, od, n, od, k])
        , (1^theme, [on, k, t, o, k, od, n, p&d, k])
        ]

make_mohra :: Korvai.StrokeMaps -> (Sequence, Sequence, Sequence)
    -> (Sequence, Sequence, Sequence) -> Korvai
make_mohra smaps (a1, a2, a3) (b1, b2, b3) = mohra $ korvai1 adi smaps $ su $
      a123.b1 . a123.b1
    . a123.b2
    . a1.b2
    . a3.b3
    where a123 = a1.a2.a3

-- | Alternate melkalam and kirkalam.
make_mohra2 :: Korvai.StrokeMaps -> (Sequence, Sequence, Sequence)
    -> (Sequence, Sequence, Sequence) -> Korvai
make_mohra2 smaps (a1, a2, a3) (b1, b2, b3) = mohra $ korvai1 adi smaps $
      a123.b1 . su (a123.b1) . a123.b1 . su (a123.b1)
    . a123.b2 . su (a123.b2)
    . a1.b2 . su (a1.b2)
    . a3.b3 . su (a3.b3)
    where a123 = a1.a2.a3

c_mohra :: Korvai
c_mohra = ganesh $ make_mohra2 mridangam (a1, a2, a1) (b1, b2, b3)
    where
    a1 = dit.__4      .tang.__.kita.nakatiku
    a2 = na.ka.dit.__2.tang.__.kita.nakatiku
    b1 = ta.langa.din.__.tat.__.din.__.tat.__.dheem.__4
    b2 = ta.langa.dheem.__4
    b3 = tri_ (dheem.__4) (ta.langa.din.__.tat.__)
    mridangam = make_mridangam
        [ (dit, [k])
        , (tang.kita, [u, p, k])
        , (ta.langa, [p, u, k])
        , (din.tat, [o, k])
        , (dheem, [od])
        , (na.ka, [n, p])
        ]

c_mohra2 :: Korvai
c_mohra2 = janahan $ make_mohra2 mridangam (a1, a2, a3) (b1, b2, b3)
    where
    a_ = kita.ta.ka.nakatiku
    a1 = dit.__4.tang.__ . a_
    a2 = dit.__2.tang.__ . a_
    a3 = dit.tang . a_
    b1 = repeat 3 (ta.ga.ta.ga) . dhom.__4
    b2 = ta.ga.ta.ga . dhom.__4
    b3 = tri_ (dhom.__4) $ repeat 2 (ta.ga.ta.ga)
    mridangam = make_mridangam
        [ (dit, [t])
        , (tang, [o])
        , (kita.ta.ka, [k, t, p, k])
        , (ta.ga, [o, u])
        , (dhom, [o])
        ]

c_mohra_youtube :: Korvai
c_mohra_youtube = source "Melakkaveri Balaji" $ source url $
    make_mohra2 mridangam (a1, a2, a3) (b1, b2, b3)
    where
    url = "https://www.youtube.com/watch?v=eq-DZeJi8Sk"
    -- he says "tikutaka tarikita" instead of "nakatiku tarikita"
    a1 =  __.dhom.ta.ka.ta .__.ki.ta . nakatiku
    a2 =  ka.din.__.din.__. ta.ki.ta . nakatiku
    a3 =  ka.dhom.ta.ka.ta .__.ki.ta . nakatiku
    b1 = ta.ka . tang.__3.ga . tang.__3.ga . tang.__3.ga . tang.__
    b2 = ta.ka . tang.__3.ga.tang.__
    b3 = ta.ka . tri_ (tang.__.ki.ta.ta.ka) (tang.__3.ga.din.__)
    mridangam = make_mridangam
        [ (dhom.ta.ka.ta, [o, k, p, u])
        , (ki.ta, [p, k])
        , (ka.din.din, [p, i, i])
        , (ta.ki.ta, [k, t, k])
        , (ka, [k])
        , (ta.ka.tang, [n, o, od])
        , (ga.tang, [o, od])
        , (ga.din.tang, [o, od, u])
        , (ki.ta.ta.ka, [p, k, k, o])
        , (ga.din, [o, od])
        ]
