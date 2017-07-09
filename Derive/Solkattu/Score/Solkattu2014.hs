-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
-- | Korvais expressed in "Derive.Solkattu.Dsl".
module Derive.Solkattu.Score.Solkattu2014 where
import Prelude hiding ((.), (^), repeat)
import qualified Data.List as List

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
    , reduce1 mempty . utarangam 4
    , reduce1 __2 . utarangam 3
    , reduce1 __3 . utarangam 2

    , expand1 mempty . utarangam 4
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
        join karv $ zipWith (.) [mempty, su tk, su tknk] seqs
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
