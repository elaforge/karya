-- Copyright 2024 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
module Solkattu.Score.Solkattu2025 where
import           Prelude hiding ((.), (^), repeat)

import qualified Solkattu.Tala as Tala

import           Solkattu.Dsl.Solkattu


e_kanjira :: Korvai
e_kanjira = exercise $ date 2025 6 25 $ korvaiV Tala.any_beats mempty
    [ taka.dhom.__.taka.taka.dhom.__.taka.taka.taka.dhom
    , r2 $ dhom.ka.ta.ka.dhom.__4
    , r4 $ dit.thom.thom.dit -- takadinna, circular motion
    , dit.thom.thom.dit.su (dit.thom).thom.thom.dit
    -- thom like open ta
    , thom.dhom.thom.dhom.taka.taka -- like nakanakadin
    -- repeated strokes move slightly up and in
    , dhom.ka.dhom.ka.ka.dhom.dhom.ka
    , dhom.ka.dhom.dhom.ka.ka.ta.dit.ta.dit.dhom.dhom.ka.dhom.dhom.ka
    --   ^            ^                             ^
    ]
    -- TODO can't do hv on sollus!

e_kanjira_patterns :: Korvai
e_kanjira_patterns = exercise $ date 2025 6 25 $ korvaiV Tala.any_beats mempty
    [ r3 $ ka.dit.dit.ka.dhom
    , r3 $ ka.dit.__.dit.ka.dhom
    , r3 $ ka.__.dit.__.dit.ka.dhom
    , r3 $ ka.dit.__.ka.dit.dit.ka.dhom
    , r3 $ ka.__.dit.__.ka.dit.dit.ka.dhom
    ]

-- also sequences in Solkattu2013

k_tatdit :: Korvai
k_tatdit = date 2025 7 14 $ korvaiV adi mridangam
    [ tri_ "dim___" "tat_dit_takadinna"
    , __D 2 . reduce3 2 "dim__" "tat_dit_takadinna"
    -- , tri123 "tat_dit_takadinna" "dit_takadinna" "takadinna"
    , reduceTo 5 2 "tat_dit_takadinna dim__"
    , reduceTo 7 2 "tat_dit_takadinna dim__"
        . r3 p5 . "_tatdin__"
        . r3 p6 . "_tat_din__"
        . r3 p7 . "_tat__".din
    , reduceTo 5 2 "tat_dit_takadinna dim__"
        -- . p5
        . r3 p5 . r3 "tatdin__"
        . r3 p6 . r3 "tat_din__"
        . r3 p7 . r3 "tat__din__" -- din -- minus 5?
    , reduceTo 5 2 theme2 . tri_ "dim___taka" "tadikita takadinna"
    ]
    where
    theme2 = "tat_dit_takadinnadin_tat_dim__"
    mridangam = makeMridangam
        [ (tat.dit, k.t)
        , (dim, od)
        , (tat.din, k.od)
        , (din.tat.dim, od.k.od)
        , ("tadikita", k.t.p.k)
        , (ta.ka, p.k)
        ]

{-
  - dom ka dom dom ka ka ta dit ta dit dom dom ka dom dom ka
    ^          ^                           ^
  dhom ka dhom ka ka dhom dhom ka - repeated strokes move slightly up and to
  right

  tom dom tom dom ta ka ta ka
  gum after dhom
  takadinna - dit thom thom dit -- circular motion
            - dit thom thom dit ditthom thom thom dit
  fingers slightly curved
  dhom ka ta ka dhom
-}
