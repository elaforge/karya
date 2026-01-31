-- Copyright 2025 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
module Solkattu.Score.Kanjira2025 where
import           Prelude hiding ((.))

import qualified Solkattu.Score.Mridangam2013 as Mridangam2013

import           Solkattu.Dsl.Mridangam


e_sarvalaghu :: Korvai
e_sarvalaghu = date 2025 9 20 $ ganesh $ korvaiV adi
    [ "k_o_o_o_tko_o_o_" . "k_o_o_o___o_o_o_" -- nddn
    -- nd_n,nd,n,nd,nxq
    , r2 $ "ook" . r2 "okook" . su "o_tktk"
    ]

ta_dit_takadin :: Korvai
ta_dit_takadin = date 2025 9 20 $ ganesh $ korvaiV adi
    [ sarvaD_ 6 . __M 2 . r 0
    , sarvaD_ 6 . __M 3 . r 1
    , sarvaD_ 6 . __M 4 . r 2
    , sarvaD_ 6 . __M 5 . r 3
    , sarvaD_ 6 . __M 6 . r 4
    , sarvaD_ 2 . __M 2 . r 0 . sarvaD_ 2 .__M 3 . r 1
        . sarvaD_ 2 . __M 4 . r 2 . sarvaD_ 2 . __M 5 . r 3
    , __M 2 . r 0 . __M 3 . r 1 . __M 4 . r 2 . r2 (__ . r 3)
    , trin (o.__) (mconcat reduction) (mconcat reduction)
        (mconcat (reverse reduction))
    ]
    where
    r = (reduction !!)
    reduction = map g $ map su
        ["k_t_k_k_tko_", "k_t_k_tko_", "k_k_tko_", "k_tko_", "tko_"]

dinnaginna1 :: Korvai
dinnaginna1 = korvaiS adi
    -- TODO suppress the t -> k technique!
    [ mconcat $ Mridangam2013.make_dinna
        "tktko_tko_k_tktk" "" o ("o_kto_", "k_tktk") ("", "")
    ]

-- manual version
dinnaginna2 :: Korvai
dinnaginna2 = date 2025 9 20 $ ganesh $ korvaiV adi
    [ sarvaD_ 6 . theme
    , "oko_o_o_".su "tktk"."o_o_o_"."k_o_o_o_" . theme
    , "k_o_o_o_" . theme . "k_o_o_o_" . theme
    , tri (o.__4) theme
    , theme.o.__4 . theme.r2 (o.__.dropM 4 theme)
    , theme.o.__4 . theme.o.__ . dropM 4 theme.r2 (o.dropM 6 theme)
    , tri o (theme.me)
    , trin o theme (theme.me) (theme.me.me)
    ]
    where
    theme = g $ su "tktko_tko_k_tktk"
    me = g $ su "tktk"

{-
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

e_sarvalaghu :: Korvai
e_sarvalaghu = date 2025 9 20 $ ganesh $ korvaiV adi mempty
    [ "ta_din_din_din_takadin_din_din_" . "ta_din_din_din___din_din_din_"
    ]
-}

{-
    exercise: ki_din_takadin_
    maximize contact, curve fingers outwards

-}

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
