-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Ruler.Tala_test where
import qualified Data.List as List
import qualified Data.Text as Text

import qualified Cmd.Ruler.Tala as Tala
import qualified Ui.UiTest as UiTest

import           Util.Test


test_make_ruler :: Test
test_make_ruler = do
    let make zoom = UiTest.meter_marklist zoom
    -- TODO used to be at zoom 20, now is at 200
    equal (map snd $ make 200 (Tala.adi 1)) $
        Text.words "1 .1 .2 .3 .X .O .X .O 2"
    equal (map snd $ UiTest.meter_marklist 400 (Tala.adi3 1)) $
        List.intercalate ["..2", "..3"] $ map (:[]) $
        Text.words "1 .1 .2 .3 .X .O .X .O 2"
    let ranks = map (fst . snd) $ UiTest.meter_zoom 8000 (Tala.adi3 1)
    equal (filter (<=4) ranks)
        [ 0, 4, 4, 3, 4, 4, 3, 4, 4, 3, 4, 4
        , 2, 4, 4, 3, 4, 4, 2, 4, 4, 3, 4, 4
        , 0
        ]

{-
    1.0.1   1       0 Section, avartanam, anga
    1.0.2   ..2     4 nadai
    1.0.3   ..3     4 nadai
    1.1.1   .1      3 Quarter akshara
    1.1.2   ..2     4
    1.1.3   ..3     4
    1.2     .2      3 akshara
    1.3     .3      3 akshara
    1.X     .X      2 anga
    1.O     .O      3 akshara
    1.X     .X      2 anga
    1.O     .O      3 akshara
    2       2       1 Avartanam

-- * r_section is several avartanam -- sections
-- * r_1 avartanam -- avartanams
-- * r_2 anga -- based on tala
-- * r_4 akshara -- basad on akshara type and jati
-- * r_8 nadai / gati -- variable
-- . r_16 -- 2
-- * r_32 -- 2
-- . r_64 -- 2
-- * r_128 -- 2
-- . r_256 -- 2
-}
