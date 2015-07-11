-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.India.Gamakam3_test where
import Util.Test
import qualified Derive.Call.India.Gamakam3 as Gamakam
import qualified Derive.DeriveTest as DeriveTest
import Global


test_parse_sequence = do
    let f = fmap (map (first Gamakam.gamakam_name)) . Gamakam.parse_sequence
    equal (f "#012 -4") $ Right [('0', ""), ('1', ""), ('2', ""), ('-', "4")]
    equal (f "#56u") $ Right [('5', ""), ('6', ""), ('-', "1"), ('1', "")]
    equal (f "#!12(") $ Left "not found: '!', not found: '('"

test_sequence = do
    let run gamakam = DeriveTest.extract extract $ DeriveTest.derive_tracks
            "import india.gamakam3"
            -- TODO use a pitch track above until I get next pitch figured out
            [ ("*", [(0, 0, "4c"), (4, 0, "4d"), (10, 0, "4e")])
            , (">", [(0, 4, ""), (4, 6, ""), (10, 2, "")])
            , ("* | transition=1",
                [(0, 4, "4c"), (4, 6, gamakam), (10, 0, "4e")])
            ]
        extract = DeriveTest.e_nns_rounded
        output nns = ([[(0, 60)], nns, [(10, 64)]], [])

    -- 4 5 6 7 8 9 10
    -- ------++++++
    equal (run "##-") (output [(4, 62)])
    -- Twice because of slicing.
    strings_like (snd $ run "# 0nn")
        ["too many arguments: nn", "too many arguments: nn"]

    -- transition=1 takes all the time, and winds up being linear.
    equal (run "transition=1 | ##01")
        (output [(4, 62), (7, 62), (8, 62.67), (9, 63.33)])
    -- Fastest transition.
    equal (run "transition=0 | ##01")
        (output [(4, 62), (7, 62), (8, 62.13), (9, 63.87)])

    -- 4 5 6 7 8 9 10
    -- ----++++----
    equal (run "##010")
        (output [(4, 62), (6, 62), (7, 63), (8, 64)])
    equal (run "##0a0")
        (output [(4, 62), (6, 62), (7, 61), (8, 60)])

    -- move_absolute
    -- Move to next pitch.
    equal (run "##-d-")
        (output [(4, 62), (6, 62), (7, 63), (8, 64)])
    -- Prev to current.
    equal (run "##<-c-")
        (output [(4, 60), (6, 60), (7, 61), (8, 62)])
