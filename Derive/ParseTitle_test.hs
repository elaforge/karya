-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.ParseTitle_test where
import qualified Util.ParseText as ParseText
import qualified Derive.ParseTitle as ParseTitle
import           Derive.ParseTitle (ControlType(..))
import qualified Derive.ScoreT as ScoreT

import           Util.Test


test_parse_control_title :: Test
test_parse_control_title = do
    let f = ParseTitle.parse_control_title
    pprint (f "a | b")

test_p_tempo :: Test
test_p_tempo = do
    let f = ParseText.parse1 ParseTitle.p_control_type
    equal (f "tempo") $ Right (Tempo Nothing)
    equal (f "tempo blah  ") $ Right (Tempo (Just "blah"))

test_p_pitch :: Test
test_p_pitch = do
    let f = ParseText.parse1 ParseTitle.p_control_type
    equal (f "*") $ Right (Pitch "" (Right ""))
    equal (f "*hi") $ Right (Pitch "hi" (Right ""))
    equal (f "*hi #pc") $ Right (Pitch "hi" (Right "pc"))
    equal (f "*hi !tc") $ Right (Pitch "hi" (Left "tc"))

test_p_control :: Test
test_p_control = do
    let f = ParseText.parse1 ParseTitle.p_control_type
    equal (f "c") $ Right $ Control (Right (ScoreT.untyped "c")) Nothing
    equal (f "c:nn") $ Right $
        Control (Right (ScoreT.Typed ScoreT.Nn "c")) Nothing
    equal (f "%") $ Right $ Control (Right (ScoreT.untyped "")) Nothing
    equal (f "!tc") $ Right $ Control (Left "tc") Nothing
    equal (f "!tc add") $ Right $ Control (Left "tc") (Just "add")
    equal (f "c add") $ Right $ Control (Right (ScoreT.untyped "c"))
        (Just "add")
    equal (f "c:nn add") $ Right $
        Control (Right (ScoreT.Typed ScoreT.Nn "c")) (Just "add")
    left_like (f "c:z add") "parse error"

test_parse_unparse_control :: Test
test_parse_unparse_control = do
    let f = fmap ParseTitle.control_type_to_title
            . ParseTitle.parse_control_type
        round_trip text = (f text, Right text)
    uncurry equal (round_trip "*")
    uncurry equal (round_trip "*scale")
    equal (f "*scale #") (Right "*scale")
    uncurry equal (round_trip "*scale #name")
    uncurry equal (round_trip "tempo")
    uncurry equal (round_trip "a-b")
    uncurry equal (round_trip "a-b add")
    uncurry equal (round_trip "c")
    uncurry equal (round_trip "c:d")
    uncurry equal (round_trip "c:d add")
    uncurry equal (round_trip "!tc add")
    left_like (f "c:q") "parse error"
    uncurry equal (round_trip "%")
    uncurry equal (round_trip "% add")
    left_like (f "$ bad") "parse error"
    left_like (f "a b c") "parse error"

test_parse_unparse_note :: Test
test_parse_unparse_note = do
    let f = fmap ParseTitle.unparse_note . ParseTitle.parse_note
    equal (f ">") (Right ">")
    equal (f ">x | abc") (Right ">x | abc")
