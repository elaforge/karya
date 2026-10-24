-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.ParseTitle_test where
import qualified Util.ParseA as ParseA
import qualified Derive.ParseTitle as ParseTitle
import           Derive.ParseTitle (ControlType(..))
import qualified Derive.ScoreT as ScoreT

import           Util.Test


test_parse_control_title :: Test
test_parse_control_title = do
    let f = ParseTitle.parseControlTitle
    pprint (f "a | b")

test_p_tempo :: Test
test_p_tempo = do
    let f = ParseA.parse1 ParseTitle.pControlType
    equal (f "tempo") $ Right (Tempo Nothing)
    equal (f "tempo blah  ") $ Right (Tempo (Just "blah"))

test_p_pitch :: Test
test_p_pitch = do
    let f = ParseA.parse1 ParseTitle.pControlType
    equal (f "*") $ Right (Pitch "" (Right ""))
    equal (f "*hi") $ Right (Pitch "hi" (Right ""))
    equal (f "*hi #pc") $ Right (Pitch "hi" (Right "pc"))
    equal (f "*hi !tc") $ Right (Pitch "hi" (Left "tc"))

test_p_control :: Test
test_p_control = do
    let f = ParseA.parse1 ParseTitle.pControlType
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
    leftLike (f "c:z add") "parse error"

test_parse_unparse_control :: Test
test_parse_unparse_control = do
    let f = fmap ParseTitle.controlTypeToTitle
            . ParseTitle.parseControlType
        roundTrip text = (f text, Right text)
    uncurry equal (roundTrip "*")
    uncurry equal (roundTrip "*scale")
    equal (f "*scale #") (Right "*scale")
    uncurry equal (roundTrip "*scale #name")
    uncurry equal (roundTrip "tempo")
    uncurry equal (roundTrip "a-b")
    uncurry equal (roundTrip "a-b add")
    uncurry equal (roundTrip "c")
    uncurry equal (roundTrip "c:d")
    uncurry equal (roundTrip "c:d add")
    uncurry equal (roundTrip "!tc add")
    leftLike (f "c:q") "parse error"
    uncurry equal (roundTrip "%")
    uncurry equal (roundTrip "% add")
    leftLike (f "$ bad") "parse error"
    leftLike (f "a b c") "parse error"

test_parse_unparse_note :: Test
test_parse_unparse_note = do
    let f = fmap ParseTitle.unparseNote . ParseTitle.parseNote
    equal (f ">") (Right ">")
    equal (f ">x | abc") (Right ">x | abc")
