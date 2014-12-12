-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Util.Hspp_test where
import qualified Util.Hspp as Hspp
import Util.Test


test_lex_dot = do
    let f = Hspp.lex_dot
    equal (f " $(hi there) fred") [("$(hi there)", " fred")]
    equal (f "$(hi (f a a) (f b)) there") [("$(hi (f a a) (f b))", " there")]
    equal (f " A.b c") [("A.b", " c")]

test_macro_matches = do
    let f mod token macro_mod quals =
            Hspp.macro_matches  (Just "qq") mod token $
                Hspp.Macro macro_mod quals "f"
    -- If module isn't set, look for 'f'.
    equal (not (f "A.B" "B.f" Nothing [])) True
    equal (f "A.B" "f" Nothing []) True

    -- In another module, look for 'B.f'
    equal (f "Q" "B.f" (Just "A.B") ["B"]) True
    equal (f "Q" "f" (Just "A.B") ["B"]) False
    -- or C.f
    equal (f "Q" "C.f" (Just "A.B") ["B", "C"]) True
    equal (f "Q" "D.f" (Just "A.B") ["B", "C"]) False

    -- In same module, look for 'f'
    equal (f "A.B" "f" (Just "A.B") ["A"]) True
    equal (f "A.B" "B.f" (Just "A.B") ["A"]) False

test_annotate = do
    let expected =
            [ ("module X (", (Nothing, False, False))
            , ("foo", (Nothing, False, False))
            , (") where", (Nothing, False, True))
            , ("foo = bar", (Just "foo", False, True))
            , ("bar, buz :: Baz", (Just "foo", True, True))
            , ("bar a b = a b", (Just "bar", False, True))
            , ("x = 10", (Just "x", False, True))
            ]
        extract (_, Hspp.Annotation _ func decl post) = (func, decl, post)
    equal (map extract (Hspp.annotate (map fst expected)))
        (map snd expected)

test_smart_lines = do
    let f = Hspp.smart_lines
    equal (f "1\n2") ["1", "2"]
    equal (f "1 \"hello\\\n  \\there\" f\n2\n")
        ["1 \"hello\\\n  \\there\" f", "2"]
