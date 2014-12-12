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
    let f mod token mtype =
            Hspp.macro_matches  (Just "func") mod token $ Hspp.Macro mtype "sym"
        qual = Hspp.Qualified ["Q"]
        unqual = Hspp.Unqualified ["Home"]
        both = Hspp.Both ["Home"] ["Q"]
    -- Qualified only matches exactly.
    equal (f "A.B" "Q.sym" qual) True
    equal (f "A.B" "B.sym" qual) False
    equal (f "A.B" "sym" qual) False
    -- Unqualified matches if you're not Home.
    equal (f "A.B" "sym" unqual) True
    equal (f "Home" "sym" unqual) False
    equal (f "A.B" "Q.sym" unqual) False
    -- But not in a function definition.
    equal (Hspp.macro_matches (Just "func") "A.B" "func"
            (Hspp.Macro unqual "func"))
        False

    -- Both matches unqualified at home, qualified when not.
    equal (f "A.B" "Q.sym" both) True
    equal (f "A.B" "sym" both) False
    equal (f "Home" "Q.sym" both) False
    equal (f "Home" "sym" both) True

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
