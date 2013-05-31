-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Util.Hspp_test where
import qualified Data.Maybe as Maybe

import qualified Util.Hspp as Hspp
import Util.Test


test_lex_dot = do
    let f = Hspp.lex_dot
    equal (f " $(hi there) fred") [("$(hi there)", " fred")]
    equal (f "$(hi (f a a) (f b)) there") [("$(hi (f a a) (f b))", " there")]
    equal (f " A.b c") [("A.b", " c")]

test_find_macro = do
    let f mod token macro_mod quals = Maybe.isJust $
            Hspp.find_macro [Hspp.SrcposMacro macro_mod quals "f"] (Just "qq")
                mod token
    -- If module isn't set, look for 'f'.
    check (not (f "A.B" "B.f" Nothing []))
    check (f "A.B" "f" Nothing [])

    -- In another module, look for 'B.f'
    check (f "Q" "B.f" (Just "A.B") ["B"])
    check (not (f "Q" "f" (Just "A.B") ["B"]))
    -- or C.f
    check (f "Q" "C.f" (Just "A.B") ["B", "C"])
    check (not (f "Q" "D.f" (Just "A.B") ["B", "C"]))

    -- In same module, look for 'f'
    check (f "A.B" "f" (Just "A.B") ["A"])
    check (not (f "A.B" "B.f" (Just "A.B") ["A"]))

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
