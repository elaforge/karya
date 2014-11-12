-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Parse_test where
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified System.Directory as Directory
import System.FilePath ((</>))

import Util.Control
import qualified Util.ParseText as ParseText
import Util.Test

import qualified Derive.Parse as Parse
import qualified Derive.Score as Score
import Derive.TestInstances ()
import qualified Derive.TrackLang as TrackLang
import Derive.TrackLang
       (ControlRef(..), Symbol(..), Val(..), Call(..), Term(..))

import qualified Perform.Signal as Signal


test_parse_expr = do
    let f = fmap NonEmpty.toList . Parse.parse_expr
        vnum = VNum . Score.untyped
    equal (f "a | b") $ Right
        [Call (Symbol "a") [], Call (Symbol "b") []]
    equal (f "a | b | c") $ Right $
        [Call (Symbol "a") [], Call (Symbol "b") [], Call (Symbol "c") []]
    -- Any word in call position is a symbol.
    equal (f "4") $ Right [Call (Symbol "4") []]
    equal (f "()") $ Right [Call (Symbol "()") []]
    equal (f "4 4") $ Right [Call (Symbol "4") [Literal (vnum 4)]]
    equal (f "4 (4)") $ Right [Call (Symbol "4") [val_call "4" []]]
    -- So the only way to have a null call is a null expression.
    equal (f "") $ Right [Call (Symbol "") []]

    equal (f "a") $ Right [Call (Symbol "a") []]
    equal (f "a 42") $ Right [Call (Symbol "a") [Literal (vnum 42)]]
    equal (f "a | ") $ Right [Call (Symbol "a") [], Call (Symbol "") []]

    equal (f "a | b = 4 | . >inst %sig") $ Right
        [ Call (Symbol "a") []
        , Call (Symbol "=") (map Literal [VSymbol "b", vnum 4])
        , Call (Symbol ".") (map Literal
            [VInstrument (Score.Instrument "inst"),
                VControl (LiteralControl "sig")])
        ]

    -- A toplevel symbol can have anything except =.
    equal (f "a|b\")") $ Right [Call (Symbol "a|b\")") []]

    -- Subcalls, however, use a close paren to delimit.
    equal (f "a (b) c") $
        Right [Call (Symbol "a") [val_call "b" [], Literal (VSymbol "c")]]
    equal (f "a (()") $
        Right [Call (Symbol "a") [val_call "(" []]]
    -- Unbalanced parens.
    left_like (f "a (b") "parse error"

    equal (f "") $ Right [Call (Symbol "") []]
    equal (f "--") $ Right [Call (Symbol "") []]
    equal (f "  a -- comment") $ Right [Call (Symbol "a") []]
    equal (f "a 'b -- c'--d") $ Right
        [Call (Symbol "a") [Literal (VSymbol (Symbol "b -- c"))]]

    equal (f "a;b") $ Right
        [Call "a" [Literal VSeparator, Literal (VSymbol "b")]]

test_parse_val = do
    let attrs = Just . VAttributes . Score.attrs
        sym = Just . VSymbol
    let invertible =
            [ (">", Just $ VInstrument (Score.Instrument ""))
            , (">fu/nny^*", Just $ VInstrument (Score.Instrument "fu/nny^*"))

            , ("0", Just (VNum (Score.untyped 0)))
            , ("0.", Nothing)
            , (".2", Just (VNum (Score.untyped 0.2)))
            , ("1c", Just (VNum (Score.Typed Score.Chromatic 1)))
            , ("-1d", Just (VNum (Score.Typed Score.Diatonic (-1))))
            , ("-.5d", Just (VNum (Score.Typed Score.Diatonic (-0.5))))
            , ("42nn", Just (VNum (Score.Typed Score.Nn 42)))
            , ("1q", Nothing)

            , ("+", attrs [])
            , ("+a", attrs ["a"])
            , ("+a+b", attrs ["a", "b"])

            , ("sym", sym "sym")
            , ("-sym", sym "-sym")
            , ("-", sym "-")
            , ("'space sym'", sym "space sym")
            , ("'23'", sym "23")
            , ("'quinn''s hat'", sym "quinn's hat")
            , ("s!$_", sym "s!$_")
            , ("'bad string", Nothing)

            , ("%", Just $ VControl $ LiteralControl "")
            , ("%sig", Just $ VControl $ LiteralControl "sig")
            , ("%sig,0", Just $ VControl $
                DefaultedControl "sig" (Score.untyped (Signal.constant 0)))
            , ("%sig,4s", Just $ VControl $
                DefaultedControl "sig"
                (Score.Typed Score.Real (Signal.constant 4)))
            , ("%sig,4q", Nothing)
            , ("%sig,", Nothing)

            , ("#", Just $ VPitchControl $ LiteralControl "")
            , ("#sig", Just $ VPitchControl $ LiteralControl "sig")

            , ("\"(a b)", Just $ VQuoted $ TrackLang.Quoted $
                Call (Symbol "a") [Literal (VSymbol (Symbol "b"))] :| [])
            , ("\"()", Just $ VQuoted $ TrackLang.Quoted $
                Call (Symbol "") [] :| [])
            , ("\"(a |)", Just $ VQuoted $ TrackLang.Quoted $
                Call (Symbol "a") [] :| [Call (Symbol "") []])

            , ("$bad", Nothing)
            , ("_", Just VNotGiven)
            , (";", Just VSeparator)
            ]
    let num = Just . VNum
    let noninvertible =
            [ ("3/2", num (Score.untyped 1.5))
            , ("-3/2", num (Score.untyped (-1.5)))
            , ("3/2d", num (Score.Typed Score.Diatonic 1.5))
            , ("0x00", num (Score.untyped 0))
            , ("0xff", num (Score.untyped 1))
            , ("-0xff", num (Score.untyped (-1)))
            ]
    let exprs = map ((,) True) invertible ++ map ((,) False) noninvertible
    forM_ exprs $ \(invertible, (expr, expected)) -> do
        let res = Parse.parse_val expr
        case (res, expected) of
            (Left err, Just expect) -> void $ failure $
                err ++ ", expected " ++ show expect
            (Right val, Nothing) -> void $ failure $
                "shouldn't have parsed: " ++ show expr ++ " -> " ++ show val
            (Right val, Just expect) -> do
                equal val expect
                when invertible $
                    void $ equal (TrackLang.show_val val) expr
            _ -> void $ success $ show res ++ " == " ++ show expected

test_parse_control_title = do
    let f = Parse.parse_control_title
    equal (f "*") $ Right ([VSymbol (Symbol "*")], [])
    equal (f "*a") $ Right ([VSymbol (Symbol "*a")], [])

test_parse_num = do
    let f = Parse.parse_num
    equal (f "`0x`00") (Right 0)
    equal (f "`0x`ff") (Right 1)
    equal (f "-`0x`ff") (Right (-1))
    left_like (f "`0x`000") "parse error"

test_p_equal = do
    let eq a b = Right (Call "=" [Literal a, b])
        num = Literal . VNum . Score.untyped
    let f = ParseText.parse Parse.p_equal
    equal (f "a = b") (eq (VSymbol "a") (Literal (VSymbol "b")))
    equal (f "a=b") (eq (VSymbol "a") (Literal (VSymbol "b")))
    equal (f "a = 10") (eq (VSymbol "a") (num 10))
    equal (f "a = (b c)") (eq (VSymbol "a")
        (val_call "b" [Literal (VSymbol "c")]))
    equal (f "a] = 1") (eq (VSymbol "a]") (num 1))
    equal (f "a) = 1") (eq (VSymbol "a)") (num 1))
    equal (f ">a = 1") (eq (VSymbol ">a") (num 1))
    equal (f "a = b c") $
        Right $ Call "=" $ map (Literal . VSymbol) ["a", "b", "c"]

    left_like (f "a = ()") "parse error"
    left_like (f "a=") "not enough input"

test_lex1 = do
    let f = Parse.lex1
    equal (f "a b c") $ ("a ", "b c")
    equal (f "(a b) c") $ ("(a b) ", "c")
    equal (f "(a (b)) c") $ ("(a (b)) ", "c")
    equal (f "(a ')' (x) b) c") $ ("(a ')' (x) b) ", "c")

    -- Incomplete parses get lexed.
    equal (f "1.") $ ("1.", "")
    equal (f "'hi") $ ("'hi", "")

val_call :: Symbol -> [Term] -> Term
val_call sym args = ValCall (Call sym args)

test_expand_macros = do
    let f = Parse.expand_macros (\s -> "(" <> s <> ")")
    equal (f "") (Right "")
    equal (f "hi") (Right "hi")
    left_like (f "hi @") "parse error"
    equal (f "hi @a-b") (Right "hi (a-b)")
    equal (f "hi @a b") (Right "hi (a) b")
    equal (f "hi (Just @a/b)") (Right "hi (Just (a/b))")
    equal (f "hi [@b0, @b1]") (Right "hi [(b0), (b1)]")
    -- Doesn't substitute macros inside quotes.
    equal (f "hi \"@a\" there") (Right "hi \"@a\" there")


-- * definitions file

test_load_ky = do
    let make_ky imports defs = unlines $
            ["import '" <> i <> "'" | i <- imports]
            ++ "note generator:" : [d <> " = z" | d <- defs]
    dir <- unique_tmp_dir "ky"
    let lib = dir </> "lib"
    Directory.createDirectory lib
    writeFile (lib </> "lib1") $ make_ky ["lib2"] ["lib1"]

    let run imports defs = do
            writeFile (dir </> "defs") (make_ky imports defs)
            either (Left . untxt) (Right . first extract) <$>
                Parse.load_ky [dir, lib] "defs"
        extract = map fst . fst . Parse.def_note
    v <- run ["z"] ["d1"]
    left_like v "imported file not found: z"
    v <- run ["lib1"] ["d1"]
    left_like v "imported file not found: lib2"

    writeFile (lib </> "lib2") $ make_ky [] ["lib2"]
    io_equal (run ["lib1"] ["d1"]) $
        Right (["d1", "lib1", "lib2"], ["defs", "lib1", "lib2"])

test_parse_ky = do
    let f extract = either (Left . untxt) (Right . extract) . Parse.parse_ky
        note = f (map (second NonEmpty.head) . fst . Parse.def_note . snd)
            . ("note generator:\n"<>)
    let sym = Literal . VSymbol
    left_like (f id "x:\na = b\n") "unknown sections: x"
    equal (note " --c\na = b\n\n") $
        Right [("a", Call "b" [])]
    equal (note "a = b\n --c\n\n c\nd = e\n  -- hi\n") $
        Right [("a", Call "b" [sym "c"]), ("d", Call "e" [])]
    equal (note "a = b\n c\n --comment\n\nd = e\n") $
        Right [("a", Call "b" [sym "c"]), ("d", Call "e" [])]
    left_like (note "a = b\nc\n") ""
    -- imports
    equal (f fst "import 'x' -- blah\nimport 'y'\n") $
        Right ["x", "y"]
    equal (f fst "import\n\t'x'\n") $ Right ["x"]
    left_like (f fst "blort x\nimport y\n") "expected eof"

test_split_sections = do
    let f = second Map.toList . Parse.split_sections
    equal (f "a:\n1\nb:\n2\na:\n3\n") $
        ("", [("a", [(2, "1"), (6, "3")]), ("b", [(4, "2")])])
    equal (f "import a\nimport b\na:\n2\n")
        ("import a\nimport b\n", [("a", [(4, "2")])])

test_p_definition = do
    let f = either (Left . untxt) Right
            . ParseText.parse_lines 1 Parse.p_definition
    equal (f "a =\n b\n c\n") $
        Right ("a", Call "b" [Literal (VSymbol "c")] :| [])
    left_like (f "a =\n b\nc = d\n") ""
    equal (f "a = n +z\n") $
        Right ("a", Call "n" [Literal (VAttributes (Score.attr "z"))] :| [])
