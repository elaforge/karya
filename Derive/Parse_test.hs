-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Parse_test where
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified System.Directory as Directory
import System.FilePath ((</>))

import qualified Util.ParseText as ParseText
import Util.Test
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Parse as Parse
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import Derive.TestInstances ()
import Derive.BaseTypes (Ref(..), Symbol(..), Val(..), Call(..), Term(..))

import qualified Perform.Signal as Signal
import Global


test_parse_expr = do
    let f = first untxt . fmap NonEmpty.toList . Parse.parse_expr
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
                VControlRef (LiteralControl "sig")])
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

test_unparsed_call = do
    let f = fmap NonEmpty.toList . Parse.parse_expr
        call = BaseTypes.Call
        vsym = Literal . BaseTypes.VSymbol
        null_call = BaseTypes.Call "" []
    equal (f "!<>\"('|") $ Right
        [call Parse.unparsed_call [vsym "<>\"('"], null_call]
    equal (f "hi \"(!blah)") $ Right
        [call "hi" [BaseTypes.Literal $ BaseTypes.VQuoted $
            BaseTypes.Quoted (call "!" [vsym "blah"] :| [])]]
    -- ! takes precedence over =
    equal (f "!a=b") $ Right [call "!" [vsym "a=b"]]

test_parse_val = do
    let attrs = Just . VAttributes . Score.attrs
        sym = Just . VSymbol
    let invertible =
            [ (">", Just $ VInstrument (Score.Instrument ""))
            , (">name-b", Just $ VInstrument (Score.Instrument "name-b"))
            , (">no/slash", Nothing)
            , (">fu/nny^*", Nothing)

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

            , ("%", Just $ VControlRef $ LiteralControl "")
            , ("%sig", Just $ VControlRef $ LiteralControl "sig")
            , ("%sig,0", Just $ VControlRef $
                DefaultedControl "sig" (Score.untyped (Signal.constant 0)))
            , ("%sig,4s", Just $ VControlRef $
                DefaultedControl "sig"
                (Score.Typed Score.Real (Signal.constant 4)))
            , ("%sig,4q", Nothing)
            , ("%sig,", Nothing)

            , ("#", Just $ VPControlRef $ LiteralControl "")
            , ("#sig", Just $ VPControlRef $ LiteralControl "sig")

            , ("\"(a b)", Just $ VQuoted $ BaseTypes.Quoted $
                Call (Symbol "a") [Literal (VSymbol (Symbol "b"))] :| [])
            , ("\"()", Just $ VQuoted $ BaseTypes.Quoted $
                Call (Symbol "") [] :| [])
            , ("\"(a |)", Just $ VQuoted $ BaseTypes.Quoted $
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
                untxt err ++ ", expected " ++ show expect
            (Right val, Nothing) -> void $ failure $
                "shouldn't have parsed: " ++ show expr ++ " -> " ++ show val
            (Right val, Just expect) -> do
                equal val expect
                when invertible $
                    void $ equal (ShowVal.show_val val) expr
            _ -> void $ success $ show res ++ " == " ++ show expected

test_parse_control_title = do
    let f = Parse.parse_control_title
    equal (f "*") $ Right ([VSymbol (Symbol "*")], [])
    equal (f "*a") $ Right ([VSymbol (Symbol "*a")], [])

test_parse_num = do
    let f = first untxt . Parse.parse_num
    equal (f "`0x`00") (Right 0)
    equal (f "`0x`ff") (Right 1)
    equal (f "-`0x`ff") (Right (-1))
    left_like (f "`0x`000") "parse error"

test_p_equal = do
    let eq a b = Right (Call "=" [Literal a, b])
        num = Literal . VNum . Score.untyped
    let f = first untxt . ParseText.parse Parse.p_equal
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
    -- Quotes let you put '=' in the assignee.
    equal (f "'=>' = 1") (eq (VSymbol "=>") (num 1))

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
    let f = first untxt . Parse.expand_macros (\s -> "(" <> s <> ")")
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
    writeFile (lib </> "lib1") $ make_ky ["lib2"] ["lib1-call"]

    let write imports =
            writeFile (dir </> "defs") (make_ky imports ["defs-call"])
    let load = (untxt *** first extract) <$> Parse.load_ky [dir, lib] "defs"
        extract = map (fst . snd) . fst . Parse.def_note
    write ["z1"]
    v <- load
    left_like v "ky file not found: z"

    write ["lib1"]
    v <- load
    left_like v "ky file not found: lib2"

    writeFile (lib </> "lib2") $ make_ky [] ["lib2-call"]
    [lib1, lib2, defs] <- mapM Directory.getModificationTime
        [lib </> "lib1", lib </> "lib2", dir </> "defs"]
    io_equal load $
        Right (["defs-call", "lib1-call", "lib2-call"],
            [ (dir </> "defs", defs)
            , (lib </> "lib1", lib1)
            , (lib </> "lib2", lib2)
            ])

test_parse_ky = do
    let f extract = (untxt *** extract) . Parse.parse_ky "fname.ky"
        note = f (map (second NonEmpty.head . snd) . fst . Parse.def_note . snd)
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
    let f = first untxt . ParseText.parse_lines 1 Parse.p_definition
    equal (f "a =\n b\n c\n") $
        Right ("a", Call "b" [Literal (VSymbol "c")] :| [])
    left_like (f "a =\n b\nc = d\n") ""
    equal (f "a = n +z\n") $
        Right ("a", Call "n" [Literal (VAttributes (Score.attr "z"))] :| [])
