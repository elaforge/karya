-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Parse_test where
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import qualified System.Directory as Directory
import System.FilePath ((</>))

import qualified Util.ParseText as ParseText
import Util.Test
import qualified Util.Testing as Testing

import qualified Derive.Attrs as Attrs
import qualified Derive.BaseTypes as BaseTypes
import Derive.BaseTypes (Ref(..), Val(..))
import Derive.Expr (Call(..), Term(..))
import qualified Derive.Expr as Expr
import qualified Derive.Parse as Parse
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import Derive.TestInstances ()

import qualified Perform.Signal as Signal
import Global


test_parse_expr = do
    let f = fmap NonEmpty.toList . Parse.parse_expr
        vnum = VNum . Score.untyped
    equal (f "a | b") $ Right [Call "a" [], Call "b" []]
    equal (f "a | b | c") $ Right $ [Call "a" [], Call "b" [], Call "c" []]
    -- Any word in call position is a symbol.
    equal (f "4") $ Right [Call "4" []]
    equal (f "()") $ Right [Call "()" []]
    equal (f "4 4") $ Right [Call "4" [Literal (vnum 4)]]
    equal (f "4 (4)") $ Right [Call "4" [Expr.val_call "4" []]]
    -- So the only way to have a null call is a null expression.
    equal (f "") $ Right [Call "" []]

    equal (f "a") $ Right [Call "a" []]
    equal (f "a 42") $ Right [Call "a" [Literal (vnum 42)]]
    equal (f "a | ") $ Right [Call "a" [], Call "" []]

    equal (f "a | b = 4 | . sym %sig") $ Right
        [ Call "a" []
        , Call "=" (map Literal [VStr "b", vnum 4])
        , Call "." (map Literal
            [VStr "sym", VControlRef (LiteralControl "sig")])
        ]

    -- A toplevel symbol can have anything except =.
    equal (f "a|b\")") $ Right [Call "a|b\")" []]

    -- Subcalls, however, use a close paren to delimit.
    equal (f "a (b) c") $
        Right [Call "a" [Expr.val_call "b" [], Literal (VStr "c")]]
    equal (f "a (()") $
        Right [Call "a" [Expr.val_call "(" []]]
    -- Unbalanced parens.
    left_like (f "a (b") "parse error"

    equal (f "") $ Right [Call "" []]
    equal (f "--") $ Right [Call "" []]
    equal (f "  a -- comment") $ Right [Call "a" []]
    equal (f "a 'b -- c'--d") $ Right [Call "a" [Literal (VStr "b -- c")]]

    equal (f "a;b") $ Right
        [Call "a" [Literal VSeparator, Literal (VStr "b")]]

test_unparsed_call = do
    let f = fmap NonEmpty.toList . Parse.parse_expr
        vsym = Literal . BaseTypes.VStr
        null_call = Call "" []
    equal (f "!<>\"('|") $ Right
        [Call Parse.unparsed_call [vsym "<>\"('"], null_call]
    equal (f "hi \"(!blah)") $ Right
        [Call "hi" [Literal $ BaseTypes.VQuoted $
            BaseTypes.Quoted (Call "!" [vsym "blah"] :| [])]]
    -- ! takes precedence over =
    equal (f "!a=b") $ Right [Call "!" [vsym "a=b"]]
    -- But comments are still comments.
    equal (f "!'a' -- b") $ Right [Call "!" [vsym "'a'"]]

-- | Vals whose 'ShowVal.show_val' is the inverse of 'Parse.parse_val'.
invertible_vals :: [(Text, Maybe Val)]
invertible_vals =
    [ ("0", Just (VNum (Score.untyped 0)))
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

    , ("sym", str "sym")
    , ("-sym", str "-sym")
    , ("-", str "-")
    , ("'space sym'", str "space sym")
    , ("'23'", str "23")
    , ("'quinn''s hat'", str "quinn's hat")
    , ("s!$_", str "s!$_")
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
        Call (Expr.Symbol "a") [Literal (VStr "b")] :| [])
    , ("\"()", Just $ VQuoted $ BaseTypes.Quoted $
        Call (Expr.Symbol "") [] :| [])
    , ("\"(a |)", Just $ VQuoted $ BaseTypes.Quoted $
        Call "a" [] :| [Call "" []])

    , ("$bad", Nothing)
    , ("_", Just VNotGiven)
    , (";", Just VSeparator)
    ]
    where
    attrs = Just . VAttributes . Attrs.attrs
    str = Just . VStr

-- | Vals whose 'ShowVal.show_val' doesn't reproduce the parsed val.
noninvertible_vals :: [(Text, Maybe Val)]
noninvertible_vals =
    [ ("3/2", num (Score.untyped 1.5))
    , ("-3/2", num (Score.untyped (-1.5)))
    , ("3/2d", num (Score.Typed Score.Diatonic 1.5))
    , ("0x00", num (Score.untyped 0))
    , ("0xff", num (Score.untyped 1))
    , ("-0xff", num (Score.untyped (-1)))
    ]
    where num = Just . VNum

test_parse_val = do
    let exprs = map ((,) True) invertible_vals
            ++ map ((,) False) noninvertible_vals
    forM_ exprs $ \(invertible, (expr, expected)) -> do
        let res = Parse.parse_val expr
        case (res, expected) of
            (Left err, Just expect) -> void $ failure $
                err <> ", expected " <> showt expect
            (Right val, Nothing) -> void $ failure $
                "shouldn't have parsed: " <> showt expr <> " -> " <> showt val
            (Right val, Just expect) -> do
                equal val expect
                when invertible $
                    void $ equal (ShowVal.show_val val) expr
            _ -> void $ success $ showt res <> " == " <> showt expected

test_parse_num = do
    let f = Parse.parse_num
    equal (f "`0x`00") (Right 0)
    equal (f "`0x`ff") (Right 1)
    equal (f "-`0x`ff") (Right (-1))
    left_like (f "`0x`000") "parse error"

test_p_equal = do
    let eq a b = Right (Call "=" [Literal (VStr a), b])
        num = Literal . VNum . Score.untyped
    let f = ParseText.parse Parse.p_equal
    equal (f "a = b") (eq "a" (Literal (VStr "b")))
    equal (f "a=b") (eq "a" (Literal (VStr "b")))
    equal (f "a = 10") (eq "a" (num 10))
    equal (f "a = (b c)") (eq "a" (Expr.val_call "b" [VStr "c"]))
    equal (f "a] = 1") (eq "a]" (num 1))
    equal (f "a) = 1") (eq "a)" (num 1))
    equal (f ">a = 1") (eq ">a" (num 1))
    -- Quotes let you put '=' in the assignee.
    equal (f "'=>' = 1") (eq "=>" (num 1))
    equal (f ".-i = t") (eq ".-i" (Literal (VStr "t")))

    let eq_syms = Call "=" . map (Literal . VStr)
    equal (f "a = b c") $ Right (eq_syms ["a", "b", "c"])
    equal (f "i=+a") $ Right (eq_syms ["i", "a", "+"])
    equal (f "i =< a b") $ Right (eq_syms ["i", "a", "b", "<"])

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

    -- Quoted macros.
    equal (f "hi @\"a b\" there") (Right "hi (a b) there")
    -- It strips the \, because 'show' will later add it back.
    equal (f "hi @\"a\\\"b\" there") (Right "hi (a\"b) there")


-- * ky file

test_load_ky = do
    let make_ky imports defs = unlines $
            ["import '" <> i <> "'" | i <- imports]
            ++ "note generator:" : [d <> " = z" | d <- defs]
    dir <- Testing.unique_tmp_dir "ky"
    let lib = dir </> "lib"
    Directory.createDirectory lib
    writeFile (lib </> "lib1") $ make_ky ["lib2"] ["lib1-call"]

    let write imports =
            writeFile (dir </> "defs") (make_ky imports ["defs-call"])
    let load = (untxt *** first extract)
            <$> Parse.load_ky [dir, lib] "import 'defs'"
        extract = map (fst . snd) . fst . Parse.def_note
    write ["z1"]
    v <- load
    left_like v "ky file not found: z"

    write ["lib1"]
    v <- load
    left_like v "ky file not found: lib2"

    writeFile (lib </> "lib2") $ make_ky [] ["lib2-call"]
    [lib1, lib2, defs] <- mapM Text.IO.readFile
        [lib </> "lib1", lib </> "lib2", dir </> "defs"]
    io_equal load $ Right
        ( ["defs-call", "lib1-call", "lib2-call"]
        , [ ("", "import 'defs'")
          , (dir </> "defs", defs)
          , (lib </> "lib1", lib1)
          , (lib </> "lib2", lib2)
          ]
        )

e_expr :: Parse.Expr -> [(Expr.Symbol, [Text])]
e_expr (Parse.Expr (call :| calls)) = e_call call : map e_call calls
    where e_call (Parse.Call sym terms) = (sym, map ShowVal.show_val terms)

test_parse_ky = do
    let f extract = (untxt *** extract) . Parse.parse_ky "fname.ky"
        note = f e_note . ("note generator:\n"<>)
        e_note = map (second (head . e_expr) . snd) . fst . Parse.def_note . snd
    left_like (f id "x:\na = b\n") "unknown sections: x"
    equal (note " --c\na = b\n\n") $
        Right [("a", ("b", []))]
    equal (note "a = b\n --c\n\n c\nd = e\n  -- hi\n") $
        Right [("a", ("b", ["c"])), ("d", ("e", []))]
    equal (note "a = b\n c\n --comment\n\nd = e\n") $
        Right [("a", ("b", ["c"])), ("d", ("e", []))]
    left_like (note "a = b\nc\n") ""
    equal (note "a = b $c") $ Right [("a", ("b", ["$c"]))]
    equal (f e_note "-- hi") (Right [])
    equal (f e_note "-- note_generator:") (Right [])
    equal (note "a = c =+ 1\n") $ Right [("a", ("=", ["c", "1", "'+'"]))]

    -- imports
    equal (f fst "import 'x' -- blah\nimport 'y'\n") $
        Right ["x", "y"]
    equal (f fst "import\n\t'x'\n") $ Right ["x"]
    left_like (f fst "blort x\nimport y\n") "expected eof"

    let aliases = Parse.def_aliases . snd
    equal (f aliases "alias:\na = b\n")
        (Right [(Score.Instrument "a", Score.Instrument "b")])

test_split_sections = do
    let f = second Map.toList . Parse.split_sections . Text.lines
    equal (f "a:\n1\nb:\n2\na:\n3\n") $
        ("", [("a", [(2, "1"), (6, "3")]), ("b", [(4, "2")])])
    equal (f "import a\nimport b\na:\n2\n")
        ("import a\nimport b\n", [("a", [(4, "2")])])

test_p_definition = do
    let f = (untxt *** second e_expr)
            . ParseText.parse_lines 1 Parse.p_definition
    equal (f "a =\n b\n c\n") (Right ("a", [("b", ["c"])]))
    left_like (f "a =\n b\nc = d\n") ""
    equal (f "a = n +z\n") (Right ("a", [("n", ["+z"])]))
    equal (f "a = b $c") (Right ("a", [("b", ["$c"])]))
