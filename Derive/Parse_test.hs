-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Parse_test where
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map

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
        , Call (Symbol "=") (map Literal [symbol "b", vnum 4])
        , Call (Symbol ".") (map Literal
            [VInstrument (Score.Instrument "inst"),
                VControl (LiteralControl "sig")])
        ]

    -- A toplevel symbol can have anything except =.
    equal (f "a|b\")") $ Right [Call (Symbol "a|b\")") []]

    -- Subcalls, however, use a close paren to delimit.
    equal (f "a (b) c") $
        Right [Call (Symbol "a") [val_call "b" [], Literal (symbol "c")]]
    equal (f "a (()") $
        Right [Call (Symbol "a") [val_call "(" []]]
    -- Unbalanced parens.
    left_like (f "a (b") "parse error"

    equal (f "") $ Right [Call (Symbol "") []]
    equal (f "--") $ Right [Call (Symbol "") []]
    equal (f "  a -- comment") $ Right [Call (Symbol "a") []]
    equal (f "a 'b -- c'--d") $ Right
        [Call (Symbol "a") [Literal (VSymbol (Symbol "b -- c"))]]

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
    let eq a b = Right (Call (Symbol "=") [Literal a, b])
        num = Literal . VNum . Score.untyped
    let f = ParseText.parse_all Parse.p_equal
    equal (f "a = b") (eq (symbol "a") (Literal (symbol "b")))
    equal (f "a=b") (eq (symbol "a") (Literal (symbol "b")))
    equal (f "a = 10") (eq (symbol "a") (num 10))
    equal (f "a = (b c)") (eq (symbol "a")
        (val_call "b" [Literal (symbol "c")]))
    equal (f "a] = 1") (eq (symbol "a]") (num 1))
    equal (f "a) = 1") (eq (symbol "a)") (num 1))
    equal (f ">a = 1") (eq (symbol ">a") (num 1))

    left_like (f "a = ()") "parse error on char 6"
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

val_call :: Text -> [Term] -> Term
val_call sym args = ValCall (Call (Symbol sym) args)

symbol :: Text -> TrackLang.Val
symbol = VSymbol . Symbol

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

test_parse_definition_file = do
    let f extract = either (Left . untxt) (Right . extract)
            . Parse.parse_definition_file
    left_like (f id "x:\na = b") "unknown sections: x"
    left_like (f id "val:\na = b\nc\n") "3: parse error"
    let text = "val:\n  a = b\n-- comment\n\nnote generator:\nn = t | x"
    let call sym = Call (Symbol sym) []
    equal (f Parse.def_val text) $ Right [("a", call "b" :| [])]
    equal (f (fst . Parse.def_note) text) $
        Right [("n", call "t" :| [call "x"])]

test_split_sections = do
    let f = either (Left . untxt) (Right . Map.toList) . Parse.split_sections
    equal (f "a:\n1\nb:\n2\na:\n3\n") $
        Right [("a", [(2, "1"), (6, "3")]), ("b", [(4, "2")])]
    left_like (f "1\na:\n2\n") "section without a header"
