-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.ParseBs_test where
import qualified Data.List.NonEmpty as NonEmpty

import Util.Control
import qualified Util.ParseBs as Util.Parse
import Util.Test

import qualified Derive.ParseBs as Parse
import qualified Derive.Score as Score
import Derive.TestInstances ()
import qualified Derive.TrackLang as TrackLang
import Derive.TrackLang
       (ControlRef(..), Symbol(..), ValType(..), Call(..), Term(..))

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

    -- Symbols can have anything in them as long as they start with a letter.
    equal (f "a|b=4") $ Right [Call (Symbol "a|b=4") []]

    -- Except a close paren, for subcalls.
    equal (f "a (b) c") $
        Right [Call (Symbol "a") [val_call "b" [], Literal (symbol "c")]]
    equal (f "a (()") $
        Right [Call (Symbol "a") [val_call "(" []]]
    -- Unbalanced parens.
    -- The error msg is strange for this one, I don't know why.
    left_like (f "a (b") "parse error"

    equal (f "") $ Right [Call (Symbol "") []]
    equal (f "--") $ Right [Call (Symbol "") []]
    equal (f "  a -- comment") $ Right [Call (Symbol "a") []]
    equal (f "a 'b -- c'--d") $ Right
        [Call (Symbol "a") [Literal (VSymbol (Symbol "b -- c"))]]

test_parse_val = do
    let attrs = Just . VAttributes . Score.attrs
        sym = Just . VSymbol . Symbol
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
            , ("#sig,(0)", Just $ VPitchControl $
                DefaultedControl "sig" (TrackLang.call "0" []))

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
    left_like (f "`0x`000") "parse error"

test_p_equal = do
    let eq a b = Right (Call (Symbol "=") [Literal a, b])
        sym = VSymbol . Symbol
    let f = Util.Parse.parse_all Parse.p_equal
    equal (f "a = b") (eq (sym "a") (Literal (VSymbol (Symbol "b"))))
    equal (f "a = 10") (eq (sym "a") (Literal (VNum (Score.untyped 10))))
    equal (f "a = (b c)") (eq (sym "a") (val_call "b" [Literal (symbol "c")]))
    left_like (f "a = ()") "parse error on byte 6"
    left_like (f "(a) = b") "parse error on byte 1"
    left_like (f "a=") "not enough bytes"
    left_like (f "a=b") "not enough bytes"

    let parse = fmap NonEmpty.toList . Parse.parse_expr
    equal (parse "a= b") $ Right [Call (Symbol "a=") [Literal (symbol "b")]]
    equal (parse "a=b") $ Right [Call (Symbol "a=b") []]

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

symbol :: Text -> TrackLang.RawVal
symbol sym = VSymbol (Symbol sym)


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
