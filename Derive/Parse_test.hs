-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Parse_test where
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified GHC.Stack as Stack

import qualified Util.ParseText as ParseText
import qualified Derive.Attrs as Attrs
import qualified Derive.DeriveT as DeriveT
import           Derive.DeriveT (Ref(..), Val(..))
import qualified Derive.Expr as Expr
import           Derive.Expr (Call(..), Term(..))
import qualified Derive.Parse as Parse
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import           Derive.TestInstances ()

import qualified Perform.Signal as Signal

import           Global
import           Util.Test


test_parse_expr :: Test
test_parse_expr = do
    let f = fmap NonEmpty.toList . Parse.parse_expr
        vnum = VNum . ScoreT.untyped
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

    equal (f "a ; b") $ Right
        [Call "a" [Literal VSeparator, Literal (VStr "b")]]

test_parse_expr_val_tokenization :: Test
test_parse_expr_val_tokenization = do
    let f = fmap NonEmpty.toList . Parse.parse_expr
    -- This is because of the spaces1 in p_call.  I want to force call
    -- arguments to be separated by spaces because otherwise "-1.1a" parses
    -- as two valid vals, which is confusing.
    -- TODO I wish for a better error than "expected eof" then
    left_like (f "a -1.1a") "parse error"

test_show_val :: Test
test_show_val = do
    roundtrip "a b"
    roundtrip "a | b"
    roundtrip "a 4 | c 'd e' | f"
    roundtrip "a (b c)"
    roundtrip "a = b"
    roundtrip "a = 'hi there'"
    roundtrip "a =+ 4"

roundtrip :: Stack.HasCallStack => Text -> Test
roundtrip t =
    right_equal (Text.strip . ShowVal.show_val <$> Parse.parse_expr t) t

test_unparsed_call :: Test
test_unparsed_call = do
    let f = fmap NonEmpty.toList . Parse.parse_expr
        vsym = Literal . DeriveT.VStr
        null_call = Call "" []
    equal (f "!<>\"('|") $ Right
        [Call Parse.unparsed_call [vsym "<>\"('"], null_call]
    equal (f "hi \"(!blah)") $ Right
        [Call "hi" [Literal $ DeriveT.VQuoted $
            DeriveT.Quoted (Call "!" [vsym "blah"] :| [])]]
    -- ! takes precedence over =
    equal (f "!a=b") $ Right [Call "!" [vsym "a=b"]]
    -- But comments are still comments.
    equal (f "!'a' -- b") $ Right [Call "!" [vsym "'a'"]]

-- | Vals whose 'ShowVal.show_val' is the inverse of 'Parse.parse_val'.
invertible_vals :: [(Text, Maybe Val)]
invertible_vals =
    [ ("0", Just (VNum (ScoreT.untyped 0)))
    , ("0.", Nothing)
    , (".2", Just (VNum (ScoreT.untyped 0.2)))
    , ("1c", Just (VNum (ScoreT.Typed ScoreT.Chromatic 1)))
    , ("-1d", Just (VNum (ScoreT.Typed ScoreT.Diatonic (-1))))
    , ("-.5d", Just (VNum (ScoreT.Typed ScoreT.Diatonic (-0.5))))
    , ("42nn", Just (VNum (ScoreT.Typed ScoreT.Nn 42)))
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
    , ("a'b", Nothing)

    , ("%", Just $ VControlRef $ LiteralControl "")
    , ("%sig", Just $ VControlRef $ LiteralControl "sig")
    , ("%sig,0", Just $ VControlRef $
        DefaultedControl "sig" (ScoreT.untyped (Signal.constant 0)))
    , ("%sig,4s", Just $ VControlRef $
        DefaultedControl "sig"
        (ScoreT.Typed ScoreT.Real (Signal.constant 4)))
    , ("%sig,4q", Nothing)
    , ("%sig,", Nothing)

    , ("#", Just $ VPControlRef $ LiteralControl "")
    , ("#sig", Just $ VPControlRef $ LiteralControl "sig")

    , ("\"(a b)", Just $ VQuoted $ DeriveT.Quoted $
        Call (Expr.Symbol "a") [Literal (VStr "b")] :| [])
    , ("\"()", Just $ VQuoted $ DeriveT.Quoted $
        Call (Expr.Symbol "") [] :| [])
    , ("\"(a |)", Just $ VQuoted $ DeriveT.Quoted $
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
    [ ("3/2", num (ScoreT.untyped 1.5))
    , ("-3/2", num (ScoreT.untyped (-1.5)))
    , ("3/2d", num (ScoreT.Typed ScoreT.Diatonic 1.5))
    , ("0x00", num (ScoreT.untyped 0))
    , ("0xff", num (ScoreT.untyped 1))
    , ("-0xff", num (ScoreT.untyped (-1)))
    ]
    where num = Just . VNum

test_parse_val :: Test
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

test_parse_num :: Test
test_parse_num = do
    let f = Parse.parse_num
    equal (f "`0x`00") (Right 0)
    equal (f "`0x`ff") (Right 1)
    equal (f "-`0x`ff") (Right (-1))
    left_like (f "`0x`000") "parse error"

test_p_equal :: Test
test_p_equal = do
    let eq a b = Right (Call "=" [Literal (VStr a), b])
        num = Literal . VNum . ScoreT.untyped
    let f = ParseText.parse1 Parse.p_equal
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
    equal (f "i =@ a b") $ Right (eq_syms ["i", "a", "b", "@"])

    left_like (f "a = ()") "parse error"
    left_like (f "a=") "not enough input"

test_lex1 :: Test
test_lex1 = do
    let f = Parse.lex1
    equal (f "a b c") $ ("a ", "b c")
    equal (f "(a b) c") $ ("(a b) ", "c")
    equal (f "(a (b)) c") $ ("(a (b)) ", "c")
    equal (f "(a ')' (x) b) c") $ ("(a ')' (x) b) ", "c")
    equal (f "=y") ("=", "y")
    equal (f "y -- c") ("y ", "-- c")

    -- Incomplete parses get lexed.
    equal (f "1.") $ ("1.", "")
    equal (f "'hi") $ ("'hi", "")

test_lex :: Test
test_lex = do
    let f = Parse.lex
    equal (f "a b c") ["a ", "b ", "c"]
    equal (f "(a b) 'c d'") ["(a b) ", "'c d'"]
    equal (f "x = y") ["x ", "= ", "y"]
    equal (f "x=y") ["x", "=", "y"]
    equal (f "!a b") ["!a b"]

    equal (f "x = y -- c") ["x ", "= ", "y ", "-- c"]

test_split_pipeline :: Test
test_split_pipeline = do
    let f = Parse.split_pipeline
    equal (f "x | y") [["x "], ["y"]]
    equal (f "x=y") [["x", "=", "y"]]
    equal (f "x=y | y") [["x", "=", "y "], ["y"]]

test_split_pipeline_roundtrip :: Test
test_split_pipeline_roundtrip = do
    let f = Parse.join_pipeline . Parse.split_pipeline
    equal (f "x | y") "x | y"
    -- Spaces preserved in round-trip.
    equal (f "x=y | y") "x=y | y"
    equal (f "x = y | y") "x = y | y"
    equal (f "x | y -- c") "x | y -- c"

test_expand_macros :: Test
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
