{-# LANGUAGE OverloadedStrings #-}
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
       (AttrMode(..), ControlRef(..), Symbol(..), Val(..), Call(..),
        Term(..))

import qualified Perform.Pitch as Pitch


test_parse_expr = do
    let f = fmap NonEmpty.toList . Parse.parse_expr
        vnum = VNum . Score.untyped
    equal (f "a | b") $ Right
        [Call (Symbol "a") [], Call (Symbol "b") []]
    equal (f "a | b | c") $ Right $
        [Call (Symbol "a") [], Call (Symbol "b") [], Call (Symbol "c") []]

    -- Any word in call position is a symbol.
    equal (f "4") $ Right [Call (Symbol "4") []]
    equal (f "4 4") $ Right [Call (Symbol "4") [Literal (vnum 4)]]
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
                VControl (LiteralControl (Score.Control "sig"))])
        ]

    -- Symbols can have anything in them as long as they start with a letter.
    equal (f "a|b=4") $ Right [Call (Symbol "a|b=4") []]

    -- Except parens, which start a subcall.
    equal (f "a(b(4))") $
        Right [Call (Symbol "a") [val_call "b" [val_call "4" []]]]
    equal (f "a (b) c") $
        Right [Call (Symbol "a") [val_call "b" [], Literal (symbol "c")]]
    -- Unbalanced parens.
    -- The error msg is strange for this one, I don't know why.
    left_like (f "a (b") "parse error"

    equal (f "") $ Right [Call (Symbol "") []]
    equal (f "--") $ Right [Call (Symbol "") []]
    equal (f "  a -- comment") $ Right [Call (Symbol "a") []]
    equal (f "a 'b -- c'--d") $ Right
        [Call (Symbol "a") [Literal (VString "b -- c")]]

test_parse_val = do
    let mkattr = Just . VRelativeAttr . TrackLang.RelativeAttr
    let invertible =
            [ (">", Just $ VInstrument (Score.Instrument ""))
            , (">fu/nny^*", Just $ VInstrument (Score.Instrument "fu/nny^*"))

            , ("+a", mkattr (Add, "a"))
            , ("-b", mkattr (Remove, "b"))
            , ("=c", mkattr (Set, "c"))
            , ("=-", mkattr (Clear, ""))
            , ("+aB", Nothing)

            , ("0", Just (VNum (Score.untyped 0)))
            , ("0.", Nothing)
            , (".2", Just (VNum (Score.untyped 0.2)))
            , ("1c", Just (VNum (Score.Typed Score.Chromatic 1)))
            , ("-1d", Just (VNum (Score.Typed Score.Diatonic (-1))))
            , ("-.5d", Just (VNum (Score.Typed Score.Diatonic (-0.5))))
            , ("1q", Nothing)

            , ("'hi'", Just (VString "hi"))
            , ("'quinn''s hat'", Just (VString "quinn's hat"))
            , ("'bad string", Nothing)

            , ("%", Just $ VControl $ LiteralControl (Score.Control ""))
            , ("%sig", Just $ VControl $ LiteralControl (Score.Control "sig"))
            , ("%sig,0", Just $ VControl $
                DefaultedControl (Score.Control "sig") (Score.untyped 0))
            , ("%sig,4s", Just $ VControl $
                DefaultedControl (Score.Control "sig")
                (Score.Typed Score.Real 4))
            , ("%sig,4q", Nothing)
            , ("%sig,", Nothing)

            , ("#", Just $ VPitchControl $
                LiteralControl (Score.Control ""))
            , ("#sig,0", Just $ VPitchControl $
                DefaultedControl (Score.Control "sig")
                    (TrackLang.Note (Pitch.Note "0") []))

            , ("*", Just $ VScaleId (Pitch.ScaleId ""))
            , ("*scale", Just $ VScaleId (Pitch.ScaleId "scale"))
            , ("*bad/scale", Nothing)

            , ("sym", Just $ VSymbol (Symbol "sym"))
            , ("s!$_", Just $ VSymbol (Symbol "s!$_"))
            , ("$bad", Nothing)
            , ("-", Nothing)
            , ("_", Just VNotGiven)
            ]
    -- Vals that don't convert back into the original val after
    -- parse . show_val.
    let not_invertible =
            [ ("`0x`00", Just (VNum (Score.untyped 0)))
            , ("`0x`001", Nothing)
            , ("`0x`ff", Just (VNum (Score.untyped 1)))
            ]
    let exprs = map ((,) True) invertible ++ map ((,) False) not_invertible
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

    equal (f "*a = *b") (eq (VScaleId (Pitch.ScaleId "a"))
        (Literal (VScaleId (Pitch.ScaleId "b"))))
    equal (f "a = =b") (eq (sym "a")
        (Literal (VRelativeAttr (TrackLang.RelativeAttr (Set, "b")))))

    let parse = fmap NonEmpty.toList . Parse.parse_expr
    equal (parse "a =b") $ Right [Call (Symbol "a")
        [Literal (VRelativeAttr (TrackLang.RelativeAttr (Set, "b")))]]
    equal (parse "a= b") $ Right [Call (Symbol "a=") [Literal (symbol "b")]]
    equal (parse "a=b") $ Right [Call (Symbol "a=b") []]

val_call :: String -> [Term] -> Term
val_call sym args = ValCall (Call (Symbol sym) args)

symbol :: String -> Val
symbol sym = VSymbol (Symbol sym)


test_expand_macros = do
    let f = Parse.expand_macros ("!" <>)
    equal (f "") (Right "")
    equal (f "hi") (Right "hi")
    left_like (f "hi @") "parse error"
    equal (f "hi @a-b") (Right "hi !a-b")
    equal (f "hi @a b") (Right "hi !a b")
    equal (f "hi (Just @there)") (Right "hi (Just !there)")
    -- Doesn't substitute macros inside quotes.
    equal (f "hi \"@a\" there") (Right "hi \"@a\" there")
    left_like (f "hi @not/ident") "parse error"
