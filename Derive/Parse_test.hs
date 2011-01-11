module Derive.Parse_test where
import Control.Monad

import Util.Test
import qualified Util.Parse as Util.Parse
import qualified Util.Pretty as Pretty

import qualified Derive.Score as Score
import qualified Derive.Parse as Parse
import Derive.Parse (Call(..), Term(..))
import qualified Derive.TrackLang as TrackLang
import Derive.TrackLang (AttrMode(..), ControlRef(..), Symbol(..), Val(..))

import qualified Perform.Pitch as Pitch


test_parse = do
    let f = Parse.parse
    equal (f "a | b") $ Right
        [Call (Symbol "a") [], Call (Symbol "b") []]
    equal (f "a | b | c") $ Right $
        [Call (Symbol "a") [], Call (Symbol "b") [], Call (Symbol "c") []]

    -- Any word in call position is a symbol.
    equal (f "4") $ Right [Call (Symbol "4") []]
    equal (f "4 4") $ Right [Call (Symbol "4") [Literal (VNum 4)]]
    -- So the only way to have a null call is a null expression.
    equal (f "") $ Right [Call (Symbol "") []]

    equal (f "a") $ Right [Call (Symbol "a") []]
    equal (f "a 42") $ Right [Call (Symbol "a") [Literal (VNum 42)]]
    equal (f "a | ") $ Right [Call (Symbol "a") [], Call (Symbol "") []]

    equal (f "a | b = 4 | . >inst %sig") $ Right
        [ Call (Symbol "a") []
        , Call (Symbol "=") (map Literal [symbol "b", VNum 4])
        , Call (Symbol ".") (map Literal [VInstrument (Score.Instrument "inst"),
            VControl (Control (Score.Control "sig"))])
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

test_p_val = do
    let mkattr = Just . VRelativeAttr . TrackLang.RelativeAttr
    let expr_expected =
            [ (">", Just $ VInstrument (Score.Instrument ""))
            , (">fu/nny^*", Just $ VInstrument (Score.Instrument "fu/nny^*"))

            , ("+a", mkattr (Add, "a"))
            , ("-b", mkattr (Remove, "b"))
            , ("=c", mkattr (Set, "c"))
            , ("=-", mkattr (Clear, ""))
            , ("+aB", Nothing)

            , ("0", Just (VNum 0))
            , ("0.", Nothing)
            , (".2", Just (VNum 0.2))

            , ("'hi'", Just (VString "hi"))
            , ("'quinn''s hat'", Just (VString "quinn's hat"))
            , ("'bad string", Nothing)

            , ("%sig", Just $ VControl $ Control (Score.Control "sig"))
            , ("%sig,0", Just $ VControl $
                DefaultedControl (Score.Control "sig") 0)
            , ("%sig,", Nothing)

            , ("#", Just $ VPitchControl $
                Control (Score.Control ""))
            , ("#sig,0", Just $ VPitchControl $
                DefaultedControl (Score.Control "sig") (Pitch.Note "0"))

            , ("*", Just $ VScaleId (Pitch.ScaleId ""))
            , ("*scale", Just $ VScaleId (Pitch.ScaleId "scale"))
            , ("*bad/scale", Nothing)

            , ("sym", Just $ VSymbol (Symbol "sym"))
            , ("s!$_", Just $ VSymbol (Symbol "s!$_"))
            , ("$bad", Nothing)
            , ("-", Nothing)
            , ("_", Just VNotGiven)
            ]
    forM_ expr_expected $ \(expr, expected) -> do
        let res = Util.Parse.parse_all Parse.p_val expr
        case (res, expected) of
            (Left err, Just expect) -> failure $
                err ++ ", expected " ++ show expect
            (Right val, Nothing) -> failure $
                "shouldn't have parsed: " ++ show expr ++ " -> " ++ show val
            (Right val, Just expect) -> do
                equal val expect
                equal (Pretty.pretty val) expr
            _ -> success $ show res ++ " == " ++ show expected

test_p_equal = do
    let eq a b = Right (Call (Symbol "=") [Literal a, b])
        sym = VSymbol . Symbol
    let f = Util.Parse.parse_all Parse.p_equal
    equal (f "a = b") (eq (sym "a") (Literal (VSymbol (Symbol "b"))))
    equal (f "a = 10") (eq (sym "a") (Literal (VNum 10)))
    equal (f "a = (b c)") (eq (sym "a") (val_call "b" [Literal (symbol "c")]))
    left_like (f "a = ()") "unexpected \")\""
    left_like (f "(a) = b") "unexpected \"(\""
    left_like (f "a=") "unexpected end of input"
    left_like (f "a=b") "unexpected end of input"

    equal (f "*a = *b") (eq (VScaleId (Pitch.ScaleId "a"))
        (Literal (VScaleId (Pitch.ScaleId "b"))))
    equal (f "a = =b") (eq (sym "a")
        (Literal (VRelativeAttr (TrackLang.RelativeAttr (Set, "b")))))


    let parse = Parse.parse
    equal (parse "a =b") $ Right [Call (Symbol "a")
        [Literal (VRelativeAttr (TrackLang.RelativeAttr (Set, "b")))]]
    equal (parse "a= b") $ Right [Call (Symbol "a=") [Literal (symbol "b")]]
    equal (parse "a=b") $ Right [Call (Symbol "a=b") []]

val_call sym args = ValCall (Call (Symbol sym) args)
symbol sym = VSymbol (Symbol sym)
