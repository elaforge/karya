module Derive.TrackLang_test where
import Control.Monad

import Util.Test
import qualified Util.Parse as Parse

import qualified Derive.Score as Score
import Derive.TrackLang (AttrMode(..), Call(..), Method(..), Signal(..),
    Symbol(..), Val(..))
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch


test_extract = do
    let sig0 :: (TrackLang.Arg Double, TrackLang.Arg Double)
        sig0 = (TrackLang.Arg "mandatory" Nothing,
            TrackLang.Arg "optional" (Just 42))
        f args sig = map_left TrackLang.show_type_error
            (TrackLang.extract2 args sig)

    left_like (f [] sig0) "too few arguments"
    equal (f [VNum 1] sig0) (Right (1, 42))
    equal (f [VNum 1, VNum 2] sig0) (Right (1, 2))
    left_like (f [VMethod (TrackLang.Method "ho")] sig0)
        "0/mandatory: expected TNum"
    left_like (f [VNum 1, VNum 2, VNum 3] sig0)
        "too many arguments"
    left_like (f [VNum 1] (snd sig0, fst sig0))
        "required args can't follow an optional one"

test_parse = do
    let f = TrackLang.parse
    equal (f "a | b") $ Right
        [Call (Symbol "a") [], Call (Symbol "b") []]
    equal (f "a | b | c") $ Right $
        [Call (Symbol "a") [], Call (Symbol "b") [], Call (Symbol "c") []]

    equal (f "a") $ Right [Call (Symbol "a") []]
    equal (f "a 42") $ Right [Call (Symbol "a") [VNum 42]]
    equal (f ">inst") $ Right
        [Call (Symbol "") [VInstrument (Score.Instrument "inst")]]
    equal (f "") $ Right [Call (Symbol "") []]
    equal (f "|") $ Right [Call (Symbol "") [], Call (Symbol "") []]

    equal (f "a|b=4|>inst %sig") $ Right
        [ Call (Symbol "a") []
        , Call (Symbol "=") [VSymbol (Symbol "b"), VNum 4]
        , Call (Symbol "") [VInstrument (Score.Instrument "inst"),
            VSignal (Signal (Nothing, Just (Score.Control "sig")))]
        ]

test_p_val = do
    let mkattr = Just . VRelativeAttr . TrackLang.RelativeAttr
        mksig deflt cont = Just $ VSignal $
            TrackLang.Signal (deflt, fmap Score.Control cont)
        mknote = Just . VNote . Pitch.Note
    let expr_expected =
            [ ("*note", mknote "note")
            , ("*5num*", mknote "5num*")

            , (">", Just $ VInstrument (Score.Instrument ""))
            , (">fu/nny^*", Just $ VInstrument (Score.Instrument "fu/nny^*"))

            , ("m'foo'", Just $ VMethod (Method "foo"))
            , ("m'foo", Nothing)

            , ("+a", mkattr (Add, "a"))
            , ("-b", mkattr (Remove, "b"))
            , ("=c", mkattr (Set, "c"))
            , ("=", mkattr (Clear, ""))
            , ("+aB", Nothing)

            , ("0", Just (VNum 0))
            , ("0.", Nothing)
            , (".2", Just (VNum 0.2))

            , ("%sig", mksig Nothing (Just "sig"))
            , ("%sig,0", mksig (Just 0) (Just "sig"))
            , ("%sig,", Nothing)

            , ("sym", Just $ VSymbol (Symbol "sym"))
            , ("bad/sym", Nothing)
            , ("-", Nothing)
            , ("_", Just VNotGiven)
            ]
    forM_ expr_expected $ \(expr, expected) -> do
        let res = Parse.parse_all TrackLang.p_val expr
        case (res, expected) of
            (Left err, Just expect) -> failure $
                err ++ ", expected " ++ show expect
            (Right val, Nothing) -> failure $
                "shouldn't have parsed: " ++ show expr ++ " -> " ++ show val
            (Right val, Just expect) -> equal val expect
            _ -> success $ show res ++ " == " ++ show expected

test_p_equal = do
    let eq a b = Right (Call (Symbol "=") [VSymbol (Symbol a), b])
    let f = Parse.parse_all TrackLang.p_equal
    equal (f "a = b") (eq "a" (VSymbol (Symbol "b")))
    equal (f "a = 10") (eq "a" (VNum 10))
    equal (f "a=10") (eq "a" (VNum 10))
    left_like (f "a=") "unexpected end of input"
