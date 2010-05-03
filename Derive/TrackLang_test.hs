module Derive.TrackLang_test where
import Control.Monad
import qualified Data.Map as Map

import Util.Control
import qualified Util.Pretty as Pretty
import Util.Test
import qualified Util.Parse as Parse

import qualified Derive.Score as Score
import Derive.TrackLang (AttrMode(..), Call(..), Method(..), Control(..),
    Symbol(..), Val(..))
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch


test_extract = do
    -- tests extract_arg
    let sig :: (TrackLang.Arg Double, TrackLang.Arg Double)
        sig = (TrackLang.required "required", TrackLang.optional "optional" 42)
        mkargs = TrackLang.passed_args "call"
        f args = map_left Pretty.pretty (TrackLang.extract2 args sig)

    left_like (f (mkargs [])) "too few arguments"
    equal (f (mkargs [VNum 1])) (Right (1, 42))
    equal (f (mkargs [VNum 1, VNum 2])) (Right (1, 2))
    equal (f (mkargs [VString "hi", VNum 2]))
        (Left "TypeError: arg 0/required: expected type Num but got 'hi'")

test_check_args = do
    let mkargs = TrackLang.passed_args "call"
        optional = (False, "optional")
        required = (True, "required")
    let f passed args = map_left Pretty.pretty $ take 2 <$>
            TrackLang.check_args passed args
    equal (f (mkargs []) []) (Right [Nothing, Nothing])
    left_like (f (mkargs [VNum 1]) []) "too many arguments: expected 0, got 1"
    left_like (f (mkargs []) [required]) "too few arguments: expected 1, got 0"
    left_like (f (mkargs []) [required, optional])
        "too few arguments: expected from 1 to 2, got 0"
    left_like (f (mkargs [VNum 1, VNum 2]) [optional, required])
        "required arg can't follow an optional one: 1/required"

    equal (f (mkargs [VNum 1]) [required, optional])
        (Right [Just (VNum 1), Nothing])
    let with_env = (mkargs []) { TrackLang.passed_environ =
            Map.fromList [(TrackLang.Symbol "call-required", VNum 10)] }
    equal (f with_env [required, optional])
        (Right [Just (VNum 10), Nothing])


test_parse = do
    let f = TrackLang.parse
    equal (f "a | b") $ Right
        [Call (Symbol "b") [], Call (Symbol "a") []]
    equal (f "a | b | c") $ Right $
        [Call (Symbol "c") [], Call (Symbol "b") [], Call (Symbol "a") []]

    equal (f "a") $ Right [Call (Symbol "a") []]
    equal (f "a 42") $ Right [Call (Symbol "a") [VNum 42]]
    equal (f ">inst") $ Right
        [Call (Symbol "") [VInstrument (Score.Instrument "inst")]]
    equal (f "") $ Right [Call (Symbol "") []]
    equal (f "|") $ Right [Call (Symbol "") [], Call (Symbol "") []]

    equal (f "a|b=4|>inst %sig") $ Right
        [ Call (Symbol "") [VInstrument (Score.Instrument "inst"),
            VControl (Control (Score.Control "sig"))]
        , Call (Symbol "=") [VSymbol (Symbol "b"), VNum 4]
        , Call (Symbol "a") []
        ]

test_p_val = do
    let mkattr = Just . VRelativeAttr . TrackLang.RelativeAttr
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
