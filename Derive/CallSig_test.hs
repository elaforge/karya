module Derive.CallSig_test where
import qualified Data.Map as Map

import Util.Control
import qualified Util.Pretty as Pretty
import Util.Test

import qualified Derive.TrackLang as TrackLang
import Derive.TrackLang (Val(..))
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.CallSig as CallSig
import Derive.CallSig (Arg(..))


test_extract = do
    -- tests extract_arg
    let sig :: (Arg Double, Arg Double)
        sig = (CallSig.required "required", CallSig.optional "optional" 42)
        f args = map_left Pretty.pretty (CallSig.extract2 args sig)

    left_like (f (mkargs [])) "too few arguments"
    equal (f (mkargs [VNum 1])) (Right (1, 42))
    equal (f (mkargs [VNum 1, VNum 2])) (Right (1, 2))
    equal (f (mkargs [VString "hi", VNum 2]))
        (Left "TypeError: arg 0/required: expected Num but got String 'hi'")

test_maybe_arg = do
    let opt = CallSig.optional "opt"
        req = CallSig.required "req"
    let f sig args = map_left Pretty.pretty (CallSig.extract1 (mkargs args) sig)
    equal (f (req :: Arg (Maybe Double)) [VNum 1]) $ Right (Just 1)
    left_like (f (req :: Arg (Maybe Double)) [VNotGiven]) "TypeError"

    equal (f (opt 42 :: Arg Double) [VNotGiven]) $ Right 42
    equal (f (opt (Just 42) :: Arg (Maybe Double)) [VNotGiven]) $
        Right (Just 42)
    equal (f (opt Nothing :: Arg (Maybe Double)) [VNotGiven]) $
        Right Nothing
    left_like (f (opt Nothing :: Arg (Maybe Double)) [VString "hi"])
        "expected Maybe Num but got String 'hi'"


test_check_args = do
    let mkargs = DeriveTest.passed_args "call"
        optional = (False, "optional")
        required = (True, "required")
    let f passed args = map_left Pretty.pretty $ take 2 <$>
            CallSig.check_args passed args
    equal (f (mkargs []) []) (Right [Nothing, Nothing])
    left_like (f (mkargs [VNum 1]) []) "too many arguments: expected 0, got 1"
    left_like (f (mkargs []) [required]) "too few arguments: expected 1, got 0"
    left_like (f (mkargs []) [required, optional])
        "too few arguments: expected from 1 to 2, got 0"
    left_like (f (mkargs [VNum 1, VNum 2]) [optional, required])
        "required arg can't follow an optional one: 1/required"

    equal (f (mkargs [VNum 1]) [required, optional])
        (Right [Just (VNum 1), Nothing])
    let with_env = (mkargs []) { Derive.passed_environ =
            Map.fromList [(TrackLang.Symbol "call-required", VNum 10)] }
    equal (f with_env [required, optional])
        (Right [Just (VNum 10), Nothing])

mkargs = DeriveTest.passed_args "call"
