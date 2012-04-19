module Derive.CallSig_test where
import qualified Data.Map as Map

import Util.Control
import qualified Util.Pretty as Pretty
import Util.Test

import qualified Ui.State as State
import qualified Derive.CallSig as CallSig
import Derive.CallSig (Arg(..))
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.TrackLang as TrackLang
import Derive.TrackLang (Val(..))


test_extract = do
    -- tests extract_arg
    let sig :: (Arg Double, Arg Double)
        sig = (CallSig.required "required", CallSig.optional "optional" 42)
        f args = DeriveTest.eval State.empty (CallSig.extract2 args sig)

    left_like (f (mkargs [])) "too few arguments"
    equal (f (mkargs [vnum 1])) (Right (1, 42))
    equal (f (mkargs [vnum 1, vnum 2])) (Right (1, 2))
    left_like (f (mkargs [VString "hi", vnum 2]))
        "arg 0/required: expected Num but got String: 'hi'"

test_maybe_arg = do
    let opt = CallSig.optional "opt"
        req = CallSig.required "req"
    let f sig args = DeriveTest.eval State.empty
            (CallSig.extract1 (mkargs args) sig)
    equal (f (req :: Arg (Maybe Double)) [vnum 1]) $ Right (Just 1)
    left_like (f (req :: Arg (Maybe Double)) [VNotGiven]) "TypeError"

    equal (f (opt 42 :: Arg Double) [VNotGiven]) $ Right 42
    equal (f (opt (Just 42) :: Arg (Maybe Double)) [VNotGiven]) $
        Right (Just 42)
    equal (f (opt Nothing :: Arg (Maybe Double)) [VNotGiven]) $
        Right Nothing
    left_like (f (opt Nothing :: Arg (Maybe Double)) [VString "hi"])
        "expected Maybe Num but got String: 'hi'"

test_check_args = do
    let mkargs = DeriveTest.passed_args "call"
        optional = (False, "optional")
        required = (True, "required")
    let f environ passed args = map_left Pretty.pretty $ take 2 <$>
            CallSig.pure_check_args environ passed args
    equal (f mempty (mkargs []) []) (Right [Nothing, Nothing])
    left_like (f mempty (mkargs [vnum 1]) [])
        "too many arguments: expected 0, got 1"
    left_like (f mempty (mkargs []) [required])
        "too few arguments: expected 1, got 0"
    left_like (f mempty (mkargs []) [required, optional])
        "too few arguments: expected from 1 to 2, got 0"
    left_like (f mempty (mkargs [vnum 1, vnum 2]) [optional, required])
        "required arg can't follow an optional one: 1/required"

    equal (f mempty (mkargs [vnum 1]) [required, optional])
        (Right [Just (vnum 1), Nothing])
    let env = Map.fromList [(TrackLang.Symbol "call-required", vnum 10)]
    equal (f env (mkargs []) [required, optional])
        (Right [Just (vnum 10), Nothing])

mkargs = DeriveTest.passed_args "call"
vnum = TrackLang.num
