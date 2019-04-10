-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Sig_test where
import qualified Derive.Call.CallTest as CallTest
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Expr as Expr
import qualified Derive.Sig as Sig
import qualified Derive.Typecheck as Typecheck

import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest

import           Global
import           Util.Test


test_type_error = do
    let ints :: Sig.Parser [Int]
        ints = Sig.many "ints" ""
        str = DeriveT.str "hi"
        int = DeriveT.num 42
        int_sym :: Sig.Parser (Int, Expr.Str)
        int_sym = (,) <$> Sig.required "int" "" <*> Sig.defaulted "sym" "" ""
    left_like (call ints [str])
        "arg 1/ints: expected Num (integral) but got Str: hi"
    left_like (call int_sym [str]) "arg 1/int: expected Num"
    left_like (call int_sym [int, int]) "arg 2/sym: expected Str"

test_eval_quoted = do
    let int :: Sig.Parser Int
        int = Sig.required "int" "doc"
    let quoted sym = DeriveT.VQuoted $ DeriveT.Quoted $ Expr.generator0 sym
    let run val = call_with $ DeriveTest.setup_deriver $
            CallTest.with_val_call "v" (val_call val)
        val_call val = Derive.val_call "test" "v" mempty "" $ Sig.call0 $ \_ ->
            return $ Typecheck.to_val val
    -- A Quoted can be coerced into an int by evaluating it.
    left_like (run (0 :: Int) int [quoted "not-found"])
        "arg 1/int from \"(not-found): *val call not found"
    left_like (run ("hi" :: Text) int [quoted "v"])
        "arg 1/int from \"(v): expected Num"
    equal (run (0 :: Int) int [quoted "v"]) (Right 0)

    let quot :: Sig.Parser DeriveT.Quoted
        quot = Sig.required "quot" "doc"
    equal (run (0 :: Int) quot [DeriveT.VStr "x"])
        (Right (DeriveT.Quoted (Expr.generator0 "x")))
    equal (run (0 :: Int) quot [quoted "v"])
        (Right (DeriveT.Quoted (Expr.generator0 "v")))

test_deriver = do
    let run = DeriveTest.extract DeriveTest.e_note
            . DeriveTest.derive_tracks_setup
                (CallTest.with_note_generator "ngen" generator) ""
            . UiTest.note_track
    equal (run [(0, 1, "ngen NOTE -- 4c")]) ([(0, 1, "4c")], [])
    equal (run [(0, 1, "ngen \"(%t-dia=1 | NOTE) -- 4c")]) ([(0, 1, "4d")], [])
    -- The derive is evaluated in 'ngen', so that's whene the error comes from.
    equal (run [(0, 1, "ngen \"(NOTE 1 2 3) -- 4c")])
        ([], ["Error: ArgError: too many arguments: 1, 2, 3"])
    equal (run [(0, 1, "ngen \"(NOTE) \"(%t-dia=1 | NOTE) -- 4c")])
        ([(0, 1, "4c"), (0, 1, "4d")], [])

generator :: Derive.Generator Derive.Note
generator = Derive.generator "test" "test" mempty "doc" $
    Sig.call ((,)
        <$> Sig.required "deriver" "doc"
        <*> Sig.defaulted "deriver" (return mempty) "doc"
    ) $ \(deriver1, deriver2) _args -> deriver1 <> deriver2

test_not_given = do
    let int :: Sig.Parser (Maybe Int)
        int = Sig.optional "int" Nothing ""
        ints :: Sig.Parser [Int]
        ints = Sig.many "ints" ""
    let num = DeriveT.num
    equal (call int []) (Right Nothing)
    equal (call int [num 42]) (Right (Just 42))
    equal (call ((,) <$> int <*> ints) [num 42])
        (Right (Just 42, []))
    equal (call ((,) <$> int <*> ints) [num 42, num 52])
        (Right (Just 42, [52]))
    equal (call ((,) <$> int <*> ints) [DeriveT.VNotGiven, num 52])
        (Right (Nothing, [52]))

call :: Sig.Parser a -> [DeriveT.Val] -> Either Text a
call = call_with id

call_with :: (Derive.Deriver a -> Derive.Deriver a) -> Sig.Parser a
    -> [DeriveT.Val] -> Either Text a
call_with with p vals = DeriveTest.eval Ui.empty $
    with $ run (Sig.call p (\val _args -> return val)) vals

run :: Derive.WithArgDoc (Sig.Generator Derive.Tagged a) -> [DeriveT.Val]
    -> Derive.Deriver a
run (f, _) vals = f args
    where
    args = Derive.PassedArgs
        { Derive.passed_vals = vals
        , Derive.passed_call_name = "test-call"
        , Derive.passed_ctx = Derive.dummy_context 0 1 "test"
        }
