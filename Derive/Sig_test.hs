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
import qualified Derive.Stream as Stream
import qualified Derive.Typecheck as Typecheck

import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest

import           Global
import           Util.Test


test_errors :: Test
test_errors = do
    let run call = extract_logs $ DeriveTest.derive_tracks ""
                [ (">", [(0, 1, "")])
                , ("c", [(0, 0, call)])
                ]
    strings_like (run "set") ["set: expected an argument at \"to\""]
    strings_like (run "set (env)") ["env: expected an argument at \"name\""]

extract_logs :: Derive.Result -> [Text]
extract_logs = map DeriveTest.show_log_stack . snd . DeriveTest.extract_logs
    . Stream.to_list . Derive.r_events

test_type_error :: Test
test_type_error = do
    let ints :: Sig.Parser [Int]
        ints = Sig.many "ints" ""
        str = DeriveT.str "hi"
        int = DeriveT.num 42
        int_sym :: Sig.Parser (Int, Expr.Str)
        int_sym = (,)
            <$> Sig.required "int" ""
            <*> Sig.defaulted "sym" ("" :: Text) ""
    left_like (call ints [str])
        "arg 1/ints: expected Signal (integral) but got Str: hi"
    left_like (call int_sym [str]) "arg 1/int: expected Signal"
    left_like (call int_sym [int, int]) "arg 2/sym: expected Str"

test_eval_quoted :: Test
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
        "arg 1/int from \"(v): expected Signal"
    equal (run (0 :: Int) int [quoted "v"]) (Right 0)

    let quot :: Sig.Parser DeriveT.Quoted
        quot = Sig.required "quot" "doc"
    equal (run (0 :: Int) quot [DeriveT.VStr "x"])
        (Right (DeriveT.Quoted (Expr.generator0 "x")))
    equal (run (0 :: Int) quot [quoted "v"])
        (Right (DeriveT.Quoted (Expr.generator0 "v")))

test_deriver :: Test
test_deriver = do
    let run = DeriveTest.extract DeriveTest.e_note
            . DeriveTest.derive_tracks_setup with_ngen ""
            . UiTest.note_track
    equal (run [(0, 1, "ngen NOTE -- 4c")]) ([(0, 1, "4c")], [])
    equal (run [(0, 1, "ngen \"(t-dia=1 | NOTE) -- 4c")]) ([(0, 1, "4d")], [])
    -- The derive is evaluated in 'ngen', so that's whene the error comes from.
    equal (run [(0, 1, "ngen \"(NOTE 1 2 3) -- 4c")])
        ([], ["too many arguments: 1, 2, 3"])
    equal (run [(0, 1, "ngen \"(NOTE) \"(t-dia=1 | NOTE) -- 4c")])
        ([(0, 1, "4c"), (0, 1, "4d")], [])

test_deriver_children :: Test
test_deriver_children = do
    let run children skel = DeriveTest.extract DeriveTest.e_note $
            DeriveTest.derive_tracks_setup
                    (with_ngen <> DeriveTest.with_skel skel) "" $
                (">", [(4, 1, "ngen")]) : concatMap UiTest.note_track children
    strings_like (snd $ run [] []) ["expected an argument at \"deriver\""]
    -- it's awkward that I have to give the skeleton explicitly
    equal (run [[(4, 1, "4c")]] [(1, 2), (2, 3)])
        ([(4, 1, "4c")], [])
    equal (run [[(4, 1, "4c")], [(4, 1, "4d")]]
            [(1, 2), (2, 3), (1, 4), (4, 5)])
        ([(4, 1, "4c"), (4, 1, "4d")], [])
    strings_like (snd $ run [[(4, 1, "4c")], [(4, 1, "4d")], [(4, 1, "4e")]]
            [(1, 2), (2, 3), (1, 4), (4, 5), (1, 6), (6, 7)])
        ["too many arguments: subtrack:"]

with_ngen :: DeriveTest.SetupA a
with_ngen = CallTest.with_note_generator "ngen" generator

generator :: Derive.Generator Derive.Note
generator = Derive.generator "test" "ngen" mempty "doc" $
    Sig.call_sub ((,)
        <$> Sig.required "deriver" "doc"
        <*> Sig.defaulted "deriver" (Nothing :: Maybe Sig.Dummy) "doc"
    ) $ \(deriver1, deriver2) _args -> deriver1 <> fromMaybe mempty deriver2

test_optional :: Test
test_optional = do
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
    let text :: Sig.Parser Text
        text = Sig.optional "text" "x" ""
    let t = Typecheck.to_val @Text
    equal (call ((,) <$> text <*> ints) [t "hi", num 42, num 52])
        (Right ("hi", [42, 52]))
    equal (call ((,) <$> text <*> ints) [num 42, num 52])
        (Right ("x", [42, 52]))

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
