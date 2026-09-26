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
    let run call = extractLogs $ DeriveTest.deriveTracks ""
                [ (">", [(0, 1, "")])
                , ("c", [(0, 0, call)])
                ]
    stringsLike (run "set") ["set: expected an argument at \"to\""]
    stringsLike (run "set (env)") ["env: expected an argument at \"name\""]

extractLogs :: Derive.Result -> [Text]
extractLogs = map DeriveTest.showLogStack . snd . DeriveTest.extractLogs
    . Stream.toList . Derive.rEvents

test_type_error :: Test
test_type_error = do
    let ints :: Sig.Parser [Int]
        ints = Sig.many "ints" ""
        str = DeriveT.str "hi"
        int = DeriveT.num 42
        intSym :: Sig.Parser (Int, Expr.Str)
        intSym = (,)
            <$> Sig.required "int" ""
            <*> Sig.defaulted "sym" ("" :: Text) ""
    leftLike (call ints [str])
        "arg 1/ints: expected Signal (integral) but got Str: hi"
    leftLike (call intSym [str]) "arg 1/int: expected Signal"
    leftLike (call intSym [int, int]) "arg 2/sym: expected Str"

test_eval_quoted :: Test
test_eval_quoted = do
    let int :: Sig.Parser Int
        int = Sig.required "int" "doc"
    let quoted sym = DeriveT.VQuoted $ DeriveT.Quoted $ Expr.generator0 sym
    let run val = callWith $ DeriveTest.setupDeriver $
            CallTest.withValCall "v" (valCall val)
        valCall val = Derive.valCall "test" "v" mempty "" $ Sig.call0 $ \_ ->
            pure $ Typecheck.toVal val
    -- A Quoted can be coerced into an int by evaluating it.
    leftLike (run (0 :: Int) int [quoted "not-found"])
        "arg 1/int from \"(not-found): *val call not found"
    leftLike (run ("hi" :: Text) int [quoted "v"])
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
    let run = DeriveTest.extract DeriveTest.eNote
            . DeriveTest.deriveTracksSetup withNgen ""
            . UiTest.noteTrack
    equal (run [(0, 1, "ngen NOTE -- 4c")]) ([(0, 1, "4c")], [])
    equal (run [(0, 1, "ngen \"(t-dia=1 | NOTE) -- 4c")]) ([(0, 1, "4d")], [])
    -- The derive is evaluated in 'ngen', so that's whene the error comes from.
    equal (run [(0, 1, "ngen \"(NOTE 1 2 3) -- 4c")])
        ([], ["too many arguments: 1, 2, 3"])
    equal (run [(0, 1, "ngen \"(NOTE) \"(t-dia=1 | NOTE) -- 4c")])
        ([(0, 1, "4c"), (0, 1, "4d")], [])

test_deriver_children :: Test
test_deriver_children = do
    let run children skel = DeriveTest.extract DeriveTest.eNote $
            DeriveTest.deriveTracksSetup
                    (withNgen <> DeriveTest.withSkel skel) "" $
                (">", [(4, 1, "ngen")]) : concatMap UiTest.noteTrack children
    stringsLike (snd $ run [] []) ["expected an argument at \"deriver\""]
    -- it's awkward that I have to give the skeleton explicitly
    equal (run [[(4, 1, "4c")]] [(1, 2), (2, 3)])
        ([(4, 1, "4c")], [])
    equal (run [[(4, 1, "4c")], [(4, 1, "4d")]]
            [(1, 2), (2, 3), (1, 4), (4, 5)])
        ([(4, 1, "4c"), (4, 1, "4d")], [])
    stringsLike (snd $ run [[(4, 1, "4c")], [(4, 1, "4d")], [(4, 1, "4e")]]
            [(1, 2), (2, 3), (1, 4), (4, 5), (1, 6), (6, 7)])
        ["too many arguments: subtrack:"]

withNgen :: DeriveTest.SetupA a
withNgen = CallTest.withNoteGenerator "ngen" generator

generator :: Derive.Generator Derive.Note
generator = Derive.generator "test" "ngen" mempty "doc" $
    Sig.callSub ((,)
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
    let t = Typecheck.toVal @Text
    equal (call ((,) <$> text <*> ints) [t "hi", num 42, num 52])
        (Right ("hi", [42, 52]))
    equal (call ((,) <$> text <*> ints) [num 42, num 52])
        (Right ("x", [42, 52]))

call :: Sig.Parser a -> [DeriveT.Val] -> Either Text a
call = callWith id

callWith :: (Derive.Deriver a -> Derive.Deriver a) -> Sig.Parser a
    -> [DeriveT.Val] -> Either Text a
callWith with p vals = DeriveTest.eval Ui.empty $
    with $ run (Sig.call p (\val _args -> pure val)) vals

run :: Derive.WithArgDoc (Sig.Generator Derive.Tagged a) -> [DeriveT.Val]
    -> Derive.Deriver a
run (f, _) vals = f args
    where
    args = Derive.PassedArgs
        { Derive.passedVals = vals
        , Derive.passedCallName = "test-call"
        , Derive.passedCtx = Derive.dummyContext 0 1 "test"
        }
