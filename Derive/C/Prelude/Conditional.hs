-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Transformers that evaluate their deriver conditionally.
module Derive.C.Prelude.Conditional where
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty

import qualified Derive.Args as Args
import qualified Derive.Call.Module as Module
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Eval as Eval
import qualified Derive.Library as Library
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Stream as Stream
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Signal as Signal

import           Global
import           Types


library :: Library.Library
library = mconcat
    [ Library.transformers [("solo", c_solo)]
    , Library.poly_generators
        [ ("if-e", c_if_e)
        , ("if-c<", c_if_c (<))
        , ("if-c>", c_if_c (>))
        , ("on-repeat", c_on_repeat)
        ]
    , Library.poly_transformers
        [ ("when-c", c_when_c False)
        , ("unless-c", c_when_c True)
        , ("when-e", c_when_e False)
        , ("unless-e", c_when_e True)
        ]
    ]

-- * generator

c_if_e :: Derive.CallableExpr d => Derive.Generator d
c_if_e = Derive.generator Module.prelude "if-e" mempty
    "Derive based on the value of an environment variable."
    $ Sig.call ((,,,)
    <$> Sig.required "name" "Environ key."
    <*> Sig.defaulted "value" (Nothing :: Maybe DeriveT.Val)
        "Environ value. If not given, require\
        \ only that the environ key is set."
    <*> Sig.required "true" "Eval if true."
    <*> Sig.required "false" "Eval if false."
    ) $ \(name, maybe_value, true, false) args ->
        ifM (has_environ name maybe_value)
            (Eval.eval_quoted (Args.context args) true)
            (Eval.eval_quoted (Args.context args) false)

c_if_c :: Derive.CallableExpr d => (Signal.Y -> Signal.Y -> Bool)
    -> Derive.Generator d
c_if_c cmp = Derive.generator Module.prelude "if-c<" mempty
    "Derive based on the value of a control."
    $ Sig.call ((,)
    <$> Sig.required "control" "Test this control."
    <*> Sig.many1 "tests" "(value, expr) pairs."
    ) $ \(control, tests) args -> do
        val <- fromMaybe 0 <$>
            (Derive.untyped_control_at control =<< Args.real_start args)
        (tests, final) <- typecheck_tests (Args.start args)
            (NonEmpty.toList tests)
        Eval.eval_quoted (Args.context args) $ maybe final snd $
            List.find (cmp val . fst) tests

typecheck_tests :: ScoreTime -> [DeriveT.Val]
    -> Derive.Deriver ([(Signal.Y, DeriveT.Quoted)], DeriveT.Quoted)
typecheck_tests start = go
    where
    go [] = Derive.throw "not enough values"
    go [x] = do
        final <- typecheck x
        return ([], final)
    go (val : result : rest) = do
        checked <- (,) <$> typecheck val <*> typecheck result
        (rest, final) <- go rest
        return (checked : rest, final)
    typecheck :: Typecheck.Typecheck a => DeriveT.Val -> Derive.Deriver a
    typecheck = Typecheck.typecheck "" start

c_on_repeat :: Derive.CallableExpr d => Derive.Generator d
c_on_repeat = Derive.generator Module.prelude "on-repeat" mempty
    "Derive the argument indexed by the `repeat` variable, where an out of\
    \ range index is clamped to be in range."
    $ Sig.call (Sig.many1 "repeat" "Eval on nth repeat.") $ \repeats args -> do
        repeat <- Derive.lookup_val "repeat"
        Eval.eval_quoted (Args.context args) (at repeats (fromMaybe 0 repeat))

at :: NonEmpty a -> Int -> a
at (x0 :| x1: xs) i
    | i <= 0 = x0
    | otherwise = at (x1 :| xs) (i-1)
at (x0 :| []) _ = x0

-- * transformer

c_solo :: Derive.Transformer Derive.Note
c_solo = Derive.transformer Module.prelude "solo" mempty
    "Only derive if `inst` is set to the given value. This is a specialized\
    \ version of `when-e`."
    $ Sig.callt (Sig.required "inst" "Instrument.")
    $ \inst _args deriver ->
        ifM (has_environ EnvKey.instrument
                (Just (Typecheck.to_val (inst :: ScoreT.Instrument))))
            deriver mempty

c_when_c :: Derive.Taggable d => Bool -> Derive.Transformer d
c_when_c inverted = Derive.transformer Module.prelude "when-c" mempty
    "Only derive if the control has the given value. E.g., you can use a\
    \ `%var` control to select among different variations."
    $ Sig.callt ((,)
    <$> Sig.required "val" "Value."
    <*> Sig.defaulted "control" (0 :: Double) "Control."
    ) $ \(val :: Int, control :: Double) _args deriver ->
        if invert (round control == val)
            then deriver else return Stream.empty
    where invert = if inverted then not else id

c_when_e :: Derive.Taggable d => Bool -> Derive.Transformer d
c_when_e inverted = Derive.transformer Module.prelude "when-e" mempty
    "Only derive if environ value is set to the given value. In a block\
    \ derived multiple times by different instruments, this can be used to\
    \ solo a bit of score to one particular instrument."
    $ Sig.callt ((,)
    <$> Sig.required "name" "Environ key."
    <*> Sig.defaulted "value" (Nothing :: Maybe DeriveT.Val)
        "Environ value. If not given, require\
        \ only that the environ key is set."
    ) $ \(name, maybe_value) _args deriver ->
        ifM (invert $ has_environ name maybe_value) deriver
            (return Stream.empty)
    where invert = if inverted then (not <$>) else id

has_environ :: Env.Key -> Maybe DeriveT.Val -> Derive.Deriver Bool
has_environ name maybe_val = Derive.lookup_val name >>= \case
    Nothing -> return False
    Just env_val -> case maybe_val of
        Nothing -> return True
        Just val -> case DeriveT.vals_equal val env_val of
            Nothing -> Derive.throw $ "vals can't be compared: "
                <> ShowVal.show_val val <> " " <> ShowVal.show_val env_val
            Just t -> return t
