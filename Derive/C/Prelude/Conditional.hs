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
    [ Library.transformers [("solo", cSolo)]
    , Library.polyGenerators
        [ ("if-e", cIfE)
        , ("if-c<", cIfC (<))
        , ("if-c>", cIfC (>))
        , ("on-repeat", cOnRepeat)
        ]
    , Library.polyTransformers
        [ ("when-c", cWhenC False)
        , ("unless-c", cWhenC True)
        , ("when-e", cWhenE False)
        , ("unless-e", cWhenE True)
        ]
    ]

-- * generator

cIfE :: Derive.CallableExpr d => Derive.Generator d
cIfE = Derive.generator Module.prelude "if-e" mempty
    "Derive based on the value of an environment variable."
    $ Sig.call ((,,,)
    <$> Sig.required "name" "Environ key."
    <*> Sig.defaulted "value" (Nothing :: Maybe DeriveT.Val)
        "Environ value. If not given, require\
        \ only that the environ key is set."
    <*> Sig.required "true" "Eval if true."
    <*> Sig.required "false" "Eval if false."
    ) $ \(name, maybeValue, true, false) args ->
        ifM (hasEnviron name maybeValue)
            (Eval.evalQuoted (Args.context args) true)
            (Eval.evalQuoted (Args.context args) false)

cIfC :: Derive.CallableExpr d => (Signal.Y -> Signal.Y -> Bool)
    -> Derive.Generator d
cIfC cmp = Derive.generator Module.prelude "if-c<" mempty
    "Derive based on the value of a control."
    $ Sig.call ((,)
    <$> Sig.required "control" "Test this control."
    <*> Sig.many1 "tests" "(value, expr) pairs."
    ) $ \(control, tests) args -> do
        val <- fromMaybe 0 <$>
            (Derive.untypedControlAt control =<< Args.realStart args)
        (tests, final) <- typecheckTests (Args.start args)
            (NonEmpty.toList tests)
        Eval.evalQuoted (Args.context args) $ maybe final snd $
            List.find (cmp val . fst) tests

typecheckTests :: ScoreTime -> [DeriveT.Val]
    -> Derive.Deriver ([(Signal.Y, DeriveT.Quoted)], DeriveT.Quoted)
typecheckTests start = go
    where
    go [] = Derive.throw "not enough values"
    go [x] = do
        final <- typecheck x
        pure ([], final)
    go (val : result : rest) = do
        checked <- (,) <$> typecheck val <*> typecheck result
        (rest, final) <- go rest
        pure (checked : rest, final)
    typecheck :: Typecheck.Typecheck a => DeriveT.Val -> Derive.Deriver a
    typecheck = Typecheck.typecheck "" start

cOnRepeat :: Derive.CallableExpr d => Derive.Generator d
cOnRepeat = Derive.generator Module.prelude "on-repeat" mempty
    "Derive the argument indexed by the `repeat` variable, where an out of\
    \ range index is clamped to be in range."
    $ Sig.call (Sig.many1 "repeat" "Eval on nth repeat.") $ \repeats args -> do
        repeat <- Derive.lookupVal "repeat"
        Eval.evalQuoted (Args.context args) (at repeats (fromMaybe 0 repeat))

at :: NonEmpty a -> Int -> a
at (x0 :| x1: xs) i
    | i <= 0 = x0
    | otherwise = at (x1 :| xs) (i-1)
at (x0 :| []) _ = x0

-- * transformer

cSolo :: Derive.Transformer Derive.Note
cSolo = Derive.transformer Module.prelude "solo" mempty
    "Only derive if `inst` is set to the given value. This is a specialized\
    \ version of `when-e`."
    $ Sig.callt (Sig.required "inst" "Instrument.")
    $ \inst _args deriver ->
        ifM (hasEnviron EnvKey.instrument
                (Just (Typecheck.toVal (inst :: ScoreT.Instrument))))
            deriver mempty

cWhenC :: Derive.Taggable d => Bool -> Derive.Transformer d
cWhenC inverted = Derive.transformer Module.prelude "when-c" mempty
    "Only derive if the control has the given value. E.g., you can use a\
    \ `%var` control to select among different variations."
    $ Sig.callt ((,)
    <$> Sig.required "val" "Value."
    <*> Sig.defaulted "control" (0 :: Double) "Control."
    ) $ \(val :: Int, control :: Double) _args deriver ->
        if invert (round control == val)
            then deriver else pure Stream.empty
    where invert = if inverted then not else id

cWhenE :: Derive.Taggable d => Bool -> Derive.Transformer d
cWhenE inverted = Derive.transformer Module.prelude "when-e" mempty
    "Only derive if environ value is set to the given value. In a block\
    \ derived multiple times by different instruments, this can be used to\
    \ solo a bit of score to one particular instrument."
    $ Sig.callt ((,)
    <$> Sig.required "name" "Environ key."
    <*> Sig.defaulted "value" (Nothing :: Maybe DeriveT.Val)
        "Environ value. If not given, require\
        \ only that the environ key is set."
    ) $ \(name, maybeValue) _args deriver ->
        ifM (invert $ hasEnviron name maybeValue) deriver
            (pure Stream.empty)
    where invert = if inverted then (not <$>) else id

hasEnviron :: Env.Key -> Maybe DeriveT.Val -> Derive.Deriver Bool
hasEnviron name maybeVal = Derive.lookupVal name >>= \case
    Nothing -> pure False
    Just envVal -> case maybeVal of
        Nothing -> pure True
        Just val -> case DeriveT.valsEqual val envVal of
            Nothing -> Derive.throw $ "vals can't be compared: "
                <> ShowVal.showVal val <> " " <> ShowVal.showVal envVal
            Just t -> pure t
