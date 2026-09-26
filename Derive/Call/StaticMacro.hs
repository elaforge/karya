-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | A static macro is like a "Derive.Call.Macro", except that its calls
-- are given directly in haskell, instead of looked up as strings during
-- evaluation.  This means that the calls can't be rebound, but on the other
-- hand, it can re-export the documentation for the sub-calls.
module Derive.Call.StaticMacro (
    Call(..), Arg(..), call, literal
    , check
    , generator, transformer
) where
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.State as Monad.State
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Tuple as Tuple

import           GHC.Stack (HasCallStack)

import qualified Util.Doc as Doc
import qualified Util.Texts as Texts
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Eval as Eval
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Stream as Stream
import qualified Derive.Typecheck as Typecheck

import           Global


data Call call = Call !call ![Arg] deriving (Show)
data Arg = Var | Given !Term deriving (Show)
data Term = ValCall !Derive.ValCall ![Arg] | Literal !DeriveT.Val
    deriving (Show)

-- | A Term whose Vars have been filled in.
data ResolvedTerm =
    RValCall !Derive.ValCall ![ResolvedTerm] | RLiteral !DeriveT.Val
    deriving (Show)

call :: Derive.ValCall -> [Arg] -> Arg
call c args = Given $ ValCall c args

literal :: Typecheck.ToVal a => a -> Arg
literal = Given . Literal . Typecheck.toVal

-- | Check the output of 'generator', 'transformer', or 'val' and crash if
-- it had a statically-detectable error.  Of course I'd much rather this
-- were a type error, but it's not worth breaking out TH for it.
check :: HasCallStack => Text -> Either Text a -> a
check callName (Left err) = errorStack $ callName <> ": " <> err
check _ (Right val) = val

-- TODO implement it
requireVal :: Sig.Arg -> Derive.Deriver DeriveT.Val
requireVal (Sig.SubTrack {}) =
    Derive.throw "child tracks don't work for macros yet"
requireVal (Sig.LiteralArg arg) = pure arg

-- | Create a generator macro from a list of transformers and a generator.
generator :: Derive.CallableExpr d => Module.Module -> Derive.CallName
    -> Tags.Tags -> Doc.Doc -> [Call (Derive.Transformer d)]
    -> Call (Derive.Generator d) -> Either Text (Derive.Generator d)
generator module_ name tags doc trans gen = do
    transArgs <- concatMapM extractArgs trans
    genArgs <- extractArgs gen
    let args = transArgs ++ genArgs
    pure $ Derive.generator module_ name tags (makeDoc doc callDocs) $
        Sig.call (Sig.requiredVals args) $ \vals args -> do
            vals <- mapM requireVal vals
            generatorMacro trans gen vals (Derive.passedCtx args)
    where callDocs = map callDoc trans ++ [callDoc gen]

generatorMacro :: Derive.CallableExpr d => [Call (Derive.Transformer d)]
    -> Call (Derive.Generator d) -> [DeriveT.Val] -> Derive.Context d
    -> Derive.Deriver (Stream.Stream d)
generatorMacro trans gen vals ctx = do
    let (tcalls, transArgs) = unzip (map split trans)
    (vals, transArgs) <- pure $
        List.mapAccumL substituteVars vals transArgs
    (vals, genArgs) <- pure $ substituteVars vals (snd (split gen))
    unless (null vals) $ Derive.throw "more args than $vars"
    transArgs <- mapM (mapM (evalTerm ctx)) transArgs
    genArgs <- mapM (evalTerm ctx) genArgs
    Eval.applyTransformers ctx (zip tcalls transArgs) $
        Eval.applyGenerator ctx (fst (split gen)) genArgs
    where
    split (Call call args) = (call, args)

transformer :: Derive.CallableExpr d => Module.Module -> Derive.CallName
    -> Tags.Tags -> Doc.Doc -> [Call (Derive.Transformer d)]
    -- ^ Apply these transformers. This is in compose order, so the last one
    -- is applied first.
    -> Either Text (Derive.Transformer d)
transformer module_ name tags doc trans = do
    args <- concatMapM extractArgs trans
    pure $ Derive.transformer module_ name tags (makeDoc doc callDocs) $
        Sig.callt (Sig.requiredVals args) $ \vals args deriver -> do
            vals <- mapM requireVal vals
            transformerMacro trans vals (Derive.passedCtx args) deriver
    where callDocs = map callDoc trans

transformerMacro :: Derive.CallableExpr d => [Call (Derive.Transformer d)]
    -> [DeriveT.Val] -> Derive.Context d
    -> Derive.Deriver (Stream.Stream d) -> Derive.Deriver (Stream.Stream d)
transformerMacro trans vals ctx deriver = do
    let (tcalls, transArgs) = unzip (map split trans)
    (vals, transArgs) <- pure $
        List.mapAccumL substituteVars vals transArgs
    unless (null vals) $ Derive.throw "more args than $vars"
    transArgs <- mapM (mapM (evalTerm ctx)) transArgs
    Eval.applyTransformers ctx (zip tcalls transArgs) deriver
    where
    split (Call call args) = (call, args)

evalTerm :: Derive.Taggable a => Derive.Context a -> ResolvedTerm
    -> Derive.Deriver DeriveT.Val
evalTerm _ (RLiteral val) = pure val
evalTerm ctx (RValCall call terms) = do
    vals <- mapM (evalTerm ctx) terms
    let passed = Derive.PassedArgs
            { passedVals = vals
            , passedCallName = Derive.callName call
            , passedCtx = Derive.tagContext ctx
            }
    Derive.callFunc call passed

-- | Substitute the given Vals into the non-'Given' Args.
substituteVars :: [DeriveT.Val] -> [Arg] -> ([DeriveT.Val], [ResolvedTerm])
    -- ^ (remaining_vals, substituted)
substituteVars allVals args = run (mapM substArg args)
    where
    substArg arg = case arg of
        Var -> RLiteral <$> pop
        Given (Literal val) -> pure (RLiteral val)
        Given (ValCall call args) -> RValCall call <$> mapM substArg args
    pop = do
        vals <- Monad.State.get
        case vals of
            -- This allows the sub-call look in the environ for a default.
            [] -> pure DeriveT.VNotGiven
            v : vs -> Monad.State.put vs >> pure v
    run = Tuple.swap . Identity.runIdentity
        . flip Monad.State.runStateT allVals

-- Look for Vars, and get the corresponding ArgDoc.
extractArgs :: Call (Derive.Call f) -> Either Text [Derive.ArgDoc]
extractArgs (Call call args) = extract (Derive.callDoc call) args
    where
    extract :: Derive.CallDoc -> [Arg] -> Either Text [Derive.ArgDoc]
    extract cdoc args
        | length args > length docs = Left $
            "call can take up to " <> showt (length docs)
            <> " args, but was given " <> showt (length args)
        | otherwise = concatMapM extractArg (zip docs args)
        where
        docs = Derive.cdocArgs cdoc
        extractArg (doc, arg) = case arg of
            Var -> Right [doc]
            Given (Literal _) -> Right []
            Given (ValCall call args) -> extract (Derive.callDoc call) args

-- ** doc

makeDoc :: Doc.Doc -> [Doc.Doc] -> Doc.Doc
makeDoc doc calls = Texts.unlines2 doc $
    "A static macro for: `" <> Texts.join " | " calls <> "`.\
    \\nEach `$` is lifted to be an argument of this macro.\
    \\nThis directly calls the underlying sub-calls, so it's not dependent on\
    \ the names they are bound to, which also means the macro text may not be a\
    \ valid expression."

callDoc :: Call (Derive.Call f) -> Doc.Doc
callDoc (Call call args) = Doc.Doc $ Text.unwords $ name : map argDoc args
    where Derive.CallName name = Derive.callName call

argDoc :: Arg -> Text
argDoc (Given (Literal val)) = ShowVal.showVal val
argDoc (Given (ValCall call args)) =
    "(" <> Text.unwords (name : map argDoc args) <> ")"
    where Derive.CallName name = Derive.callName call
argDoc Var = "$"
