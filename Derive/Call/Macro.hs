-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
-- | Create macros, which are calls that can substitute arguments into
-- an expression.  E.g. @apply-start-offset | start-s = (cf-rnd-a $distance)@.
module Derive.Call.Macro (
    generator, transformer, valCall
#ifdef TESTING
    , module Derive.Call.Macro
#endif
) where
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.State as Monad.State

import qualified Data.List.NonEmpty as NonEmpty

import qualified Util.Doc as Doc
import qualified Util.NEs as NEs
import qualified Util.Texts as Texts

import qualified Derive.Call.Module as Module
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Eval as Eval
import qualified Derive.Expr as Expr
import qualified Derive.Parse.Ky as Ky
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Stream as Stream
import qualified Derive.ValType as ValType

import           Global


generator :: Derive.CallableExpr d => Module.Module -> Derive.CallName
    -> Tags.Tags -> Doc.Doc -> Ky.Expr -> Derive.Generator d
generator module_ name tags doc expr =
    Derive.generator module_ name tags (makeDoc doc expr) $
        Sig.call (makeSignature (extractVars expr)) (generatorMacro expr)

transformer :: Derive.CallableExpr d => Module.Module -> Derive.CallName
    -> Tags.Tags -> Doc.Doc -> Ky.Expr -> Derive.Transformer d
transformer module_ name tags doc expr =
    Derive.transformer module_ name tags (makeDoc doc expr) $
        Sig.callt (makeSignature (extractVars expr)) (transformerMacro expr)

valCall :: Module.Module -> Derive.CallName -> Tags.Tags -> Doc.Doc
    -> Ky.Call -> Derive.ValCall
valCall module_ name tags doc callExpr =
    Derive.makeValCall module_ name tags (makeDoc doc expr) $
        Sig.call (makeSignature (extractVars expr)) (valMacro callExpr)
    where expr = Ky.Expr (callExpr :| [])

extractVars :: Ky.Expr -> [(Ky.Var, Expr.Symbol, Int)]
extractVars (Ky.Expr calls) = concatMap extractCall (NonEmpty.toList calls)
    where
    extractCall (Ky.Call sym args) =
        concatMap (extractArg sym) (zip [0..] args)
    extractArg sym (argnum, arg) = case arg of
        Ky.VarTerm var -> [(var, sym, argnum)]
        Ky.Literal _ -> []
        Ky.ValCall call -> extractCall call

generatorMacro :: Derive.CallableExpr d => Ky.Expr -> [Sig.Arg]
    -> Derive.PassedArgs d -> Derive.Deriver (Stream.Stream d)
generatorMacro expr vals args = do
    vals <- mapM requireVal vals
    expr <- Derive.requireRight id $ substituteVars vals expr
    let (trans, gen) = splitExpr expr
    let ctx = Derive.passedCtx args
    (transCalls, transArgs) <- unzip <$> mapM (evalArgs ctx) trans
    transCalls <- mapM Eval.getTransformer transCalls
    (genCall, genArgs) <- evalArgs ctx gen
    genCall <- Eval.getGenerator genCall
    Eval.applyTransformers ctx (zip transCalls transArgs) $
        Eval.applyGenerator ctx genCall genArgs

transformerMacro :: Derive.CallableExpr d => Ky.Expr
    -> [Sig.Arg] -> Derive.PassedArgs d
    -> Derive.Deriver (Stream.Stream d) -> Derive.Deriver (Stream.Stream d)
transformerMacro expr vals args deriver = do
    vals <- mapM requireVal vals
    calls <- Derive.requireRight id $ substituteVars vals expr
    let ctx = Derive.passedCtx args
    (transCalls, transArgs) <-
        unzip <$> mapM (evalArgs ctx) (NonEmpty.toList calls)
    transCalls <- mapM Eval.getTransformer transCalls
    Eval.applyTransformers ctx (zip transCalls transArgs) deriver

valMacro :: Ky.Call -> [Sig.Arg] -> Derive.PassedArgs Derive.Tagged
    -> Derive.Deriver DeriveT.Val
valMacro callExpr vals args = do
    vals <- mapM requireVal vals
    callExpr :| _ <- Derive.requireRight id $
        substituteVars vals (Ky.Expr (callExpr :| []))
    Eval.eval (Derive.passedCtx args) (Expr.ValCall callExpr)

splitExpr :: DeriveT.Expr -> ([DeriveT.Call], DeriveT.Call)
splitExpr = NEs.unsnoc

evalArgs :: Derive.Taggable a => Derive.Context a -> DeriveT.Call
    -> Derive.Deriver (Expr.Symbol, [DeriveT.Val])
evalArgs ctx (Expr.Call sym args) = (,) sym <$> mapM (Eval.eval ctx) args

substituteVars :: [DeriveT.Val] -> Ky.Expr -> Either Text DeriveT.Expr
substituteVars vals (Ky.Expr calls) = run vals (mapM subCall calls)
    where
    subCall (Ky.Call sym args) = Expr.Call sym <$> mapM subArg args
    subArg term = case term of
        Ky.VarTerm (Ky.Var _) -> Expr.Literal <$> pop
        Ky.Literal val -> pure (Expr.Literal val)
        Ky.ValCall call -> Expr.ValCall <$> subCall call
    pop = do
        vals <- Monad.State.get
        case vals of
            [] -> Except.throwError "ran out of vals"
            v : vs -> Monad.State.put vs >> pure v
    run state = check . Identity.runIdentity . Except.runExceptT
        . flip Monad.State.runStateT state
    check (Left e) = Left e
    check (Right (a, [])) = Right a
    check (Right (_, xs)) = Left $ "left overs: " <> pretty xs

-- | Since I don't have the Sig.Parsers but just the ArgDocs, I can't check the
-- types, but I can have the right doc.  The type will be checked by the calls
-- when I apply them.
--
-- TODO these are all required, but should I support optional args?  But isn't
-- the whole point of doing this in haskell that I don't get tied up in more
-- and more hacky language features?
makeSignature :: [(Ky.Var, Expr.Symbol, Int)] -> Sig.Parser [Sig.Arg]
makeSignature vars = Sig.requiredVals (map doc vars)
    where
    doc (Ky.Var var, call, argnum) = Derive.ArgDoc
        { argName = Derive.ArgName var
        , argType = ValType.TVal
        , argParser = Derive.Required
        , argEnvironDefault = Derive.Prefixed
        , argDoc = "Passed to " <> ShowVal.doc call <> "'s "
            <> ordinal (argnum+1) <> " argument."
        }

-- TODO implement it
requireVal :: Sig.Arg -> Derive.Deriver DeriveT.Val
requireVal (Sig.SubTrack {}) =
    Derive.throw "child tracks don't work for macros yet"
requireVal (Sig.LiteralArg arg) = pure arg

ordinal :: Int -> Doc.Doc
ordinal n = Doc.Doc $ showt n <> case n of
    1 -> "st"; 2 -> "nd"; 3 -> "rd"; _ -> "th"

makeDoc :: Doc.Doc -> Ky.Expr -> Doc.Doc
makeDoc (Doc.Doc doc) expr = Doc.Doc $
    Texts.unlines2 ("A macro for: " <> exprDoc <> ".") doc
    where (Doc.Doc exprDoc) = ShowVal.doc expr
