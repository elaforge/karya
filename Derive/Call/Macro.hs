-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
-- | Create macros, which are calls that can substitute arguments into
-- an expression.  E.g. @apply-start-offset | start-s = (cf-rnd-a $distance)@.
module Derive.Call.Macro (
    generator, transformer, val_call
#ifdef TESTING
    , module Derive.Call.Macro
#endif
) where
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.State as Monad.State

import qualified Data.List.NonEmpty as NonEmpty

import qualified Util.Doc as Doc
import qualified Util.Seq as Seq
import qualified Util.Texts as Texts

import qualified Derive.Call.Module as Module
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Eval as Eval
import qualified Derive.Expr as Expr
import qualified Derive.Parse as Parse
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Stream as Stream
import qualified Derive.ValType as ValType

import           Global


generator :: Derive.CallableExpr d => Module.Module -> Derive.CallName
    -> Tags.Tags -> Doc.Doc -> Parse.Expr -> Derive.Generator d
generator module_ name tags doc expr =
    Derive.generator module_ name tags (make_doc doc expr) $
        Sig.call (make_signature (extract_vars expr)) (generator_macro expr)

transformer :: Derive.CallableExpr d => Module.Module -> Derive.CallName
    -> Tags.Tags -> Doc.Doc -> Parse.Expr -> Derive.Transformer d
transformer module_ name tags doc expr =
    Derive.transformer module_ name tags (make_doc doc expr) $
        Sig.callt (make_signature (extract_vars expr)) (transformer_macro expr)

val_call :: Module.Module -> Derive.CallName -> Tags.Tags -> Doc.Doc
    -> Parse.Call -> Derive.ValCall
val_call module_ name tags doc call_expr =
    Derive.make_val_call module_ name tags (make_doc doc expr) $
        Sig.call (make_signature (extract_vars expr)) (val_macro call_expr)
    where expr = Parse.Expr (call_expr :| [])

extract_vars :: Parse.Expr -> [(Parse.Var, Expr.Symbol, Int)]
extract_vars (Parse.Expr calls) = concatMap extract_call (NonEmpty.toList calls)
    where
    extract_call (Parse.Call sym args) =
        concatMap (extract_arg sym) (zip [0..] args)
    extract_arg sym (argnum, arg) = case arg of
        Parse.VarTerm var -> [(var, sym, argnum)]
        Parse.Literal _ -> []
        Parse.ValCall call -> extract_call call

generator_macro :: Derive.CallableExpr d => Parse.Expr -> [Sig.Arg]
    -> Derive.PassedArgs d -> Derive.Deriver (Stream.Stream d)
generator_macro expr vals args = do
    vals <- mapM require_val vals
    expr <- Derive.require_right id $ substitute_vars vals expr
    let (trans, gen) = split_expr expr
    let ctx = Derive.passed_ctx args
    (trans_calls, trans_args) <- unzip <$> mapM (eval_args ctx) trans
    trans_calls <- mapM Eval.get_transformer trans_calls
    (gen_call, gen_args) <- eval_args ctx gen
    gen_call <- Eval.get_generator gen_call
    Eval.apply_transformers ctx (zip trans_calls trans_args) $
        Eval.apply_generator ctx gen_call gen_args

transformer_macro :: Derive.CallableExpr d => Parse.Expr
    -> [Sig.Arg] -> Derive.PassedArgs d
    -> Derive.Deriver (Stream.Stream d) -> Derive.Deriver (Stream.Stream d)
transformer_macro expr vals args deriver = do
    vals <- mapM require_val vals
    calls <- Derive.require_right id $ substitute_vars vals expr
    let ctx = Derive.passed_ctx args
    (trans_calls, trans_args) <-
        unzip <$> mapM (eval_args ctx) (NonEmpty.toList calls)
    trans_calls <- mapM Eval.get_transformer trans_calls
    Eval.apply_transformers ctx (zip trans_calls trans_args) deriver

val_macro :: Parse.Call -> [Sig.Arg] -> Derive.PassedArgs Derive.Tagged
    -> Derive.Deriver DeriveT.Val
val_macro call_expr vals args = do
    vals <- mapM require_val vals
    call_expr :| _ <- Derive.require_right id $
        substitute_vars vals (Parse.Expr (call_expr :| []))
    Eval.eval (Derive.passed_ctx args) (Expr.ValCall call_expr)

split_expr :: DeriveT.Expr -> ([DeriveT.Call], DeriveT.Call)
split_expr = Seq.ne_viewr

eval_args :: Derive.Taggable a => Derive.Context a -> DeriveT.Call
    -> Derive.Deriver (Expr.Symbol, [DeriveT.Val])
eval_args ctx (Expr.Call sym args) = (,) sym <$> mapM (Eval.eval ctx) args

substitute_vars :: [DeriveT.Val] -> Parse.Expr -> Either Text DeriveT.Expr
substitute_vars vals (Parse.Expr calls) = run vals (mapM sub_call calls)
    where
    sub_call (Parse.Call sym args) = Expr.Call sym <$> mapM sub_arg args
    sub_arg term = case term of
        Parse.VarTerm (Parse.Var _) -> Expr.Literal <$> pop
        Parse.Literal val -> return (Expr.Literal val)
        Parse.ValCall call -> Expr.ValCall <$> sub_call call
    pop = do
        vals <- Monad.State.get
        case vals of
            [] -> Except.throwError "ran out of vals"
            v : vs -> Monad.State.put vs >> return v
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
make_signature :: [(Parse.Var, Expr.Symbol, Int)] -> Sig.Parser [Sig.Arg]
make_signature vars = Sig.required_vals (map doc vars)
    where
    doc (Parse.Var var, call, argnum) = Derive.ArgDoc
        { arg_name = Derive.ArgName var
        , arg_type = ValType.TVal
        , arg_parser = Derive.Required
        , arg_environ_default = Derive.Prefixed
        , arg_doc = "Passed to " <> ShowVal.doc call <> "'s "
            <> ordinal (argnum+1) <> " argument."
        }

-- TODO implement it
require_val :: Sig.Arg -> Derive.Deriver DeriveT.Val
require_val (Sig.SubTrack {}) =
    Derive.throw "child tracks don't work for macros yet"
require_val (Sig.LiteralArg arg) = return arg

ordinal :: Int -> Doc.Doc
ordinal n = Doc.Doc $ showt n <> case n of
    1 -> "st"; 2 -> "nd"; 3 -> "rd"; _ -> "th"

make_doc :: Doc.Doc -> Parse.Expr -> Doc.Doc
make_doc (Doc.Doc doc) expr = Doc.Doc $
    Texts.joinWith "\n" ("A macro for: " <> expr_doc <> ".") doc
    where (Doc.Doc expr_doc) = ShowVal.doc expr
