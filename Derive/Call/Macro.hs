-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}
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

import qualified Util.Seq as Seq
import qualified Util.TextUtil as TextUtil
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.Eval as Eval
import qualified Derive.Parse as Parse
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Stream as Stream
import qualified Derive.ValType as ValType

import Global


generator :: Derive.Callable d => Module.Module -> Derive.CallName
    -> Tags.Tags -> Derive.Doc -> Parse.Expr -> Derive.Generator d
generator module_ name tags doc expr =
    Derive.generator module_ name tags (make_doc doc expr) $
        Sig.call (make_signature (extract_vars expr)) (generator_macro expr)

transformer :: Derive.Callable d => Module.Module -> Derive.CallName
    -> Tags.Tags -> Derive.Doc -> Parse.Expr -> Derive.Transformer d
transformer module_ name tags doc expr =
    Derive.transformer module_ name tags (make_doc doc expr) $
        Sig.callt (make_signature (extract_vars expr)) (transformer_macro expr)

val_call :: Module.Module -> Derive.CallName -> Tags.Tags -> Derive.Doc
    -> Parse.Call -> Derive.ValCall
val_call module_ name tags doc call_expr =
    Derive.make_val_call module_ name tags (make_doc doc expr) $
        Sig.call (make_signature (extract_vars expr)) (val_macro call_expr)
    where expr = Parse.Expr (call_expr :| [])

extract_vars :: Parse.Expr -> [(Parse.Var, BaseTypes.CallId, Int)]
extract_vars (Parse.Expr calls) = concatMap extract_call (NonEmpty.toList calls)
    where
    extract_call (Parse.Call call_id args) =
        concatMap (extract_arg call_id) (zip [0..] args)
    extract_arg call_id (argnum, arg) = case arg of
        Parse.VarTerm var -> [(var, call_id, argnum)]
        Parse.Literal _ -> []
        Parse.ValCall call -> extract_call call

generator_macro :: Derive.Callable d => Parse.Expr -> [BaseTypes.Val]
    -> Derive.PassedArgs d -> Derive.Deriver (Stream.Stream d)
generator_macro expr vals args = do
    expr <- Derive.require_right id $ substitute_vars vals expr
    let (trans, gen) = split_expr expr
    let ctx = Derive.passed_ctx args
    (trans_calls, trans_args) <- unzip <$> mapM (eval_args ctx) trans
    trans_calls <- mapM Eval.get_transformer trans_calls
    (gen_call, gen_args) <- eval_args ctx gen
    gen_call <- Eval.get_generator gen_call
    Eval.apply_transformers ctx (zip trans_calls trans_args) $
        Eval.apply_generator ctx gen_call gen_args

transformer_macro :: Derive.Callable d => Parse.Expr
    -> [BaseTypes.Val] -> Derive.PassedArgs d
    -> Derive.Deriver (Stream.Stream d) -> Derive.Deriver (Stream.Stream d)
transformer_macro expr vals args deriver = do
    calls <- Derive.require_right id $ substitute_vars vals expr
    let ctx = Derive.passed_ctx args
    (trans_calls, trans_args) <-
        unzip <$> mapM (eval_args ctx) (NonEmpty.toList calls)
    trans_calls <- mapM Eval.get_transformer trans_calls
    Eval.apply_transformers ctx (zip trans_calls trans_args) deriver

val_macro :: Parse.Call -> [BaseTypes.Val] -> Derive.PassedArgs Derive.Tagged
    -> Derive.Deriver BaseTypes.Val
val_macro call_expr vals args = do
    call_expr :| _ <- Derive.require_right id $
        substitute_vars vals (Parse.Expr (call_expr :| []))
    Eval.eval (Derive.passed_ctx args) (BaseTypes.ValCall call_expr)

split_expr :: BaseTypes.Expr -> ([BaseTypes.Call], BaseTypes.Call)
split_expr = Seq.ne_viewr

eval_args :: Derive.Taggable a => Derive.Context a -> BaseTypes.Call
    -> Derive.Deriver (BaseTypes.CallId, [BaseTypes.Val])
eval_args ctx (BaseTypes.Call call_id args) =
    (,) call_id <$> mapM (Eval.eval ctx) args

substitute_vars :: [BaseTypes.Val] -> Parse.Expr -> Either Text BaseTypes.Expr
substitute_vars vals (Parse.Expr calls) = run vals (mapM sub_call calls)
    where
    sub_call (Parse.Call call_id args) =
        BaseTypes.Call call_id <$> mapM sub_arg args
    sub_arg term = case term of
        Parse.VarTerm (Parse.Var _) -> BaseTypes.Literal <$> pop
        Parse.Literal val -> return (BaseTypes.Literal val)
        Parse.ValCall call -> BaseTypes.ValCall <$> sub_call call
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
make_signature :: [(Parse.Var, BaseTypes.CallId, Int)]
    -> Sig.Parser [BaseTypes.Val]
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

ordinal :: Int -> Derive.Doc
ordinal n = Derive.Doc $ showt n <> case n of
    1 -> "st"; 2 -> "nd"; 3 -> "rd"; _ -> "th"

make_doc :: Derive.Doc -> Parse.Expr -> Derive.Doc
make_doc (Derive.Doc doc) expr = Derive.Doc $
    TextUtil.joinWith "\n" ("A macro for: " <> expr_doc <> ".") doc
    where (Derive.Doc expr_doc) = ShowVal.doc expr
