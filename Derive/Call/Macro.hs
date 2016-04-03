-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}
-- | Create macros, which are calls that can substitute arguments into
-- an expression.  E.g. @apply-start-offset | start-s = (cf-rnd-a $distance)@.
module Derive.Call.Macro (
    generator, transformer
#ifdef TESTING
    , module Derive.Call.Macro
#endif
) where
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.State as Monad.State

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text

import qualified Util.Seq as Seq
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.Eval as Eval
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Stream as Stream
import qualified Derive.ValType as ValType

import Global


newtype Expr var = Expr (NonEmpty (Call var))
    deriving (Show)
data Call var = Call BaseTypes.CallId [Either var (Term var)]
    deriving (Show)
data Term var = ValCall (Call var) | Literal BaseTypes.Val deriving (Show)
newtype Var = Var Text deriving (Show)

generator :: Derive.Callable d => Module.Module -> Text -> Tags.Tags
    -> Expr Var -> Derive.Generator d
generator module_ name tags expr =
    Derive.generator module_ name tags (make_doc expr) $
        Sig.call (make_signature (extract_vars expr)) (generator_macro expr)

transformer :: Derive.Callable d => Module.Module -> Text -> Tags.Tags
    -> Expr Var -> Derive.Transformer d
transformer module_ name tags expr =
    Derive.transformer module_ name tags (make_doc expr) $
        Sig.callt (make_signature (extract_vars expr)) (transformer_macro expr)

extract_vars :: Expr Var -> [(Var, BaseTypes.CallId, Int)]
extract_vars (Expr calls) = concatMap extract_call (NonEmpty.toList calls)
    where
    extract_call (Call call_id args) =
        concatMap (extract_arg call_id) (zip [0..] args)
    extract_arg call_id (argnum, arg) = case arg of
        Left var -> [(var, call_id, argnum)]
        Right (Literal _) -> []
        Right (ValCall call) -> extract_call call

generator_macro :: Derive.Callable d => Expr Var -> [BaseTypes.Val]
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

transformer_macro :: Derive.Callable d => Expr Var -> [BaseTypes.Val]
    -> Derive.PassedArgs d
    -> Derive.Deriver (Stream.Stream d) -> Derive.Deriver (Stream.Stream d)
transformer_macro expr vals args deriver = do
    calls <- Derive.require_right id $ substitute_vars vals expr
    let ctx = Derive.passed_ctx args
    (trans_calls, trans_args) <-
        unzip <$> mapM (eval_args ctx) (NonEmpty.toList calls)
    trans_calls <- mapM Eval.get_transformer trans_calls
    Eval.apply_transformers ctx (zip trans_calls trans_args) deriver

split_expr :: BaseTypes.Expr -> ([BaseTypes.Call], BaseTypes.Call)
split_expr = Seq.ne_viewr

eval_args :: Derive.Taggable a => Derive.Context a -> BaseTypes.Call
    -> Derive.Deriver (BaseTypes.CallId, [BaseTypes.Val])
eval_args ctx (BaseTypes.Call call_id args) =
    (,) call_id <$> mapM (Eval.eval ctx) args

substitute_vars :: [BaseTypes.Val] -> Expr Var -> Either Text BaseTypes.Expr
substitute_vars vals (Expr calls) = run vals (mapM sub_call calls)
    where
    sub_call (Call call_id args) = BaseTypes.Call call_id <$> mapM sub_arg args
    sub_arg (Left (Var _)) = BaseTypes.Literal <$> pop
    sub_arg (Right term) = case term of
        Literal val -> return (BaseTypes.Literal val)
        ValCall call -> BaseTypes.ValCall <$> sub_call call
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
make_signature :: [(Var, BaseTypes.CallId, Int)] -> Sig.Parser [BaseTypes.Val]
make_signature vars = Sig.many_vals (map doc vars)
    where
    doc (Var var, call, argnum) = Derive.ArgDoc
        { arg_name = var
        , arg_type = ValType.TVal
        , arg_parser = Derive.Required
        , arg_environ_default = Derive.Prefixed
        , arg_doc = "Passed to " <> ShowVal.doc call <> "'s "
            <> ordinal (argnum+1) <> " argument."
        }

ordinal :: Int -> Text
ordinal n = showt n <> case n of
    1 -> "st"; 2 -> "nd"; 3 -> "rd"; _ -> "th"

make_doc :: Expr Var -> Text
make_doc expr = "A macro for " <> ShowVal.doc expr <> "."

instance ShowVal.ShowVal a => ShowVal.ShowVal (Expr a) where
    show_val (Expr calls) = Text.intercalate " | "
        (map ShowVal.show_val (NonEmpty.toList calls))

instance ShowVal.ShowVal a => ShowVal.ShowVal (Call a) where
    show_val (Call call_id args) = Text.unwords $
        ShowVal.show_val call_id
            : map (either ShowVal.show_val ShowVal.show_val) args

instance ShowVal.ShowVal a => ShowVal.ShowVal (Term a) where
    show_val (ValCall call) = "(" <> ShowVal.show_val call <> ")"
    show_val (Literal val) = ShowVal.show_val val

instance ShowVal.ShowVal Var where
    show_val (Var name) = "$" <> name
