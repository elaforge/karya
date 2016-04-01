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
data Call var = Call BaseTypes.CallId [Either var BaseTypes.Term]
    deriving (Show)
data Var = Var Text deriving (Show)

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
extract_vars (Expr calls) = concatMap extract (NonEmpty.toList calls)
    where
    extract (Call call_id args) =
        [(var, call_id, argnum) | (argnum, Left var) <- zip [0..] args]

generator_macro :: Derive.Callable d => Expr Var -> [BaseTypes.Val]
    -> Derive.PassedArgs d -> Derive.Deriver (Stream.Stream d)
generator_macro expr vals args = do
    expr <- Derive.require_right id $ substitute_vars vals expr
    let (trans, gen) = split_expr expr
    let ctx = Derive.passed_ctx args
    trans <- mapM (eval_args ctx) trans
    (gen_call, gen_args) <- eval_args ctx gen
    Eval.apply_transformers ctx trans $
        Eval.apply_generator ctx gen_call gen_args

transformer_macro :: Derive.Callable d => Expr Var -> [BaseTypes.Val]
    -> Derive.PassedArgs d
    -> Derive.Deriver (Stream.Stream d) -> Derive.Deriver (Stream.Stream d)
transformer_macro expr vals args deriver = do
    Expr calls <- Derive.require_right id $ substitute_vars vals expr
    let ctx = Derive.passed_ctx args
    trans <- mapM (eval_args ctx) (NonEmpty.toList calls)
    Eval.apply_transformers ctx trans deriver

split_expr :: Expr a -> ([Call a], Call a)
split_expr (Expr calls) = Seq.ne_viewr calls

eval_args :: Derive.Taggable a => Derive.Context a -> Call BaseTypes.Val
    -> Derive.Deriver (BaseTypes.CallId, [BaseTypes.Val])
eval_args ctx (Call call_id args) =
    (,) call_id <$> mapM (either return (Eval.eval ctx)) args

substitute_vars :: [BaseTypes.Val] -> Expr Var
    -> Either Text (Expr BaseTypes.Val)
substitute_vars vals (Expr calls) = run vals (Expr <$> mapM sub_call calls)
    where
    sub_call (Call call_id args) = Call call_id <$> mapM sub_arg args
    sub_arg (Left (Var _)) = Left <$> pop
    sub_arg (Right term) = return (Right term)
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

instance ShowVal.ShowVal Var where
    show_val (Var name) = "$" <> name
