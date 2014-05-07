-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
-- | Evaluate tracklang expressions.
module Derive.Eval (
    -- * eval / apply
    apply_toplevel
    , reapply_generator, apply_transformers, reapply_transformer
    , eval, apply

    -- * lookup call
    , unknown_call_id, symbol_to_block_id, is_relative_call

    -- * util
    , eval_one, eval_one_call, eval_one_at
    , eval_event, reapply_gen, reapply_gen_normalized
    , reapply, reapply_string, reapply_call, eval_pitch, apply_pitch
    , eval_expr, apply_transform

    -- * misc
    , cast
) where
import qualified Data.Char as Char
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text

import Util.Control
import qualified Util.Seq as Seq
import qualified Ui.Event as Event
import qualified Ui.Id as Id
import qualified Ui.State as State

import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.LEvent as LEvent
import qualified Derive.Parse as Parse
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.ShowVal as ShowVal
import qualified Derive.TrackLang as TrackLang

import Types


type CallInfo d = Derive.CallInfo (Derive.Elem d)
type PassedArgs d = Derive.PassedArgs (Derive.Elem d)

-- * eval / apply

-- | Apply a toplevel expression.
apply_toplevel :: Derive.Callable d => CallInfo d -> TrackLang.Expr
    -> Derive.LogsDeriver d
apply_toplevel cinfo expr = apply_transformers cinfo transform_calls $
    apply_generator cinfo generator_call
    where (transform_calls, generator_call) = Seq.ne_viewr expr

apply_generator :: forall d. Derive.Callable d => CallInfo d
    -> TrackLang.Call -> Derive.LogsDeriver d
apply_generator cinfo (TrackLang.Call call_id args) = do
    vals <- mapM (eval cinfo) args
    reapply_generator cinfo call_id vals (Derive.info_expr cinfo)

-- | Like 'apply_generator', but for when the args are already parsed and
-- evaluated.  This is useful when one generator wants to dispatch to another.
--
-- In addition to a call and args, this also requires the expression that
-- the call and args represents.  The reason is complicated and annoying:
-- 'Derive.info_expr' must have the expression currently being evaluated,
-- because it's used later by inversion.  Since I'm evaluating a new call_id
-- and args, and the cinfo is likely reused from another call, the @info_expr@
-- is probably wrong.  Unfortunately, I can't "unparse" a call_id and args back
-- to an expr, because pitches are code and can't necessarily be returned to
-- the expression from whence they sprang.
--
-- The reason @info_expr@ is unparsed text is also thanks to pitch signal
-- expressions.  Maybe I should get rid of them?
reapply_generator :: Derive.Callable d => CallInfo d
    -> TrackLang.CallId -> [TrackLang.Val] -> Event.Text -> Derive.LogsDeriver d
reapply_generator cinfo call_id args expr = do
    call <- get_generator call_id
    let passed = Derive.PassedArgs
            { Derive.passed_vals = args
            , Derive.passed_call_name = Derive.call_name call
            , Derive.passed_info = cinfo { Derive.info_expr = expr  }
            }
    Internal.with_stack_call (Derive.call_name call) $
        Derive.call_func call passed

apply_transformers :: Derive.Callable d => CallInfo d
    -> [TrackLang.Call] -> Derive.LogsDeriver d
    -> Derive.LogsDeriver d
apply_transformers _ [] deriver = deriver
apply_transformers cinfo (TrackLang.Call call_id args : calls) deriver = do
    vals <- mapM (eval cinfo) args
    call <- get_transformer call_id
    let under_invert = Derive.cdoc_tags (Derive.call_doc call)
            `Tags.contains` Tags.under_invert
        inverted = Derive.info_inverted cinfo
        -- If there are no subs and I'm not inverted, then inversion won't
        -- happen, so I'd better run it anyway.
        skip = not $ inverted == under_invert
            || null (Derive.info_sub_tracks cinfo) && not inverted
    let passed = Derive.PassedArgs
            { Derive.passed_vals = vals
            , Derive.passed_call_name = Derive.call_name call
            , Derive.passed_info = cinfo
            }
    let rest = apply_transformers cinfo calls deriver
    if skip then rest
        else Internal.with_stack_call (Derive.call_name call) $
            Derive.call_func call passed rest

-- | The transformer version of 'reapply_generator'.  Like
-- 'apply_transformers', but apply only one, and apply to already
-- evaluated 'TrackLang.Val's.  This is useful when you want to re-apply an
-- already parsed set of vals.
reapply_transformer :: (Derive.Callable d) => CallInfo d
    -> TrackLang.CallId -> [TrackLang.Val] -> Derive.LogsDeriver d
    -> Derive.LogsDeriver d
reapply_transformer cinfo call_id args deriver = do
    call <- get_transformer call_id
    let passed = Derive.PassedArgs
            { Derive.passed_vals = args
            , Derive.passed_call_name = Derive.call_name call
            , Derive.passed_info = cinfo
            }
    Internal.with_stack_call (Derive.call_name call) $
        Derive.call_func call passed deriver

eval :: (Derive.ToTagged a) => Derive.CallInfo a -> TrackLang.Term
    -> Derive.Deriver TrackLang.Val
eval _ (TrackLang.Literal val) = return val
eval cinfo (TrackLang.ValCall (TrackLang.Call call_id terms)) = do
    call <- get_val_call call_id
    apply (Derive.tag_call_info cinfo) call terms

apply :: Derive.CallInfo Derive.Tagged -> Derive.ValCall
    -> [TrackLang.Term] -> Derive.Deriver TrackLang.Val
apply cinfo call args = do
    vals <- mapM (eval cinfo) args
    let passed = Derive.PassedArgs
            { Derive.passed_vals = vals
            , Derive.passed_call_name = Derive.vcall_name call
            , Derive.passed_info = cinfo
            }
    Derive.vcall_call call passed

-- * lookup call

get_val_call :: TrackLang.CallId -> Derive.Deriver Derive.ValCall
get_val_call call_id =
    require_call False call_id "val call" =<< Derive.lookup_val_call call_id

get_generator :: forall d. (Derive.Callable d) =>
    TrackLang.CallId -> Derive.Deriver (Derive.Generator d)
get_generator call_id =
    require_call True call_id (name <> " generator")
        =<< Derive.lookup_generator call_id
    where name = Derive.callable_name (Proxy :: Proxy d)

get_transformer :: forall d. (Derive.Callable d) =>
    TrackLang.CallId -> Derive.Deriver (Derive.Transformer d)
get_transformer call_id =
    require_call False call_id (name <> " transformer")
        =<< Derive.lookup_transformer call_id
    where name = Derive.callable_name (Proxy :: Proxy d)

require_call :: Bool -> TrackLang.CallId -> Text -> Maybe a -> Derive.Deriver a
require_call _ _ _ (Just a) = return a
require_call is_generator call_id name Nothing = do
    -- If the call wasn't found, it can be seen as a block call whose block
    -- doesn't exist yet.  If it is created later, I have to know that this
    -- block depends on it, otherwise it won't be rederived and hence won't
    -- realize that the bad call is now valid.
    when is_generator $ do
        caller <- Internal.lookup_current_block_id
        ns <- Derive.get_ui_state $ State.config_namespace . State.state_config
        whenJust (symbol_to_block_id ns caller call_id) Internal.add_block_dep
    Derive.throw $ untxt (unknown_call_id name call_id)

unknown_call_id :: Text -> TrackLang.CallId -> Text
unknown_call_id name (TrackLang.Symbol sym) = name <> " not found: " <> sym

-- | Given a CallId, try to come up with the BlockId of the block it could be
-- a call for.
symbol_to_block_id :: Id.Namespace -> Maybe BlockId
    -- ^ If the symbol starts with ., this block is prepended to it.
    -> TrackLang.CallId -> Maybe BlockId
symbol_to_block_id ns maybe_caller sym
    | sym == "" = Nothing
    | otherwise = Just $ Id.BlockId $ Id.read_short ns relative
    where
    relative
        | Just caller <- maybe_caller, is_relative_call sym =
            Id.ident_text caller <> TrackLang.unsym sym
        | otherwise = TrackLang.unsym sym

is_relative_call :: TrackLang.CallId -> Bool
is_relative_call (TrackLang.Symbol sym) = "." `Text.isPrefixOf` sym

-- * util

-- | Evaluate a single note as a generator.  Fake up an event with no prev or
-- next lists.
eval_one :: Derive.Callable d => Bool -> TrackLang.Expr -> Derive.LogsDeriver d
eval_one collect = eval_one_at collect 0 1

eval_one_call :: Derive.Callable d => Bool -> TrackLang.Call
    -> Derive.LogsDeriver d
eval_one_call collect = eval_one collect . (:| [])

eval_one_at :: Derive.Callable d => Bool -> ScoreTime -> ScoreTime
    -> TrackLang.Expr -> Derive.LogsDeriver d
eval_one_at collect start dur expr = eval_expr collect cinfo expr
    where
    -- Set the event start and duration instead of using Derive.place since
    -- this way I can have zero duration events.
    cinfo = Derive.dummy_call_info start dur $
        "eval_one: " <> ShowVal.show_val expr

-- | Like 'derive_event' but evaluate the event outside of its track context.
-- This is useful if you want to evaluate things out of order, i.e. evaluate
-- the /next/ pitch.
eval_event :: Derive.Callable d => Event.Event
    -> Derive.Deriver (Either String (LEvent.LEvents d))
eval_event event = case Parse.parse_expr (Event.event_text event) of
    Left err -> return $ Left err
    Right expr -> Right <$>
        -- TODO eval it separately to catch any exception?
        eval_one_at False (Event.start event) (Event.duration event) expr

-- | Evaluate a generator, reusing the passed args but replacing the CallId.
-- Generators can use this to delegate to other generators.
reapply_gen :: Derive.Callable d => PassedArgs d -> TrackLang.CallId
    -> Derive.LogsDeriver d
reapply_gen args call_id = do
    let cinfo = Derive.passed_info args
    -- As documented in 'reapply_generator', I need the expr that
    -- (call_id, passed_vals) corresponds to.  Since I'm reusing an existing
    -- call, it's probably safe to reuse its expr, swapping out the call_id.
    expr <- Derive.require_right ("reapply_gen: unparseable info_expr: "<>) $
        replace_generator call_id (Derive.info_expr cinfo)
    reapply_generator cinfo call_id (Derive.passed_vals args) expr

-- | Like 'reapply_gen', but the note is given normalized time, 0--1, instead
-- of inheriting the start and duration from the args.  This is essential if
-- you want to shift or stretch the note.
reapply_gen_normalized :: Derive.Callable d => PassedArgs d -> TrackLang.CallId
    -> Derive.LogsDeriver d
reapply_gen_normalized args = reapply_gen $ args
    { Derive.passed_info = cinfo
        { Derive.info_event = (Derive.info_event cinfo)
            { Event.start = 0
            , Event.duration = 1
            }
        , Derive.info_event_end = 1
        }
    }
    where cinfo = Derive.passed_info args

replace_generator :: TrackLang.CallId -> Event.Text -> Either String Event.Text
replace_generator call_id = fmap replace . Parse.parse_expr
    where
    replace = ShowVal.show_val . TrackLang.map_generator (const call_id)

-- | Apply an expr with the current call info.  This discards the parsed
-- arguments in the 'PassedArgs' since it gets args from the 'TrackLang.Expr'.
reapply :: (Derive.Callable d) => PassedArgs d -> TrackLang.Expr
    -> Derive.LogsDeriver d
reapply args = eval_expr False (Derive.passed_info args)

-- | Like 'reapply', but parse the string first.
reapply_string :: (Derive.Callable d) => PassedArgs d -> Text
    -> Derive.LogsDeriver d
reapply_string args s = case Parse.parse_expr s of
    Left err -> Derive.throw $ "parse error: " ++ err
    Right expr -> reapply args expr

reapply_call :: (Derive.Callable d) => PassedArgs d -> TrackLang.Symbol
    -> [TrackLang.Term] -> Derive.LogsDeriver d
reapply_call args call_id call_args =
    reapply args (TrackLang.call call_id call_args :| [])

-- | A version of 'eval' specialized to evaluate pitch calls.
eval_pitch :: ScoreTime -> TrackLang.PitchCall
    -> Derive.Deriver PitchSignal.Pitch
eval_pitch pos call =
    cast ("eval pitch " <> ShowVal.show_val call)
        =<< eval cinfo (TrackLang.ValCall call)
    where
    cinfo :: Derive.CallInfo PitchSignal.Pitch
    cinfo = Derive.dummy_call_info pos 0 "<eval_pitch>"
    -- Pitch calls shouldn't care about their pos.

-- | This is like 'eval_pitch' when you already know the call, presumably
-- because you asked 'Derive.scale_note_to_call'.
apply_pitch :: ScoreTime -> Derive.ValCall -> Derive.Deriver TrackLang.Val
apply_pitch pos call = apply cinfo call []
    where cinfo = Derive.dummy_call_info pos 0 "<apply_pitch>"

-- | Evaluate a single expression, catching an exception if it throws.
eval_expr :: Derive.Callable d => Bool -> CallInfo d -> TrackLang.Expr
    -> Derive.LogsDeriver d
eval_expr collect cinfo expr =
    fromMaybe [] <$> Derive.catch collect (apply_toplevel cinfo expr)

-- | Parse and apply a transform expression.
apply_transform :: (Derive.Callable d) => Text -> Text
    -> Derive.LogsDeriver d -> Derive.LogsDeriver d
apply_transform name expr_str deriver
    | Text.all Char.isSpace expr_str = deriver
    | otherwise = do
        expr <- case Parse.parse_expr expr_str of
            Left err -> Derive.throw $ untxt name ++ ": " ++ err
            Right expr -> return expr
        let info = Derive.dummy_call_info 0 1 name
        apply_transformers info (NonEmpty.toList expr) deriver

-- * misc

-- | Cast a Val to a haskell val, or throw if it's the wrong type.
cast :: forall a. TrackLang.Typecheck a => Text -> TrackLang.Val
    -> Derive.Deriver a
cast name val = case TrackLang.from_val val of
    Nothing -> Derive.throw $ untxt $
        name <> ": expected " <> prettyt return_type
        <> " but val was " <> prettyt (TrackLang.type_of val)
        <> " " <> TrackLang.show_val val
    Just a -> return a
    where return_type = TrackLang.to_type (Proxy :: Proxy a)
