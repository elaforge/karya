-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RankNTypes #-}
-- | Evaluate tracklang expressions.
module Derive.Eval (
    -- * eval / apply
    eval_toplevel, eval_quoted, eval_quoted_normalized, eval_expr_val
    -- ** generator
    , apply_generator
    -- ** transformer
    , eval_transformers, eval_transform_expr
    , apply_transformer, apply_transformers
    , eval_quoted_transformers
    -- ** val calls
    , eval, apply
    , get_val_call

    -- * lookup call
    , get_generator, get_transformer, get_track_call
    , unknown_symbol, call_to_block_id, block_id_to_call
    -- ** relative calls
    , is_relative, make_relative, parse_relative_id

    -- * util
    , eval_one, eval_one_call, eval_one_at
    , eval_event, reapply_generator, reapply_generator_normalized
    , reapply, reapply_call, eval_pitch, eval_note, apply_pitch
    , eval_expr
) where
import qualified Data.Char as Char
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text

import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Expr as Expr
import qualified Derive.PSignal as PSignal
import qualified Derive.Parse as Parse
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Stream as Stream
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import qualified Ui.Event as Event
import qualified Ui.Id as Id
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig

import           Global
import           Types


-- * eval / apply

-- | Apply a toplevel expression.
{-# SCC eval_toplevel #-}
eval_toplevel :: Derive.CallableExpr d => Derive.Context d
    -> DeriveT.Expr -> Derive.Deriver (Stream.Stream d)
eval_toplevel ctx expr =
    eval_transformers ctx transform_calls (eval_generator ctx generator_call)
    where (transform_calls, generator_call) = Expr.split expr

eval_quoted :: Derive.CallableExpr d => Derive.Context d -> DeriveT.Quoted
    -> Derive.Deriver (Stream.Stream d)
eval_quoted ctx (DeriveT.Quoted expr) = eval_toplevel ctx expr

-- | This is like 'eval_quoted', except that the 'Derive.ctx_event' is set to
-- (0, 1) normalized time.  This is important if you want to place the
-- resulting deriver.  Otherwise, you can use eval_quoted and the event's
-- position will fall through to the callee.
--
-- TODO this awkwardness is because events evaluate in track time, not in
-- normalized time.  Details in "Derive.EvalTrack".
eval_quoted_normalized :: Derive.CallableExpr d => Derive.Context d
    -> DeriveT.Quoted -> Derive.Deriver (Stream.Stream d)
eval_quoted_normalized = eval_quoted . normalize_event

normalize_event :: Derive.Context val -> Derive.Context val
normalize_event ctx = ctx
    { Derive.ctx_event = Event.place 0 1 (Derive.ctx_event ctx)
    , Derive.ctx_prev_events = []
    , Derive.ctx_next_events = []
    }

-- | Eval a Expr MiniVal, which is produced by 'Expr.ToExpr'.
eval_expr_val :: Derive.CallableExpr d => Derive.Context d
    -> Expr.Expr Expr.MiniVal -> Derive.Deriver (Stream.Stream d)
eval_expr_val ctx expr = eval_toplevel ctx (convert_minival expr)

-- TODO find a better place for this, or get rid of MiniVal
convert_minival :: Expr.Expr Expr.MiniVal -> DeriveT.Expr
convert_minival = fmap $ fmap $ \case
    Expr.VNum v -> DeriveT.VSignal $ Signal.constant <$> v
    Expr.VStr v -> DeriveT.VStr v

-- ** generator

{-# SCC eval_generator #-}
eval_generator :: (Derive.Callable (Derive.Generator d), Derive.Taggable d)
    => Derive.Context d -> DeriveT.Call -> Derive.Deriver (Stream.Stream d)
eval_generator ctx (Expr.Call sym args) = do
    vals <- mapM (eval ctx) args
    call <- get_generator sym
    apply_generator ctx call vals

-- | Like 'eval_generator', but for when the args are already parsed and
-- evaluated.  This is useful when one generator wants to dispatch to another.
{-# SCC apply_generator #-}
apply_generator :: Derive.Context d -> Derive.Generator d -> [DeriveT.Val]
    -> Derive.Deriver (Stream.Stream d)
apply_generator ctx call args = do
    let passed = Derive.PassedArgs
            { Derive.passed_vals = args
            , Derive.passed_call_name = Derive.call_name call
            , Derive.passed_ctx = ctx
            }
    mode <- Derive.get_mode
    Internal.with_stack_call (Derive.call_name call) $ case mode of
        Derive.ScoreDurationQuery -> do
            dur <- Derive.gfunc_score_duration (Derive.call_func call) passed
            set_score_duration dur
            return Stream.empty
        Derive.RealDurationQuery -> do
            dur <- Derive.gfunc_real_duration (Derive.call_func call) passed
            set_real_duration dur
            return Stream.empty
        _ -> do
            -- Ensure a unique serial number for each generator call, as
            -- documneted in 'Stack.Serial'.
            serial <- Derive.gets $
                Derive.state_event_serial . Derive.state_threaded
            Internal.with_stack_serial serial $
                Derive.gfunc_f (Derive.call_func call) passed

-- | See 'Derive.CallDuration' for details.
set_score_duration :: Derive.CallDuration ScoreTime -> Derive.Deriver ()
set_score_duration dur = Internal.modify_collect $ \collect ->
    collect { Derive.collect_score_duration = dur }

set_real_duration :: Derive.CallDuration RealTime -> Derive.Deriver ()
set_real_duration dur = Internal.modify_collect $ \collect ->
    collect { Derive.collect_real_duration = dur }

-- ** transformer

{-# SCC eval_transformers #-}
eval_transformers :: (Derive.Callable (Derive.Transformer d), Derive.Taggable d)
    => Derive.Context d -> [DeriveT.Call] -> Derive.Deriver (Stream.Stream d)
    -> Derive.Deriver (Stream.Stream d)
eval_transformers ctx calls deriver = go calls
    where
    go [] = deriver
    go (Expr.Call sym args : calls) = do
        vals <- mapM (eval ctx) args
        call <- get_transformer sym
        apply_transformer ctx call vals (go calls)

-- | Parse and apply a transformer expression.
eval_transform_expr ::
    (Derive.Callable (Derive.Transformer d), Derive.Taggable d)
    => Text -> Text -> Derive.Deriver (Stream.Stream d)
    -> Derive.Deriver (Stream.Stream d)
eval_transform_expr name expr_str deriver
    | Text.all Char.isSpace expr_str = deriver
    | otherwise = do
        expr <- case Parse.parse_expr expr_str of
            Left err -> Derive.throw $ name <> ": " <> err
            Right expr -> return expr
        let ctx = Derive.dummy_context 0 1 name
        eval_transformers ctx (NonEmpty.toList expr) deriver

-- | The same as 'eval_transformers', but get them out of a Quoted.
eval_quoted_transformers ::
    (Derive.Callable (Derive.Transformer d), Derive.Taggable d)
    => Derive.Context d -> DeriveT.Quoted -> Derive.Deriver (Stream.Stream d)
    -> Derive.Deriver (Stream.Stream d)
eval_quoted_transformers ctx (DeriveT.Quoted expr) =
    eval_transformers ctx (NonEmpty.toList expr)

-- | The transformer version of 'apply_generator'.  Like 'eval_transformers',
-- but apply only one, and apply to already evaluated 'DeriveT.Val's.  This
-- is useful when you want to re-apply an already parsed set of vals.
{-# SCC apply_transformer #-}
apply_transformer :: Derive.Context d -> Derive.Transformer d -> [DeriveT.Val]
    -> Derive.Deriver (Stream.Stream d) -> Derive.Deriver (Stream.Stream d)
apply_transformer ctx call args deriver =
    Internal.with_stack_call (Derive.call_name call) $
        Derive.call_func call passed deriver
    where
    passed = Derive.PassedArgs
        { passed_vals = args
        , passed_call_name = Derive.call_name call
        , passed_ctx = ctx
        }

-- | A list version of 'apply_transformer'.
apply_transformers :: Derive.Context d
    -> [(Derive.Transformer d, [DeriveT.Val])]
    -> Derive.Deriver (Stream.Stream d) -> Derive.Deriver (Stream.Stream d)
apply_transformers ctx calls deriver = foldr apply deriver calls
    where apply (sym, args) = apply_transformer ctx sym args

-- ** val call

{-# SCC eval #-}
eval :: Derive.Taggable a => Derive.Context a -> DeriveT.Term
    -> Derive.Deriver DeriveT.Val
eval _ (Expr.Literal val) = return val
eval ctx (Expr.ValCall (Expr.Call sym terms)) = do
    call <- get_val_call sym
    apply (Derive.tag_context ctx) call terms

{-# SCC apply #-}
apply :: Derive.Context Derive.Tagged -> Derive.ValCall
    -> [DeriveT.Term] -> Derive.Deriver DeriveT.Val
apply ctx call args = do
    vals <- mapM (eval ctx) args
    let passed = Derive.PassedArgs
            { passed_vals = vals
            , passed_call_name = Derive.vcall_name call
            , passed_ctx = ctx
            }
    Internal.with_stack_call (Derive.vcall_name call) $
        Derive.vcall_call call passed

-- * lookup call

get_val_call :: Expr.Symbol -> Derive.Deriver Derive.ValCall
get_val_call sym = require_call False sym name =<< Derive.lookup_call sym
    where name = Derive.callable_name (Proxy :: Proxy Derive.ValCall)

get_generator :: forall d. Derive.Callable (Derive.Generator d) =>
    Expr.Symbol -> Derive.Deriver (Derive.Generator d)
get_generator sym = require_call True sym name =<< Derive.lookup_call sym
    where name = Derive.callable_name (Proxy :: Proxy (Derive.Generator d))

get_transformer :: forall d. Derive.Callable (Derive.Transformer d) =>
    Expr.Symbol -> Derive.Deriver (Derive.Transformer d)
get_transformer sym =
    require_call False sym name =<< Derive.lookup_call sym
    where name = Derive.callable_name (Proxy :: Proxy (Derive.Transformer d))

get_track_call :: forall d. Derive.Callable (Derive.TrackCall d) =>
    Expr.Symbol -> Derive.Deriver (Derive.TrackCall d)
get_track_call sym =
    require_call False sym name =<< Derive.lookup_call sym
    where name = Derive.callable_name (Proxy :: Proxy (Derive.TrackCall d))

require_call :: Bool -> Expr.Symbol -> Text -> Maybe a -> Derive.Deriver a
require_call _ _ _ (Just a) = return a
require_call is_generator sym name Nothing = do
    -- If the call wasn't found, it can be seen as a block call whose block
    -- doesn't exist yet.  If it is created later, I have to know that this
    -- block depends on it, otherwise it won't be rederived and hence won't
    -- realize that the bad call is now valid.
    when is_generator $ do
        caller <- Internal.lookup_current_block_id
        ns <- Derive.get_ui_state $ UiConfig.config_namespace . Ui.state_config
        whenJust (call_to_block_id ns caller sym) Internal.add_block_dep
    Derive.throw $ unknown_symbol name sym

unknown_symbol :: Text -> Expr.Symbol -> Text
unknown_symbol name sym = name <> " not found: " <> ShowVal.show_val sym

-- | Given a Symbol, try to come up with the BlockId of the block it could be
-- a call for.
call_to_block_id :: Id.Namespace -> Maybe BlockId
    -- ^ If the symbol starts with -, this block is prepended to it.
    -> Expr.Symbol -> Maybe BlockId
call_to_block_id ns maybe_caller sym
    | sym == "" = Nothing
    | otherwise = Just $ Id.BlockId $ Id.read_short ns relative
    where
    relative
        | Just caller <- maybe_caller, is_relative sym =
            Id.ident_text caller <> Expr.unsym sym
        | otherwise = Expr.unsym sym

-- | Create the symbol to call a given block.
block_id_to_call :: Bool -> BlockId -> BlockId -> Text
block_id_to_call relative parent child
    | Id.ident_namespace parent /= Id.ident_namespace child =
        Id.show_id (Id.unpack_id child)
    | relative, Just suffix <- Text.stripPrefix
            (parent_name <> relative_separator) child_name =
        relative_separator <> suffix
    | otherwise = child_name
    where
    child_name = Id.ident_name child
    parent_name = Id.ident_name parent

-- | True if this is a relative block call.
is_relative :: Expr.Symbol -> Bool
is_relative = (relative_separator `Text.isPrefixOf`) . Expr.unsym

-- | Make a block name relative to a parent block.
make_relative :: BlockId -> Text -> Text
make_relative block_id name =
    Id.ident_name block_id <> relative_separator <> name

-- | If it's a relative BlockId, return the parent and relative call.
parse_relative_id :: BlockId -> Maybe (BlockId, Id.Id)
parse_relative_id block_id
    | Text.null post = Nothing
    | otherwise = Just
        ( Id.BlockId $ Id.set_name pre (Id.unpack_id block_id)
        , Id.id (Id.ident_namespace block_id) (Text.drop 1 post)
        )
    where (pre, post) = Text.breakOn relative_separator $ Id.ident_name block_id

relative_separator :: Text
relative_separator = "-"

-- * util

-- | Evaluate a single note as a generator.  Fake up an event with no prev or
-- next lists.
eval_one :: Derive.CallableExpr d => Bool -> DeriveT.Expr
    -> Derive.Deriver (Stream.Stream d)
eval_one collect = eval_one_at collect 0 1

eval_one_call :: Derive.CallableExpr d => Bool -> DeriveT.Call
    -> Derive.Deriver (Stream.Stream d)
eval_one_call collect = eval_one collect . (:| [])

eval_one_at :: Derive.CallableExpr d => Bool -> ScoreTime -> ScoreTime
    -> DeriveT.Expr -> Derive.Deriver (Stream.Stream d)
eval_one_at collect start dur expr = eval_expr collect ctx expr
    where
    -- Set the event start and duration instead of using Derive.place since
    -- this way I can have zero duration events.
    ctx = Derive.dummy_context start dur $ ShowVal.show_val expr

-- | Like 'Derive.EvalTrack.derive_event' but evaluate the event outside of its
-- track context.  This is useful if you want to evaluate things out of order,
-- i.e. evaluate the /next/ pitch.
eval_event :: Derive.CallableExpr d => Event.Event
    -> Derive.Deriver (Either Text (Stream.Stream d))
eval_event event = case Parse.parse_expr (Event.text event) of
    Left err -> return $ Left err
    Right expr -> Right <$>
        -- TODO eval it separately to catch any exception?
        eval_one_at False (Event.start event) (Event.duration event) expr

-- | Evaluate a generator, reusing the passed args but replacing the Symbol.
-- Generators can use this to delegate to other generators.
reapply_generator :: Derive.Callable (Derive.Generator d)
    => Derive.PassedArgs d -> Expr.Symbol -> Derive.Deriver (Stream.Stream d)
reapply_generator args sym = do
    let ctx = Derive.passed_ctx args
    call <- get_generator sym
    apply_generator ctx call (Derive.passed_vals args)

-- | Like 'reapply_generator', but the note is given normalized time, 0--1,
-- instead of inheriting the start and duration from the args.  This is
-- essential if you want to shift or stretch the note.
reapply_generator_normalized :: Derive.Callable (Derive.Generator d)
    => Derive.PassedArgs d -> Expr.Symbol -> Derive.Deriver (Stream.Stream d)
reapply_generator_normalized args = reapply_generator $ args
    { Derive.passed_ctx = ctx
        { Derive.ctx_event = Event.place 0 1 (Derive.ctx_event ctx)
        , Derive.ctx_event_end = 1
        }
    }
    where ctx = Derive.passed_ctx args

-- | Apply an expr with an explicit Context.  You can use this to reuse the
-- current call's Context, but be careful because it will also inherit the
-- 'Derive.ctx_sub_tracks', which means if inversion hasn't happened yet, which
-- may be what you want or may be surprising.  For instance, it will likely
-- override any pitch you try to set.
reapply :: Derive.CallableExpr d => Derive.Context d -> DeriveT.Expr
    -> Derive.Deriver (Stream.Stream d)
reapply = eval_expr True

reapply_call :: Derive.CallableExpr d => Derive.Context d -> Expr.Symbol
    -> [DeriveT.Term] -> Derive.Deriver (Stream.Stream d)
reapply_call ctx sym call_args =
    reapply ctx (Expr.generator $ Expr.Call sym call_args)

-- | A version of 'eval' specialized to evaluate pitch calls.  It's unknown if
-- this pitch has been transposed or not.
eval_pitch :: ScoreTime -> DeriveT.PitchCall
    -> Derive.Deriver (PSignal.RawPitch a)
eval_pitch pos call = do
    pitch <- Typecheck.typecheck ("eval pitch " <> ShowVal.show_val call) pos
        =<< eval ctx (Expr.ValCall call)
    return $ PSignal.coerce (pitch :: PSignal.Pitch)
    where
    ctx :: Derive.Context Derive.Pitch
    ctx = Derive.dummy_context pos 0 "<eval_pitch>"

-- | Get a Pitch from in a given scale.  Like 'eval_pitch', it's unknown if
-- this pitch has been transposed or not.
eval_note :: Derive.Scale -> Pitch.Note -> Derive.Deriver (PSignal.RawPitch a)
eval_note scale note = case Derive.scale_note_to_call scale note of
    Nothing -> Derive.throw $ pretty scale <> " has no note " <> pretty note
    Just vcall -> apply_pitch 0 vcall

-- | This is like 'eval_pitch' when you already know the call, presumably
-- because you asked 'Derive.scale_note_to_call'.
apply_pitch :: ScoreTime -> Derive.ValCall
    -> Derive.Deriver (PSignal.RawPitch a)
apply_pitch pos call = do
    pitch <- Typecheck.typecheck msg pos =<< apply ctx call []
    return $ PSignal.coerce (pitch :: PSignal.Pitch)
    where
    msg = "apply pitch: " <> showt (Derive.vcall_name call)
    ctx = Derive.dummy_context pos 0 "<apply_pitch>"

-- | Evaluate a single expression, catching an exception if it throws.
eval_expr :: Derive.CallableExpr d => Bool -- ^ See 'Derive.catch'.  This
    -- should be True for evals that generate notes for eventual output.
    -> Derive.Context d -> DeriveT.Expr -> Derive.Deriver (Stream.Stream d)
eval_expr collect ctx expr =
    fromMaybe Stream.empty <$> Derive.catch collect (eval_toplevel ctx expr)
