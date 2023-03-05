-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Block call and support.
module Derive.C.Prelude.Block (
    library
    , eval_root_block, global_transform
) where
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map

import qualified Derive.Args as Args
import qualified Derive.Cache as Cache
import qualified Derive.Call.BlockUtil as BlockUtil
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Eval as Eval
import qualified Derive.Expr as Expr
import qualified Derive.Library as Library
import qualified Derive.PSignal as PSignal
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import qualified Derive.Stack as Stack
import qualified Derive.Stream as Stream

import qualified Perform.Signal as Signal
import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Id as Id
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig

import           Global
import           Types


library :: Library.Library
library = mconcat
    [ Library.generators
        [(BlockUtil.capture_null_control, c_capture_null_control)]
    , Library.pattern pattern_note_block
    , Library.pattern pattern_control_block
    ]

-- * root block

-- | Evaluate the root block in a performance.  Making this an ordinary call
-- means it participates in the derive cache just like all other calls.
--
-- Tempo.with_tempo does a bit of magic to stretch all blocks to length 1,
-- except the root one.  The root block should operate in real time, so no
-- stretching here.  Otherwise, a tempo of 2 would be the same as 1.
{-# SCC eval_root_block #-}
eval_root_block :: BlockId -> Derive.NoteDeriver
eval_root_block block_id =
    global_transform $ Eval.eval_one_call True $ call_from_block_id block_id

{-# SCC global_transform #-}
global_transform :: Derive.NoteDeriver -> Derive.NoteDeriver
global_transform = transform_if_present ctx "GLOBAL"
    where ctx = Derive.dummy_context 0 1 "<GLOBAL transform>"

transform_if_present :: Derive.Callable (Derive.Transformer a)
    => Derive.Context a
    -> Expr.Symbol -> Derive.Deriver (Stream.Stream a)
    -> Derive.Deriver (Stream.Stream a)
transform_if_present ctx sym deriver = Derive.lookup_call sym >>= \case
    Nothing -> deriver
    Just call -> Eval.apply_transformer ctx call [] deriver

-- * note calls

pattern_note_block :: Derive.PatternCall (Derive.Generator Derive.Note)
pattern_note_block = Derive.PatternCall
    { pat_description = "block name"
    , pat_doc = Derive.extract_doc fake_call
    , pat_function = \sym -> fmap c_block <$> call_to_block_id sym
    }
    where
    -- Not evaluated, so it doesn't matter if the BlockId is invalid.
    fake_call = c_block (Id.BlockId (Id.read_id "example/block"))

{-# SCC c_block #-}
c_block :: BlockId -> Derive.Generator Derive.Note
c_block block_id = Derive.with_score_duration get_score_duration $
    Derive.with_real_duration (const $ get_real_duration block_id) $
    Derive.generator Module.prelude
        (Derive.CallName $ "block " <> showt block_id) Tags.prio_block
    "Substitute the named block into the score. If the symbol doesn't contain\
    \ a `/`, the default namespace is applied. If it starts with a `-`, this\
    \ is a relative call and the calling block's namespace and name are\
    \ prepended."
    $ Sig.call0 $ Sub.inverting $ \args -> do
        stack <- Internal.get_stack
        when (Stack.Block block_id `elem` Stack.innermost stack) $
            Derive.throw $ "recursive call to " <> pretty block_id
        -- This ensures that each block call from the same event sees a new
        -- serial number, as documented in 'Derive.CacheKey'.
        Internal.increment_event_serial
        -- I have to put the block on the stack before calling 'd_block'
        -- because 'Cache.block' relies on on the block id already being
        -- on the stack.
        Internal.with_stack_block block_id $ Cache.block block_id run args
    where
    run args = Derive.place start (end - start) $ trim args $ d_block block_id
        where (start, end) = Args.range args
    trim args deriver = do
        end <- Derive.real (1 :: ScoreTime)
        if Event.is_positive (Args.event args)
            then trim_controls end deriver
            else deriver
    get_score_duration :: a -> Derive.Deriver (Derive.CallDuration ScoreTime)
    get_score_duration _ = Derive.CallDuration . (\(s, e) -> e-s) <$>
        Derive.block_logical_range block_id
    get_real_duration :: BlockId
        -> Derive.Deriver (Derive.CallDuration RealTime)
    get_real_duration block_id = fmap (either (const Derive.Unknown) id) $
        Derive.get_real_duration $ Internal.with_stack_block block_id $
            d_block block_id

-- | Remove samples at the given RealTime.  This is to support final block
-- notes and 'Derive.Flags.infer_duration'.  Otherwise, an event at the end of
-- a block will pick up controls (e.g. a transposition change) that is meant to
-- start at the beginning of the next block.
--
-- However, I can't trim the signal entirely because I still want the control
-- signals past the end of the block to be visible to non-final events.  The
-- problem is that I do want control samples at the end time to be visible
-- if they are interior to the block.  So I need a way to tell the difference
-- between controls from the caller and local ones, so a final event can
-- exclude the former and include the latter.
--
-- TODO this is unsatisfying because it feels ad-hoc and clunky.  In addition,
-- it drops a sample that I don't actually want to drop for non-final events.
-- Not to mention it's inefficient.
trim_controls :: RealTime -> Derive.Deriver a -> Derive.Deriver a
trim_controls end =
    Derive.modify_signals (Signal.drop_discontinuity_at end)
        (PSignal.drop_discontinuity_at end)

d_block :: BlockId -> Derive.NoteDeriver
d_block block_id = do
    blocks <- Derive.get_ui_state Ui.state_blocks
    -- Do some error checking.  These are all caught later, but if I throw here
    -- I can give more specific error msgs.
    title <- case Map.lookup block_id blocks of
        Nothing -> Derive.throw "block_id not found"
        Just block -> return $ Block.block_title block
    -- Record a dependency on this block.  This should happen even if the
    -- derivation throws, because a dependency on a block with an error is
    -- still a dependency.
    Internal.add_block_dep block_id
    let deriver = BlockUtil.note_deriver block_id
    fmap (fromMaybe Stream.empty) $ Derive.catch True $
        case ParseTitle.parse_block title of
            Left err -> Derive.throw $ "block title: " <> err
            -- An empty title means no transformation.
            Right (Expr.Call call [] :| []) | call == "" -> deriver
            Right expr ->
                Eval.eval_transformers ctx (NonEmpty.toList expr) deriver
                where ctx = Derive.dummy_context 0 1 "block title"

-- | Given a block id, produce a call expression that will call that block.
call_from_block_id :: BlockId -> DeriveT.Call
call_from_block_id block_id =
    Expr.call0 (Expr.Symbol $ Id.show_id $ Id.unpack_id block_id)

-- | Like 'Eval.call_to_block_id' but make sure the block exists.
call_to_block_id :: Expr.Symbol -> Derive.Deriver (Maybe BlockId)
call_to_block_id sym = do
    caller <- Internal.lookup_current_block_id
    ns <- Derive.get_ui_state $ UiConfig.config_namespace . Ui.state_config
    case Eval.call_to_block_id ns caller sym of
        Nothing -> return Nothing
        Just block_id -> do
            blocks <- Derive.get_ui_state Ui.state_blocks
            return $ if Map.member block_id blocks then Just block_id
                else Nothing

-- * control calls

pattern_control_block :: Derive.PatternCall (Derive.Generator Derive.Control)
pattern_control_block = Derive.PatternCall
    { pat_description = "block name"
    , pat_doc = Derive.extract_doc fake_call
    , pat_function = \sym -> fmap c_control_block <$> call_to_block_id sym
    }
    where
    -- Not evaluated, so it doesn't matter if the BlockId is invalid.
    fake_call = c_control_block (Id.BlockId (Id.read_id "example/block"))

c_control_block :: BlockId -> Derive.Generator Derive.Control
c_control_block block_id = Derive.generator Module.prelude "control-block"
    mempty ("Substitute the control signal from the named control block.\
    \ A control block should consist of a single branch ending in\
    \ a track named `%`.  The signal from that track will be\
    \ substituted."
    ) $
    Sig.call0 $ \args -> do
        let (start, end) = Args.range args
        Derive.place start (end-start) (d_control_block block_id)

d_control_block :: BlockId -> Derive.ControlDeriver
d_control_block block_id = Internal.with_stack_block block_id $ do
    -- Control calls aren't cached, so I can put the block stack in the
    -- convenient place.
    blocks <- Derive.get_ui_state Ui.state_blocks
    when (Map.lookup block_id blocks == Nothing) $
        Derive.throw "block_id not found"
    Internal.add_block_dep block_id
    deriver <- Derive.eval_ui $ BlockUtil.control_deriver block_id
    deriver

c_capture_null_control :: Derive.Generator Derive.Note
c_capture_null_control = Derive.generator1 Module.internal
    (Derive.sym_to_call_name BlockUtil.capture_null_control) mempty
    ("This is an internal call used to capture the control signal at the\
    \ bottom of a control block."
    ) $ Sig.call0 $ \_ -> do
        sig <- Derive.require "no null control to capture"
            =<< Derive.lookup_signal Controls.null
        stack <- Derive.get_stack
        return $! Score.set_control Controls.null sig Score.empty_event
            { Score.event_stack = stack }
