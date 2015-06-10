-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Block call and support.
module Derive.Call.Prelude.Block (
    eval_root_block, note_calls, control_calls
) where
import qualified Data.Char as Char
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text

import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Id as Id
import qualified Ui.State as State

import qualified Derive.Args as Args
import qualified Derive.Cache as Cache
import qualified Derive.Call.BlockUtil as BlockUtil
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Sub as Sub
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Eval as Eval
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.PSignal as PSignal
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Signal as Signal
import Global
import Types


-- * root block

-- | Evaluate the root block in a performance.  Making this an ordinary call
-- means it participates in the derive cache just like all other calls.
eval_root_block :: Text -> BlockId -> Derive.NoteDeriver
    -- Tempo.with_tempo does a bit of magic to stretch all blocks to length 1,
    -- except the root one.  The root block should operate in real time, so
    -- no stretching here.  Otherwise, a tempo of '2' is the same as '1'.
eval_root_block global_transform block_id =
    Eval.eval_transform_expr "global transform" global_transform $
        Eval.eval_one_call True $ call_from_block_id block_id

-- * note calls

note_calls :: Derive.CallMaps Derive.Note
note_calls =
    Derive.generator_call_map
        [(BlockUtil.capture_null_control, c_capture_null_control)]
    <> Derive.CallMaps [lookup_note_block] []

lookup_note_block :: Derive.LookupCall (Derive.Generator Derive.Note)
lookup_note_block = Derive.LookupPattern "block name"
    (Derive.extract_doc fake_call)
    (\sym -> fmap c_block <$> call_to_block_id sym)
    where
    -- Not evaluated, so it doesn't matter if the BlockId is invalid.
    fake_call = c_block (Id.BlockId (Id.read_id "example/block"))

c_block :: BlockId -> Derive.Generator Derive.Note
c_block block_id = Derive.generator_with_duration get_duration Module.prelude
    ("block " <> showt block_id) mempty
    "Substitute the named block into the score. If the symbol doesn't contain\
    \ a `/`, the default namespace is applied. If it starts with a `-`, this\
    \ is a relative call and the calling block's namespace and name are\
    \ prepended."
    $ Sig.call0 $ Sub.inverting $ \args ->
        -- I have to put the block on the stack before calling 'd_block'
        -- because 'Cache.block' relies on on the block id already being
        -- on the stack.
        Internal.with_stack_block block_id $ Cache.block run args
    where
    run args = Derive.place start (end - start) $ trim args (d_block block_id)
        where (start, end) = Args.range args
    trim args deriver = do
        end <- Derive.real (1 :: ScoreTime)
        if Event.positive (Args.event args) then trim_controls end deriver
            else constant_controls_at end deriver
    get_duration :: a -> Derive.Deriver Derive.CallDuration
    get_duration _ = Derive.Duration . (\(s, e) -> e-s) <$>
        Derive.block_logical_range block_id

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
trim_controls end = Internal.local $ \dyn -> dyn
    { Derive.state_controls = fmap trim_c <$> Derive.state_controls dyn
    , Derive.state_pitch = trim_p (Derive.state_pitch dyn)
    , Derive.state_pitches = trim_p <$> Derive.state_pitches dyn
    }
    where
    trim_c sig
        | Maybe.isNothing (Signal.sample_at end sig) = sig
        | otherwise = Signal.drop_at_after end sig
            <> Signal.drop_before_at end sig
    trim_p sig
        | Maybe.isNothing (PSignal.sample_at end sig) = sig
        | otherwise = PSignal.drop_at_after end sig
            <> PSignal.drop_before_at end sig

-- | Replace all controls and pitches with constants from ScoreTime 1.
-- This is to support arrival notes.  If a block call has negative duration,
-- then its controls should be taken from its start time, which is the end of
-- the event, time-wise.  Since 'Derive.place' has already been called, that's
-- ScoreTime 1.
--
-- Details in "Derive.Call.Post.ArrivalNote".  TODO probabbly get rid of this
constant_controls_at :: RealTime -> Derive.Deriver a -> Derive.Deriver a
constant_controls_at start = Internal.local $ \dyn -> dyn
    { Derive.state_controls =
        Map.map (fmap (Signal.constant . Signal.at start))
            (Derive.state_controls dyn)
    , Derive.state_pitch = pitch_at start (Derive.state_pitch dyn)
    , Derive.state_pitches =
        Map.map (pitch_at start) (Derive.state_pitches dyn)
    }
    where
    pitch_at p = maybe mempty PSignal.constant . PSignal.at p

d_block :: BlockId -> Derive.NoteDeriver
d_block block_id = do
    blocks <- Derive.get_ui_state State.state_blocks
    -- Do some error checking.  These are all caught later, but if I throw here
    -- I can give more specific error msgs.
    title <- case Map.lookup block_id blocks of
        Nothing -> Derive.throw "block_id not found"
        Just block -> return $ Block.block_title block
    transform <- if Text.all Char.isSpace title
        then return id
        else case ParseTitle.parse_block title of
            Left err -> Derive.throw $ "block title: " <> err
            Right expr ->
                return $ Eval.eval_transformers info (NonEmpty.toList expr)
                where info = Derive.dummy_call_info 0 1 "block title"
    -- Record a dependency on this block.
    transform $ Internal.add_block_dep block_id
        *> BlockUtil.note_deriver block_id

-- | Given a block id, produce a call expression that will call that block.
call_from_block_id :: BlockId -> TrackLang.Call
call_from_block_id block_id =
    TrackLang.call (TrackLang.Symbol $ Id.show_id $ Id.unpack_id block_id) []

-- | Like 'Eval.call_to_block_id' but make sure the block exists.
call_to_block_id :: TrackLang.Symbol -> Derive.Deriver (Maybe BlockId)
call_to_block_id sym = do
    caller <- Internal.lookup_current_block_id
    ns <- Derive.get_ui_state $ State.config_namespace . State.state_config
    case Eval.call_to_block_id ns caller sym of
        Nothing -> return Nothing
        Just block_id -> do
            blocks <- Derive.get_ui_state State.state_blocks
            return $ if Map.member block_id blocks then Just block_id
                else Nothing

-- * control calls

control_calls :: Derive.CallMaps Derive.Control
control_calls = Derive.CallMaps [lookup_control_block] []

lookup_control_block :: Derive.LookupCall (Derive.Generator Derive.Control)
lookup_control_block = Derive.LookupPattern "block id"
    (Derive.extract_doc fake_call)
    (\sym -> fmap c_control_block <$> call_to_block_id sym)
    where
    -- Not evaluated, so it doesn't matter if the BlockId is invalid.
    fake_call = c_control_block (Id.BlockId (Id.read_id "fake/block"))

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
    blocks <- Derive.get_ui_state State.state_blocks
    when (Map.lookup block_id blocks == Nothing) $
        Derive.throw "block_id not found"
    Internal.add_block_dep block_id
    deriver <- Derive.eval_ui ("d_control_block " <> showt block_id)
        (BlockUtil.control_deriver block_id)
    deriver

c_capture_null_control :: Derive.Generator Derive.Note
c_capture_null_control = Derive.generator1 Module.internal
    (TrackLang.unsym BlockUtil.capture_null_control) mempty
    ("This is an internal call used to capture the control signal at the\
    \ bottom of a control block."
    ) $ Sig.call0 $ \_ -> do
        sig <- Derive.require "no null control to capture"
            =<< Derive.get_control_signal Controls.null
        stack <- Derive.get_stack
        return $! Score.set_control Controls.null sig Score.empty_event
            { Score.event_stack = stack }
