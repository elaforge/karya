-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Block call and support.
module Derive.Call.Block (eval_root_block, note_calls, control_calls) where
import qualified Data.Char as Char
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Text as Text

import Util.Control
import qualified Util.Seq as Seq
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
import qualified Derive.LEvent as LEvent
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import Derive.Sig (required)
import qualified Derive.TrackLang as TrackLang

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import Types


-- * root block

-- | Evaluate the root block in a performance.  Making this an ordinary call
-- means it participates in the derive cache just like all other calls.
eval_root_block :: Text -> BlockId -> Derive.NoteDeriver
    -- Derive.d_tempo does a bit of magic to stretch all blocks to length 1,
    -- except the root one.  The root block should operate in real time, so
    -- no stretching here.  Otherwise, a tempo of '2' is the same as '1'.
eval_root_block global_transform block_id =
    Eval.apply_transform "global transform" global_transform $
        Eval.eval_one_call True $ call_from_block_id block_id

-- * note calls

note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.generator_call_map
    [ ("clip", c_clip)
    , ("Clip", c_clip_start)
    , ("loop", c_loop)
    , ("tile", c_tile)
    , (BlockUtil.capture_null_control, c_capture_null_control)
    ]
    <> Derive.CallMaps [lookup_note_block] []

lookup_note_block :: Derive.LookupCall (Derive.Generator Derive.Note)
lookup_note_block = Derive.LookupPattern "block name"
    (Derive.extract_doc fake_call)
    (\sym -> fmap c_block <$> symbol_to_block_id sym)
    where
    -- Not evaluated, so it doesn't matter if the BlockId is invalid.
    fake_call = c_block (Id.BlockId (Id.read_id "example/block"))

c_block :: BlockId -> Derive.Generator Derive.Note
c_block block_id = Derive.make_call Module.prelude ("block " <> showt block_id)
    mempty "Substitute the named block into the score."
    $ Sig.call0 $ Sub.inverting $ \args ->
        -- I have to put the block on the stack before calling 'd_block'
        -- because 'Cache.block' relies on on the block id already being
        -- on the stack.
        Internal.with_stack_block block_id (Cache.block run args)
    where
    run args = Derive.place start (end-start) $ trim args (d_block block_id)
        where (start, end) = Args.range args
    trim args deriver = do
        end <- Derive.real (1 :: ScoreTime)
        if Event.positive (Args.event args) then trim_controls end deriver
            else constant_controls_at end deriver

-- | Trim all controls to stop before the given RealTime.  This is to support
-- final block notes and 'Derive.Flags.infer_duration'.  Otherwise, an event
-- at the end of a block will pick up controls (e.g. a transposition change)
-- that is meant to start at the beginning of the next block.
trim_controls :: RealTime -> Derive.Deriver a -> Derive.Deriver a
trim_controls end = Internal.local $ \dyn -> dyn
    { Derive.state_controls =
        fmap (Signal.drop_after end) <$> Derive.state_controls dyn
    , Derive.state_pitch = PitchSignal.drop_after end (Derive.state_pitch dyn)
    , Derive.state_pitches =
        PitchSignal.drop_after end <$> Derive.state_pitches dyn
    }

block_call_doc :: Text
block_call_doc =
    "Derive this block. If the symbol doesn't contain a `/`, the default\
    \ namespace is applied. If it starts with a `.`, the calling block's\
    \ namespace and name are prepended."

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
    pitch_at p = maybe mempty PitchSignal.constant . PitchSignal.at p

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
                return $ Eval.apply_transformers info (NonEmpty.toList expr)
                where info = Derive.dummy_call_info 0 1 "block title"
    transform $ do
        -- Record a dependency on this block.
        Internal.add_block_dep block_id
        BlockUtil.note_deriver block_id

-- | Given a block id, produce a call expression that will call that block.
call_from_block_id :: BlockId -> TrackLang.Call
call_from_block_id block_id = TrackLang.call
    (TrackLang.Symbol $ Id.show_id $ Id.unpack_id block_id) []

-- | Like 'Eval.symbol_to_block_id' but make sure the block exists.
symbol_to_block_id :: TrackLang.Symbol -> Derive.Deriver (Maybe BlockId)
symbol_to_block_id sym = do
    caller <- Internal.lookup_current_block_id
    ns <- Derive.get_ui_state $ State.config_namespace . State.state_config
    case Eval.symbol_to_block_id ns caller sym of
        Nothing -> return Nothing
        Just block_id -> do
            blocks <- Derive.get_ui_state State.state_blocks
            return $ if Map.member block_id blocks then Just block_id
                else Nothing

require_block_id :: TrackLang.Symbol -> Derive.Deriver BlockId
require_block_id sym = maybe
    (Derive.throw $ untxt $
        "block not found: " <> TrackLang.show_val sym)
    return =<< symbol_to_block_id sym

-- ** clip

c_clip :: Derive.Generator Derive.Note
c_clip = make_block_call "clip"
    "Like the normal block call, this will substitute the named block into\
    \ the score. But instead of stretching the block to fit the event\
    \ length, the block will be substituted with no stretching. Any\
    \ events that lie beyond the end of the event will be clipped off.\
    \ This can be used to cut a sequence short, for example to substitute\
    \ a different ending."
    $ \block_id dur args -> do
        end <- Derive.real (snd (Args.range args))
        takeWhile (event_before end) <$>
            Derive.place (Args.start args) dur (d_block block_id)

c_clip_start :: Derive.Generator Derive.Note
c_clip_start = make_block_call "Clip"
    "Like `clip`, but align the named block to the end of the event instead\
    \ of the beginning. Events that then lie before the start are clipped."
    $ \block_id dur args -> do
        start <- Args.real_start args
        dropWhile (event_before start) <$>
            Derive.place (Args.end args - dur) dur (d_block block_id)

-- ** loop

c_loop :: Derive.Generator Derive.Note
c_loop = make_block_call "loop"
    "This is similar to `clip`, but when the called note runs out, it is\
    \ repeated."
    $ \block_id dur args -> do
        let (start, end) = Args.range args
        let repeats = ceiling $ (end - start) / dur
            starts = take repeats $ Seq.range_ start dur
        real_end <- Derive.real end
        takeWhile (event_before real_end) <$> mconcat
            [Derive.place s dur (d_block block_id) | s <- starts]

c_tile :: Derive.Generator Derive.Note
c_tile = make_block_call "tile"
    "This is like `loop`, but it can start the looped sub-block in its middle\
    \ instead of starting from 0. The effect is as if the loop is tiled from\
    \ the beginning of the called block, and is only \"let through\" during\
    \ the `tile` call. This is useful for patterns that are tied to the meter,\
    \ but may be interrupted at arbitrary times, e.g. sarvalaghu patterns."
    $ \block_id dur args -> do
        let (start, end) = Args.range args
        let sub_start = fromIntegral (floor (start / dur)) * dur
        let repeats = ceiling $ (end - sub_start) / dur
            starts = take repeats $ Seq.range_ sub_start dur
        real_end <- Derive.real end
        real_start <- Derive.real start
        dropWhile (event_before real_start) . takeWhile (event_before real_end)
            <$> mconcat [Derive.place s dur (d_block block_id) | s <- starts]

-- ** util

make_block_call :: Text -> Text
    -> (BlockId -> ScoreTime -> Derive.NoteArgs -> Derive.NoteDeriver)
    -> Derive.Generator Derive.Note
make_block_call name doc call = Derive.make_call Module.prelude name mempty doc
    $ Sig.call ((,)
    <$> required "block-id" block_call_doc
    <*> Sig.defaulted "dur" Nothing "If given, the callee will be stretched to\
        \ this duration. Otherwise, it retains its own duration."
    ) $ \(sym, maybe_dur) -> Sub.inverting $ \args -> do
        block_id <- require_block_id sym
        sub_dur <- Derive.get_block_dur block_id
        let dur = case maybe_dur of
                Nothing -> sub_dur
                Just (TrackLang.Positive dur) -> dur
        Internal.with_stack_block block_id $
            Cache.block (call block_id dur) args

-- | Consistent with half-open ranges, block calls try to include events lining
-- up with the start, and exclude ones lining up with the end.
event_before :: RealTime -> LEvent.LEvent Score.Event -> Bool
event_before t =
    LEvent.either ((< t - RealTime.eta) . Score.event_start) (const True)

-- * control calls

control_calls :: Derive.CallMaps Derive.Control
control_calls = Derive.CallMaps [lookup_control_block] []

lookup_control_block :: Derive.LookupCall (Derive.Generator Derive.Control)
lookup_control_block = Derive.LookupPattern "block id"
    (Derive.extract_doc fake_call)
    (\sym -> fmap c_control_block <$> symbol_to_block_id sym)
    where
    -- Not evaluated, so it doesn't matter if the BlockId is invalid.
    fake_call = c_control_block (Id.BlockId (Id.read_id "fake/block"))

c_control_block :: BlockId -> Derive.Generator Derive.Control
c_control_block block_id = Derive.make_call Module.prelude "control-block"
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
    deriver <- Derive.eval_ui ("d_control_block " ++ show block_id)
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
        return $! Score.empty_event
            { Score.event_controls = Map.singleton Controls.null sig
            , Score.event_stack = stack
            }
