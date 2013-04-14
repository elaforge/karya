-- | Block call and support.
module Derive.Call.Block (
    note_calls
    , eval_root_block, lookup_note_block
    , lookup_control_block
) where
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map

import Util.Control
import qualified Ui.Block as Block
import qualified Ui.Id as Id
import qualified Ui.State as State
import qualified Ui.Types as Types

import qualified Derive.Args as Args
import qualified Derive.Cache as Cache
import qualified Derive.Call as Call
import qualified Derive.Call.BlockUtil as BlockUtil
import qualified Derive.Call.Note as Note
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.LEvent as LEvent
import qualified Derive.ParseBs as ParseBs
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import Derive.Sig (required)
import qualified Derive.TrackLang as TrackLang

import Types


note_calls :: Derive.NoteCallMap
note_calls = Derive.make_calls
    [ ("clip", c_clip)
    , (BlockUtil.capture_null_control, c_capture_null_control)
    ]

-- * root block

-- | Evaluate the root block in a performance.  Making this an ordinary call
-- means it participates in the derive cache just like all other calls.
eval_root_block :: String -> BlockId -> Derive.EventDeriver
    -- Derive.d_tempo does a bit of magic to stretch all blocks to length 1,
    -- except the root one.  The root block should operate in real time, so
    -- no stretching here.  Otherwise, a tempo of '2' is the same as '1'.
eval_root_block global_transform block_id =
    apply_transform "global transform" global_transform $
        Call.eval_one_call $ call_from_block_id block_id

apply_transform :: String -> String -> Derive.EventDeriver
    -> Derive.EventDeriver
apply_transform name expr_str deriver = do
    expr <- case ParseBs.parse_expr (ParseBs.from_string expr_str) of
        Left err -> Derive.throw $ name ++ ": " ++ err
        Right expr -> return expr
    let transform = if null expr_str then id
            else Call.apply_transformer info (NonEmpty.toList expr)
        info = Derive.dummy_call_info 0 1 name
    transform deriver

-- * note block calls

lookup_note_block :: Derive.LookupCall Derive.NoteCall
lookup_note_block = Derive.pattern_lookup "block id"
    (Derive.extract_doc fake_call)
    (\sym -> fmap c_block <$> symbol_to_block_id sym)
    where
    -- Not evaluated, so it doesn't matter if the BlockId is invalid.
    fake_call = c_block (Types.BlockId (Id.read_id "example/block"))

c_block :: BlockId -> Derive.NoteCall
c_block block_id = Derive.stream_generator ("block " <> txt (show block_id))
        Tags.prelude "Substitute the named block into the score." $
    Sig.call0 $ Note.inverting $ \args ->
        -- I have to put the block on the stack before calling 'd_block'
        -- because 'Cache.cache_block' relies on on the block id already being
        -- on the stack.
        Internal.with_stack_block block_id (Cache.cache_block run args)
    where
    run args = Derive.d_place start (end-start) (d_block block_id)
        where (start, end) = Args.range args

d_block :: BlockId -> Derive.EventDeriver
d_block block_id = do
    blocks <- Derive.get_ui_state State.state_blocks
    -- Do some error checking.  These are all caught later, but if I throw here
    -- I can give more specific error msgs.
    title <- case Map.lookup block_id blocks of
        Nothing -> Derive.throw "block_id not found"
        Just block -> return $ Block.block_title block
    apply_transform "block title" title $ do
        -- Record a dependency on this block.
        Internal.add_block_dep block_id
        deriver <- Derive.eval_ui ("d_block " ++ show block_id)
            (BlockUtil.note_deriver block_id)
        deriver

-- | Given a block id, produce a call expression that will call that block.
call_from_block_id :: BlockId -> TrackLang.Call
call_from_block_id block_id =
    TrackLang.call (txt $ Id.show_id $ Id.unpack_id block_id) []

-- | Like 'Call.symbol_to_block_id' but return Nothing if the block doesn't
-- exist.
symbol_to_block_id :: TrackLang.CallId -> Derive.Deriver (Maybe BlockId)
symbol_to_block_id sym = Call.symbol_to_block_id sym >>= \x -> case x of
    Nothing -> return Nothing
    Just block_id -> do
        blocks <- Derive.get_ui_state State.state_blocks
        if Map.member block_id blocks
            then return (Just block_id) else return Nothing

-- ** clip

c_clip :: Derive.NoteCall
c_clip = Derive.stream_generator "clip" Tags.prelude
    ("Like the normal block call, this will substitute the named block into\
    \ the score. But instead of stretching the block to fit the event\
    \ length, the block will be substituted with no stretching. Any\
    \ events that lie beyond the end of the event will be clipped off.\
    \ This can be used to cut a sequence short, for example to substitute\
    \ a different ending.\
    \\nIt's not necessarily easy to use because the callee block\
    \ may not be in the same time scale as the calling block."
    -- TODO wait until I actually start using this to see if it's worth
    -- coming up with a solution for that.
    ) $ Sig.call
    ( required "block_id" $
        "Derive this block. If it doesn't contain a /, the default namespace\
        \ is applied."
    ) $
    \sym -> Note.inverting $ \args -> do
        block_id <- maybe
            (Derive.throw $ untxt $
                "block not found: " <> TrackLang.show_val sym)
            return =<< symbol_to_block_id sym
        Internal.with_stack_block block_id $
            Cache.cache_block (clip_call block_id) args
    where
    clip_call block_id args = do
        sub_dur <- Derive.get_block_dur block_id
        end <- Derive.real (snd (Args.range args))
        takeWhile (before end) <$>
            Derive.d_place (Args.start args) sub_dur (d_block block_id)
    before end = LEvent.either ((<end) . Score.event_start) (const True)


-- * control call

lookup_control_block :: Derive.LookupCall Derive.ControlCall
lookup_control_block = Derive.pattern_lookup "block id"
    (Derive.extract_doc fake_call)
    (\sym -> fmap c_control_block <$> symbol_to_block_id sym)
    where
    -- Not evaluated, so it doesn't matter if the BlockId is invalid.
    fake_call = c_control_block (Types.BlockId (Id.read_id "fake/block"))

c_control_block :: BlockId -> Derive.ControlCall
c_control_block block_id = Derive.stream_generator "control-block" Tags.prelude
    ("Substitute the control signal from the named control block.\
    \ A control block should consist of a single branch ending in\
    \ a track named `%`.  The signal from that track will be\
    \ substituted."
    ) $
    Sig.call0 $ \args -> do
        let (start, end) = Args.range args
        Derive.d_place start (end-start) (d_control_block block_id)

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

c_capture_null_control :: Derive.NoteCall
c_capture_null_control = Derive.generator1 BlockUtil.capture_null_control
    Tags.internal
    ("This is an internal call used to capture the control signal at the\
    \ bottom of a control block."
    ) $ Sig.call0 $ \_ -> do
        sig <- Derive.require "no null control to capture"
            =<< Derive.get_control Score.c_null
        stack <- Derive.get_stack
        return $! Score.empty_event
            { Score.event_controls = Map.singleton Score.c_null sig
            , Score.event_stack = stack
            }
