-- | Block call and support.
module Derive.Call.Block (
    note_calls
    , eval_root_block, lookup_note_block
    , lookup_control_block
) where
import qualified Data.Map as Map

import Util.Control
import qualified Util.Pretty as Pretty
import qualified Ui.Block as Block
import qualified Ui.Id as Id
import qualified Ui.State as State
import qualified Ui.Types as Types

import qualified Derive.Args as Args
import qualified Derive.Cache as Cache
import qualified Derive.Call as Call
import qualified Derive.Call.BlockUtil as BlockUtil
import qualified Derive.Call.Note as Note
import qualified Derive.CallSig as CallSig
import Derive.CallSig (required)
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.LEvent as LEvent
import qualified Derive.ParseBs as ParseBs
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang

import Types


note_calls :: Derive.NoteCallMap
note_calls = Derive.make_calls
    [ ("clip", c_clip)
    , (BlockUtil.capture_null_control, c_capture_null_control)
    ]

-- | Evaluate the root block in a performance.  Making this an ordinary call
-- means it participates in the derive cache just like all other calls.
--
-- TODO put global postproc here, like negative duration
eval_root_block :: BlockId -> Derive.EventDeriver
    -- Derive.d_tempo does a bit of magic to stretch all blocks to length 1,
    -- except the root one.  The root block should operate in real time, so
    -- no stretching here.  Otherwise, a tempo of '2' is the same as '1'.
eval_root_block block_id = Call.eval_one 0 1 [call_from_block_id block_id]

-- * note block calls

lookup_note_block :: Derive.LookupCall Derive.NoteCall
lookup_note_block sym = fmap c_block <$> symbol_to_block_id sym

c_block :: BlockId -> Derive.NoteCall
c_block block_id = block_call (const (Just block_id)) $
    Derive.stream_generator ("block " ++ show block_id) $
        Note.inverting $ Cache.caching_call run
    where
    run args
        | null (Derive.passed_vals args) =
            Derive.d_place start (end-start) (d_block block_id)
        | otherwise =
            Derive.throw_arg_error $
                "args for block call not implemented yet: "
                ++ Pretty.pretty (Derive.passed_vals args)
        where (start, end) = Args.range args

block_call :: ((Id.Namespace, Derive.PassedArgs d) -> Maybe BlockId)
    -> Derive.Call d -> Derive.Call d
block_call f call =
    call { Derive.call_generator = add <$> Derive.call_generator call }
    where add gcall = gcall { Derive.gcall_block = f }

d_block :: BlockId -> Derive.EventDeriver
d_block block_id = do
    -- The block id is put on the stack by 'gdep_block' before this is called.
    blocks <- Derive.get_ui_state State.state_blocks
    -- Do some error checking.  These are all caught later, but if I throw here
    -- I can give more specific error msgs.
    title <- case Map.lookup block_id blocks of
        Nothing -> Derive.throw $ "block_id not found"
        Just block -> return $ Block.block_title block
    expr <- case ParseBs.parse_expr (ParseBs.from_string title) of
        Left err -> Derive.throw $ "block title: " ++ err
        Right expr -> return expr
    deriver <- Derive.eval_ui ("d_block " ++ show block_id)
        (BlockUtil.note_deriver block_id)

    let transform = if null title then id else Call.apply_transformer info expr
        info = (Call.note_dinfo, Derive.dummy_call_info "block title")
    -- Record a dependency on this block.
    Internal.add_block_dep block_id
    transform deriver

-- | Given a block id, produce a call expression that will call that block.
call_from_block_id :: BlockId -> TrackLang.Call
call_from_block_id block_id =
    TrackLang.call (Id.show_id (Id.unpack_id block_id)) []

symbol_to_block_id :: TrackLang.Symbol -> Derive.Deriver (Maybe BlockId)
symbol_to_block_id sym
    | sym == TrackLang.Symbol "" = return Nothing
    | otherwise = do
        ui_state <- Derive.get_ui_state id
        let ns = State.config_namespace (State.state_config ui_state)
        return $ do
            block_id <- make_block_id ns sym
            if block_id `Map.member` State.state_blocks ui_state
                then Just block_id else Nothing

make_block_id :: Id.Namespace -> TrackLang.Symbol -> Maybe BlockId
make_block_id namespace (TrackLang.Symbol call) =
    Types.BlockId <$> Id.make namespace call

-- ** clip

-- | Call a block, but clip it instead of stretching it to fit the event.
--
-- This is useful to cut a certain sequence short, for example to substitute
-- a different ending.
--
-- It's not necessarily super easy to use because the callee block may not be
-- in the same time scale as the calling block.  TODO wait until I actually
-- start using this to see if it's worth coming up with a solution for that.
c_clip :: Derive.NoteCall
c_clip = block_call get_block_id $ Derive.stream_generator "clip" $
    Note.inverting $ Cache.caching_call $ \args ->
    CallSig.call1 args (required "block_id") $ \sym -> do
        block_id <- maybe
            (Derive.throw $ "block not found: " ++ TrackLang.show_val sym)
            return =<< symbol_to_block_id sym
        sub_dur <- Derive.get_block_dur block_id
        end <- Derive.real (snd (Args.range args))
        takeWhile (before end) <$>
            Derive.d_place (Args.start args) sub_dur
                (d_block block_id)
    where
    before end = LEvent.either ((<end) . Score.event_start) (const True)
    get_block_id (ns, args) = case Derive.passed_vals args of
        TrackLang.VSymbol sym : _ -> make_block_id ns sym
        _ -> Nothing


-- * control call

lookup_control_block :: Derive.LookupCall Derive.ControlCall
lookup_control_block sym = fmap c_control_block <$> symbol_to_block_id sym

-- TODO can I factor out repetition with d_block?
-- call_name error_msg

c_control_block :: BlockId -> Derive.ControlCall
c_control_block block_id = block_call (const (Just block_id)) $
    Derive.stream_generator "control-block" run
    where
    run args
        | null (Derive.passed_vals args) =
            Derive.d_place start (end-start) (d_control_block block_id)
        | otherwise =
            Derive.throw_arg_error $
                "args for control block call not implemented yet: "
                ++ Pretty.pretty (Derive.passed_vals args)
        where (start, end) = Args.range args

d_control_block :: BlockId -> Derive.ControlDeriver
d_control_block block_id = do
    blocks <- Derive.get_ui_state State.state_blocks
    when (Map.lookup block_id blocks == Nothing) $
        Derive.throw "block_id not found"
    Internal.add_block_dep block_id
    deriver <- Derive.eval_ui ("d_control_block " ++ show block_id)
        (BlockUtil.control_deriver block_id)
    deriver

c_capture_null_control :: Derive.NoteCall
c_capture_null_control = Derive.generator1 BlockUtil.capture_null_control $
    \args -> CallSig.call0 args $ do
        sig <- Derive.require "no null control to capture"
            =<< Derive.get_control Score.c_null
        stack <- Derive.get_stack
        return $! Score.empty_event
            { Score.event_controls = Map.singleton Score.c_null sig
            , Score.event_stack = stack
            }
