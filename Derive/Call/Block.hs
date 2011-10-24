-- | Block call and support.
module Derive.Call.Block (
    note_calls
    , eval_root_block, lookup_block
) where
import qualified Data.Map as Map

import Util.Control
import qualified Util.Pretty as Pretty
import Ui
import qualified Ui.Id as Id
import qualified Ui.State as State
import qualified Ui.Types as Types

import qualified Derive.Cache as Cache
import qualified Derive.Call as Call
import qualified Derive.Call.Note as Note
import Derive.CallSig (required)
import qualified Derive.CallSig as CallSig
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang


note_calls :: Derive.NoteCallMap
note_calls = Derive.make_calls
    [ ("clip", c_clip)
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

lookup_block :: Derive.LookupCall Derive.NoteCall
lookup_block sym = fmap c_block <$> symbol_to_block_id sym

c_block :: BlockId -> Derive.NoteCall
c_block block_id = block_call (const (Just block_id)) $
    Derive.stream_generator "block" $ Note.inverting $ Cache.caching_call run
    where
    run args
        | null (Derive.passed_vals args) =
            Derive.d_place start (end-start) (d_block block_id)
        | otherwise =
            Derive.throw_arg_error "args for block call not implemented yet"
        where (start, end) = Derive.passed_range args

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
    case Map.lookup block_id blocks of
        Nothing -> Derive.throw "block_id not found"
        _ -> return ()
    -- Record a dependency on this block.
    Internal.add_block_dep block_id
    -- This check disabled since a block will show up in the stack twice if it
    -- is inverted.  I'm still protected from recursion by the stack limit.
    -- -- Since there is no branching, any recursion will be endless.
    -- stack <- Derive.gets Derive.state_stack
    -- when (Stack.Block block_id `elem` drop 1 (Stack.innermost stack)) $
    --     Derive.throw "recursive block derivation"
    state <- Derive.get
    let rethrow exc = Derive.throw $ "lookup deriver for " ++ show block_id
            ++ ": " ++ show exc
    deriver <- either rethrow return
        (Derive.state_lookup_deriver (Derive.state_constant state) block_id)
    deriver

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
            block_id = make_block_id ns sym
        return $ if block_id `Map.member` State.state_blocks ui_state
            then Just block_id
            else Nothing

make_block_id :: Id.Namespace -> TrackLang.Symbol -> BlockId
make_block_id namespace (TrackLang.Symbol call) =
    Types.BlockId (Id.make namespace call)

-- * clip

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
            (Derive.throw $ "block not found: " ++ Pretty.pretty sym) return
            =<< symbol_to_block_id sym
        sub_dur <- Derive.get_block_dur block_id
        end <- Derive.real (snd (Derive.passed_range args))
        takeWhile (before end) <$>
            Derive.d_place (Derive.passed_score args) sub_dur
                (d_block block_id)
    where
    before end = LEvent.either ((<end) . Score.event_start) (const True)
    get_block_id (ns, args) = case Derive.passed_vals args of
        TrackLang.VSymbol sym : _ -> Just (make_block_id ns sym)
        _ -> Nothing
