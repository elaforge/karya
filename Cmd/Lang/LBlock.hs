module Cmd.Lang.LBlock where
import qualified Data.Map as Map

import Util.Control
import qualified Util.Seq as Seq
import qualified Ui.Event as Event
import qualified Ui.Id as Id
import qualified Ui.State as State
import qualified Ui.Types as Types

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Integrate
import qualified Cmd.ModifyEvents as ModifyEvents
import qualified Cmd.Perf as Perf
import qualified Cmd.Selection as Selection

import qualified Derive.Call.Integrate as Call.Integrate
import qualified Derive.LEvent as LEvent
import Types


-- | Rename a block and all occurrances in the current block.
--
-- It doesn't update TrackIds so they may still be named under their old block,
-- but track id names aren't supposed to carry meaning anyway.
rename :: BlockId -> BlockId -> Cmd.CmdL ()
rename from to = Create.rename_block from to

-- | Rename block calls in a single block.
replace :: BlockId -> BlockId -> Cmd.CmdL ()
replace from to = do
    block_id <- Cmd.get_focused_block
    ModifyEvents.block_tracks block_id $ ModifyEvents.track_text $
        replace_block_call from to

replace_block_call :: BlockId -> BlockId -> String -> String
replace_block_call from to text
    | text == Id.ident_name from = Id.ident_name to
    | text == Id.ident_string from = Id.ident_string to
    | otherwise = text


-- * create

-- | If the events under the cursor are a block calls, create blocks that don't
-- already exist.  Optionally use a model block.
block_for_event :: Maybe BlockId -> Cmd.CmdL ()
block_for_event model = mapM_ make =<< Selection.events
    where
    make (_, _, events) = mapM_ (make_named model . Event.event_string . snd)
        events

make_named :: Maybe BlockId -> String -> Cmd.CmdL ()
make_named template name = whenM (can_create name) $ case template of
    Nothing -> do
        template_id <- Cmd.get_focused_block
        Create.block_from_template False template_id
    Just template_id -> Create.block_from_template True template_id

can_create :: (State.M m) => String -> m Bool
can_create "" = return False
can_create name = do
    ns <- State.get_namespace
    case Types.BlockId <$> Id.make ns name of
        Just block_id -> not . Map.member block_id
            <$> State.gets State.state_blocks
        Nothing -> return False

-- * integrate

integrate_block :: (Cmd.M m) => BlockId -> m ()
integrate_block block_id = do
    perf <- Cmd.get_performance block_id
    lookup_scale <- Cmd.get_lookup_scale
    key <- Perf.get_key block_id Nothing
    events <- Call.Integrate.unwarp block_id
        (LEvent.events_of $ Cmd.perf_events perf)
    let (tracks, errs) = Call.Integrate.integrate lookup_scale key events
    if null errs
        then do
            new_block <- Cmd.Integrate.create block_id tracks
            void $ Create.view new_block
        else Cmd.throw $ "integrate errors: " ++ Seq.join "; " errs
