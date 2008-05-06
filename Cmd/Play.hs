{- | Master control for playing blocks.

- Find the relevant block to play.
- Start a thread to start feeding the notelist from the block to its sink.
- The process is dependent on the backend, and may need access to the midi
device map and writer (midi backend), or IO (language backend), or socket IO
(osc backend).
- Add absolute time to the note events' times.
- Optionally loop a block.
- Receive position update msgs and advance the playback selection.

- On cancel, kill the thread, and invoke a backend specific means to cancel
outstanding notes (flush midi port, kill external renderer, ...).


Dependent on backend:
- send notelist to sink
- convert from absolute time to device specific time
- for realtime protocols, how far ahead of playback time to stay


Here's how it works:

- Deriver generates a renderable Score and an inverse tempo map.

- The Score is passed on to Render, which starts whatever threads are necessary
to play the Score, and returns a transport control mutable that can be used to
stop the playback.

- The transport and tempo map are passed to a play display updater, which uses
the tempo map to display the play position in the various blocks, and aborts
along with the renderer if the transport says to stop.

- Completion: Both the renderer and the play updater have the play_chan to send
play status msgs to the responder.  The renderer uses it to send info about
errors rendering back to the UI, and the updater uses it to send a Completed
msg when it's done, for whatever reason.  An error from the renderer will cause
the responder to set the transport to Stop, and a Completed from the updater
will update the play_box on the UI to indicate that the block is no longer
playing.

-}
module Cmd.Play where
import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Exception
import Control.Monad
import qualified Control.Monad.Trans as Trans
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Util.Log as Log
import qualified Util.Thread as Thread
import qualified Ui.Block as Block
import qualified Ui.State as State
import qualified Ui.Sync as Sync

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg

import qualified Derive.Derive as Derive
import qualified Derive.Render as Render
import qualified Derive.Player as Player

import qualified App.Config as Config


cmd_play_block :: Player.Info -> Cmd.CmdT IO Cmd.Status
cmd_play_block player_info = do
    cmd_state <- Cmd.get_state
    case Cmd.state_player_transport cmd_state of
        Just _ -> Log.warn "player already running" >> Cmd.abort
        _ -> return ()
    view_id <- Cmd.get_active_view
    block_id <- find_play_block view_id
    block <- State.get_block block_id
    deriver <- get_deriver (Block.block_deriver block)
    ui_state <- State.get

    score <- Derive.get_block_score block
    let (derived_score, tempo_map) = Derive.derive deriver ui_state score
    transport <- Trans.liftIO $
        Render.render player_info block_id derived_score

    Trans.liftIO $ Thread.start_thread "play position updater" $
        update_play_position transport player_info tempo_map ui_state

    Cmd.modify_state $ \st ->
        st { Cmd.state_player_transport = Just transport }
    return Cmd.Done

update_play_position transport player_info tempo_map ui_state = do
    let view_blocks = Map.assocs (Map.map Block.view_block
                (State.state_views ui_state))
        block_ids = map snd view_blocks
        player_chan = Player.info_player_chan player_info
        get_cur_ts = Player.info_get_current_timestamp player_info
    -- This won't be exactly the same as the renderer's ts offset, but it's
    -- probably close enough.
    ts_offset <- get_cur_ts
    Exception.bracket_
        (mapM_ (Player.write_status player_chan Player.Playing) block_ids)
        (mapM_ (Player.write_status player_chan Player.Stopped) block_ids)
        (updater_loop transport ts_offset get_cur_ts tempo_map view_blocks)

updater_loop transport ts_offset get_cur_ts tempo_map view_blocks = do
    -- Offset from current time back to block start relative time.
    cur_ts <- fmap (\ts -> ts - ts_offset) get_cur_ts
    let spos = map (\(_, block_id) -> tempo_map block_id cur_ts) view_blocks
    forM_ (zip view_blocks spos) $ \((view_id, _), maybe_pos) ->
        Sync.set_play_position view_id maybe_pos

    tmsg <- Player.check_transport transport
    -- putStrLn $ "UPDATER at " ++ show cur_ts ++ ": "
    --     ++ show tmsg ++ ", " ++ show spos
    -- Either the transport is stopped or I ran out of tempo map.
    when (tmsg == Player.Play && not (all Maybe.isNothing spos)) $ do
        -- Concurrent.threadDelay (10^5)
        Concurrent.threadDelay 50000
        updater_loop transport ts_offset get_cur_ts tempo_map view_blocks

-- play_block_loop = do

cmd_stop :: Cmd.CmdT IO Cmd.Status
cmd_stop = do
    maybe_control <- fmap Cmd.state_player_transport Cmd.get_state
    transport <- case maybe_control of
        Nothing -> Log.warn "player thread not running" >> Cmd.abort
        Just transport -> return transport
    Trans.liftIO $ Player.send_transport transport Player.Stop
    return Cmd.Done

-- | Respond to player status msgs coming back from the player thread.
cmd_player_msg :: (Monad m) => Msg.Msg -> Cmd.CmdT m Cmd.Status
cmd_player_msg msg = do
    (block_id, status) <- case msg of
        Msg.Player (Player.Status block_id status) -> return (block_id, status)
        _ -> Cmd.abort
    State.set_play_box block_id (play_state_color status)
    Log.notice $ "player status for " ++ show block_id ++ ": " ++ show status
    case status of
        Player.Playing -> return ()
        -- Since 'update_play_position' is the only thing that sends Stopped,
        -- and it does that when it exits, the updater thread should be
        -- finished here.
        Player.Stopped -> Cmd.modify_state $ \st ->
            st { Cmd.state_player_transport = Nothing }
        Player.Died exc -> Log.notice ("player died: " ++ show exc)
    return Cmd.Done


-- * util

play_state_color status = case status of
    Player.Playing -> Config.play_color
    _ -> Config.box_color

get_deriver deriver_id = maybe
    (State.throw $ "deriver not found: " ++ show deriver_id)
    return (Derive.get_deriver deriver_id)

-- | Find the block to play, relative to the given view.
-- find_play_block :: State.State -> Block.ViewId -> Block.BlockId
find_play_block view_id = do
    view <- State.get_view view_id
    return (Block.view_block view)
