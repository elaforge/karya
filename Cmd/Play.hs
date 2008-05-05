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


TODO:
- verify how multiple threads generating / consuming a list work
- find out how to correlate real time with portmidi timer
- simple synchronous deriver and midi backend

-}
module Cmd.Play where
import Control.Monad
import qualified Control.Monad.Trans as Trans

import qualified Util.Log as Log
import qualified Ui.Block as Block
import qualified Ui.State as State

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg

import qualified Derive.Derive as Derive
import qualified Derive.Render as Render
import qualified Derive.Player as Player

import qualified App.Config as Config


cmd_play_block :: Player.Info -> Cmd.CmdT IO Cmd.Status
cmd_play_block player_info = do
    cmd_state <- Cmd.get_state
    case Cmd.state_player_control cmd_state of
        Just _ -> Log.warn "player thread already running" >> Cmd.abort
        _ -> return ()
    view_id <- Cmd.get_active_view
    block_id <- find_play_block view_id
    block <- State.get_block block_id
    deriver <- get_deriver (Block.block_deriver block)
    st <- State.get

    score <- Derive.get_block_score block
    let derived_score = Derive.derive deriver st score
    player_control <- Trans.liftIO $
        Render.render player_info block_id derived_score
    Cmd.modify_state $ \st ->
        st { Cmd.state_player_control = Just player_control }
    return Cmd.Done

-- play_block_loop = do

cmd_stop :: Cmd.CmdT IO Cmd.Status
cmd_stop = do
    maybe_control <- fmap Cmd.state_player_control Cmd.get_state
    control <- case maybe_control of
        Nothing -> Log.warn "player thread not running" >> Cmd.abort
        Just control -> return control
    Trans.liftIO $ Player.send control Player.Stop
    return Cmd.Done

-- | Respond to player status msgs coming back from the player thread.
cmd_player_msg :: (Monad m) => Msg.Msg -> Cmd.CmdT m Cmd.Status
cmd_player_msg msg = do
    (block_id, status) <- case msg of
        Msg.Player (Player.Status block_id status) -> return (block_id, status)
        _ -> Cmd.abort
    State.set_play_box block_id (play_state_color status)
    Log.notice $ "player status for " ++ show block_id ++ ": " ++ show status
    when (status /= Player.Playing) $
        Cmd.modify_state $ \st -> st { Cmd.state_player_control = Nothing }
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
