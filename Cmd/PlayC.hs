-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | The FFI-using part of "Cmd.Play".  It uses the FFI because the play
    monitor wants to directly call to the UI to update the playback indicator,
    which is much more efficient than going through the whole "Ui.Diff" thing.

    But that means "Cmd.Play" would import FFI-using modules, which causes
    a problem for ghci, and, since I want the Cmd.Play functions to be
    available there, for the REPL as well.  So I engage in a hack: the play cmd
    returns a special 'Cmd.PlayMidi' value which contains the arguments for the
    play monitor thread.  The responder treats it as a Done but will call
    'play' with the given args.
-}
module Cmd.PlayC (cmd_play_msg, play) where
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Vector as Vector

import Util.Control
import qualified Util.Log as Log
import qualified Util.Num as Num
import qualified Util.Seq as Seq
import qualified Util.Thread as Thread

import qualified Ui.Block as Block
import qualified Ui.Color as Color
import qualified Ui.State as State
import qualified Ui.Sync as Sync
import qualified Ui.Track as Track
import qualified Ui.Ui as Ui

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.Perf as Perf

import qualified Derive.Score as Score
import qualified Derive.Stack as Stack
import qualified Perform.Midi.Play as Midi.Play
import qualified Perform.Transport as Transport
import qualified App.Config as Config
import Types


-- * cmd_play_msg

-- | Respond to msgs about derivation and playing status.
cmd_play_msg :: Ui.Channel -> Msg.Msg -> Cmd.CmdIO
cmd_play_msg ui_chan msg = do
    case msg of
        Msg.Transport status -> transport_msg status
        Msg.DeriveStatus block_id status -> derive_status_msg block_id status
        _ -> Cmd.abort
    return Cmd.Done
    where
    transport_msg status = case status of
        Transport.Playing -> set_all_play_boxes Config.play_color
        -- Either the performer has declared itself stopped, or the play
        -- monitor has declared it stopped.  In any case, I don't need
        -- a transport to tell it what to do anymore.
        Transport.Stopped -> do
            Cmd.modify_play_state $ \st ->
                st { Cmd.state_play_control = Nothing }
            -- This will cover up derive status info, but that should be ok.
            -- And play normally only comes after derive is completed.
            set_all_play_boxes Config.box_color
        Transport.Died err_msg -> Log.warn $ "player died: " <> txt err_msg
    derive_status_msg block_id status = do
        whenJust (derive_status_color status) (State.set_play_box block_id)
        case status of
            Msg.DeriveComplete perf -> do
                Cmd.modify_play_state $ \st -> st
                    { Cmd.state_performance = Map.insert block_id
                         perf (Cmd.state_performance st)
                    }
                    -- Don't mess with state_current_performance because Play
                    -- may have flipped the 'Cmd.perf_logs_written' bit.
                update_track_signals ui_chan block_id
                    (Cmd.perf_track_signals perf)
                update_highlights ui_chan block_id (Cmd.perf_events perf)
            _ -> return ()
    derive_status_color status = case status of
        Msg.OutOfDate {} -> Just $ Color.brightness 1.5 Config.busy_color
        Msg.Deriving {} -> Just Config.busy_color
        Msg.DeriveComplete {} -> Just Config.box_color

set_all_play_boxes :: State.M m => Color.Color -> m ()
set_all_play_boxes color =
    mapM_ (flip State.set_play_box color) =<< State.all_block_ids

type Range = (TrackTime, TrackTime)

-- ** track signals

update_track_signals :: Ui.Channel -> BlockId -> Track.TrackSignals
    -> Cmd.CmdT IO ()
update_track_signals ui_chan block_id tsigs = do
    rendering <- rendering_tracks block_id
    let empty = Track.empty_track_signal
    liftIO $ Sync.set_track_signals ui_chan $
        [ (view_id, tracknum, if wants_tsig
            then Map.findWithDefault empty (block_id, track_id) tsigs
            else empty)
        | (view_id, track_id, tracknum, wants_tsig) <- rendering
        ]

-- | Get the tracks of this block and whether they want to render a signal.
rendering_tracks :: State.M m => BlockId
    -> m [(ViewId, TrackId, TrackNum, Bool)]
rendering_tracks block_id = do
    view_ids <- Map.keys <$> State.views_of block_id
    blocks <- mapM State.block_of view_ids
    btracks <- mapM get_tracks blocks
    return $ do
        (view_id, tracks) <- zip view_ids btracks
        ((tracknum, track_id, flags), track) <- tracks
        -- I don't even want to send an empty signal to these.
        guard $ Block.Collapse `Set.notMember` flags
        return (view_id, track_id, tracknum,
            Block.track_wants_signal flags track)
    where
    get_tracks block = zip triples <$> mapM State.get_track track_ids
        where
        track_ids = [tid | (_, tid, _) <- triples]
        triples =
            [ (tracknum, tid, Block.track_flags track)
            | (tracknum,
                track@(Block.Track { Block.tracklike_id = Block.TId tid _ }))
                <- zip [0..] (Block.block_tracks block)
            ]

-- ** highlights

-- | Get highlights from the events, clear old highlighs, and set the new ones.
update_highlights :: Ui.Channel -> BlockId -> Msg.Events -> Cmd.CmdT IO ()
update_highlights ui_chan block_id events = do
    sels <- get_event_highlights block_id events
    view_ids <- State.gets $ Map.keysSet . State.state_views
    let used_view_ids = Set.fromList [view_id | ((view_id, _), _) <- sels]
    liftIO $ do
        Sync.clear_highlights ui_chan $
            Set.toList $ view_ids `Set.difference` used_view_ids
        Sync.set_highlights ui_chan sels

-- | Get highlight selections from the events.
get_event_highlights :: Cmd.M m => BlockId -> Cmd.Events
    -> m [((ViewId, TrackNum), (Range, Color.Color))]
get_event_highlights block_id events = do
    colors <- Cmd.gets $ Cmd.state_highlight_colors . Cmd.state_config
    resolve_tracks $ event_highlights block_id colors events

resolve_tracks :: State.M m => [((BlockId, TrackId), a)]
    -> m [((ViewId, TrackNum), a)]
resolve_tracks = concatMapM resolve
    where
    resolve ((block_id, track_id), val) = do
        tracknum <- State.get_tracknum_of block_id track_id
        view_ids <- Map.keys <$> State.views_of block_id
        return [((view_id, tracknum), val) | view_id <- view_ids]

event_highlights :: BlockId -> Map.Map Color.Highlight Color.Color -> Cmd.Events
    -> [((BlockId, TrackId), (Range, Color.Color))]
event_highlights derived_block_id colors
    | Map.null colors = const []
    | otherwise = Seq.unique_on key . Vector.foldr collect []
    where
    key (track, (range, _)) = (track, range)
    collect event accum
        | highlight /= Color.NoHighlight,
                Just (block_id, track_id, range) <- maybe_pos,
                block_id == derived_block_id,
                Just color <- Map.lookup highlight colors =
            ((block_id, track_id), (range, color)) : accum
        | otherwise = accum
        where
        highlight = Score.event_highlight event
        maybe_pos = Stack.block_track_region_of $ Score.event_stack event

-- * play

-- | This actually kicks off a MIDI play thread, and if an inverse tempo
-- function is given, a play monitor thread.
play :: Ui.Channel -> State.State -> Transport.Info -> Cmd.PlayMidiArgs
    -> IO Transport.PlayControl
play ui_chan ui_state transport_info
        (Cmd.PlayMidiArgs mmc name msgs maybe_inv_tempo repeat_at) = do
    (play_ctl, monitor_ctl) <-
        Midi.Play.play transport_info mmc name msgs repeat_at
    -- Pass the current state in the MVar.  ResponderSync will keep it up
    -- to date afterwards, but only if blocks are added or removed.
    MVar.modifyMVar_ (Transport.info_state transport_info) $
        const (return ui_state)
    liftIO $ void $ Thread.start $ case maybe_inv_tempo of
        Just inv_tempo -> play_monitor_thread ui_chan transport_info
            monitor_ctl inv_tempo repeat_at
        Nothing -> passive_play_monitor_thread
            (Transport.info_send_status transport_info) monitor_ctl
    return play_ctl

-- | Just coordinate Playing and Stopped, don't update a play position.
passive_play_monitor_thread :: (Transport.Status -> IO ())
    -> Transport.PlayMonitorControl -> IO ()
passive_play_monitor_thread send monitor_ctl =
    Exception.bracket_ (send Transport.Playing) (send Transport.Stopped) $
        Transport.wait_player_stopped monitor_ctl

-- | Run along the InverseTempoMap and update the play position selection.
-- Note that this goes directly to the UI through Sync, bypassing the usual
-- state diff folderol.
play_monitor_thread :: Ui.Channel -> Transport.Info
    -> Transport.PlayMonitorControl -> Transport.InverseTempoFunction
    -> Maybe RealTime -> IO ()
play_monitor_thread ui_chan transport_info ctl inv_tempo_func repeat_at = do
    let get_now = Transport.info_get_current_time transport_info
    -- This won't be exactly the same as the renderer's ts offset, but it's
    -- probably close enough.
    offset <- get_now
    let state = UpdaterState
            { monitor_ctl = ctl
            , monitor_offset = offset
            , monitor_get_now = get_now
            , monitor_inv_tempo_func = inv_tempo_func
            , monitor_active_sels = Set.empty
            , monitor_ui_state = Transport.info_state transport_info
            , monitor_repeat_at = repeat_at
            , monitor_ui_channel = ui_chan
            }
    ui_state <- MVar.readMVar (monitor_ui_state state)
    Sync.clear_play_position ui_chan $
        Map.keys $ State.state_views ui_state
    Exception.bracket_
        (Transport.info_send_status transport_info Transport.Playing)
        (Transport.info_send_status transport_info Transport.Stopped)
        (monitor_loop state)

data UpdaterState = UpdaterState {
    monitor_ctl :: Transport.PlayMonitorControl
    -- | When the monitor thread started.
    , monitor_offset :: RealTime
    , monitor_get_now :: IO RealTime
    , monitor_inv_tempo_func :: Transport.InverseTempoFunction
    , monitor_active_sels :: Set.Set (ViewId, [TrackNum])
    , monitor_ui_state :: MVar.MVar State.State
    , monitor_repeat_at :: Maybe RealTime
    , monitor_ui_channel :: Ui.Channel
    }

monitor_loop :: UpdaterState -> IO ()
monitor_loop state = do
    now <- maybe id (flip Num.fmod) (monitor_repeat_at state)
        . subtract (monitor_offset state) <$> monitor_get_now state
    let fail err = Log.error ("state error in play monitor: " <> showt err)
            >> return []
    ui_state <- MVar.readMVar (monitor_ui_state state)
    let block_pos = monitor_inv_tempo_func state now
    play_pos <- either fail return $ State.eval ui_state $
        Perf.block_pos_to_play_pos block_pos
    Sync.set_play_position (monitor_ui_channel state) play_pos

    let active_sels = Set.fromList
            [(view_id, map fst num_pos) | (view_id, num_pos) <- play_pos]
    Sync.clear_play_position (monitor_ui_channel state) $ map fst $ Set.toList $
        Set.difference (monitor_active_sels state) active_sels
    state <- return $ state { monitor_active_sels = active_sels }

    stopped <- Transport.poll_player_stopped (monitor_ctl state)
    -- If repeat_at is on, then playback will never stop on its own, even if
    -- I run out of tempo map (which happens if the repeat point is at or past
    -- the end of the score).  When the monitor thread stops, it sends Stopped
    -- on the transport, which clears out the player ctl which will make the
    -- player unstoppable, if it's still going.
    if stopped || (null block_pos && Maybe.isNothing (monitor_repeat_at state))
        then do
            Sync.clear_play_position (monitor_ui_channel state) $ map fst $
                Set.toList (monitor_active_sels state)
            unless stopped $ wait_for_stop $
                Transport.poll_player_stopped (monitor_ctl state)
        else Thread.delay 0.05 >> monitor_loop state
    where
    wait_for_stop ctl = do
        Thread.delay 0.1
        stopped <- ctl
        unless stopped $ wait_for_stop ctl
