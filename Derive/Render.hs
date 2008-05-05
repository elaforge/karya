{- | The Render layer takes the Score data and turns it into sound.  It has
several backends, such as MIDI, OSC, csound, etc.


    ---- OUT OF DATE ----

        MidiRender

    Convert Score to midi.
    - Sends timestamped midi msgs to a midi player thread that tries to get the
    timing as close as possible.
    - Renders controller curves into a stream of midi msgs, depending on the
    controller resolution settings.
    - Maps pitches to midi note number + pitch bend.
    - Distributes notes to midi channels: if an instrument is given >1 channel,
    it will try to keep notes with overlapping note numbers or controller
    curves on separate channels, so you can e.g. pitch bend one note without
    affecting others, or play two notes on the same nn.
    - Keeps track of outgoing midi bandwidth for display to the user.
    - Saves and sends sysex dumps to configure synths.
    - Can also insert MIDI timing to sync other devices (can I sync cubase this
    way?)


        MidiRecord

    Receive timestamped midi msgs and convert them to Score, which can then be
    converted to Block format and inserted into a block.

        MidiThread

    Receive a stream of timestamped midi msgs and play them accurately.  Also
    receive midi msgs, timestamp them, and relay to the midi input chan.

    Normally the Msg thread is listening to the input chan so it can trigger
    Actions.  However, when recording or monitoring, MidiRecord gets them.  For
    monitoring, MidiRecord also sends incoming msgs back out again, possibly
    after remapping to a different channel and port.
-}
module Derive.Render where
import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Exception
import Control.Monad
import qualified Control.Concurrent as Concurrent
import Data.Function
import qualified Data.IORef as IORef
import qualified Data.Maybe as Maybe

import qualified Util.Seq as Seq
import qualified Util.Log as Log
import qualified Util.Thread as Thread

import qualified Ui.Block as Block
import qualified Ui.Track as Track
import qualified Ui.Event as Event

import qualified Derive.Derive as Derive
import qualified Derive.Player as Player
-- import qualified Derive.RenderMidi as RenderMidi


-- * render

render :: Player.Info -> Block.BlockId -> Derive.Score -> IO Player.Control
render player_info block_id score = do
    control <- fmap Player.Control (IORef.newIORef Player.Play)
    let state = Player.state player_info control block_id
    Thread.start_thread "render midi" $
        render_midi state score `Exception.catch` \exc ->
            Player.write_status state (Player.Died exc)
    return control

render_midi state (Derive.Score name tracks) = do
    Log.notice $ "play score " ++ show name
    Player.write_status state Player.Playing
    let events = (merge_events . map (Track.event_list . Track.track_events)
            . Maybe.catMaybes . map event_track) tracks
    play_midi state events

play_midi state [] = Player.write_status state Player.Completed
play_midi state ((pos, event) : events) = do
    putStrLn $ "*** PLAY " ++ show pos ++ " -- " ++ Event.event_text event
    Concurrent.threadDelay 100000
    pmsg <- Player.check_control state
    if pmsg /= Player.Play
        then Player.write_status state (Player.Stopped (show pmsg))
        else play_midi state events


-- * util

merge_events = foldr (Seq.merge_by (compare `on` fst)) []

event_track (Block.T track _) = Just track
event_track _ = Nothing
