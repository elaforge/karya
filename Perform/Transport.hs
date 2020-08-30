-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | The transport is the communication mechanism between the app and the
    performer.  Extensive description is in the Cmd.Play docstring.
-}
module Perform.Transport (
    -- * info
    Status(..), Info(..)
    -- * play control
    , PlayControl, play_control
    , stop_player, poll_stop_player
    -- * play monitor control
    , PlayMonitorControl, play_monitor_control
    , player_stopped, poll_player_stopped, wait_player_stopped
    -- * play timing
    , TempoFunction, ClosestWarpFunction, InverseTempoFunction, Stop(..)
) where
import qualified Control.Concurrent.MVar as MVar

import qualified Util.Thread as Thread
import qualified Midi.Interface as Interface
import qualified Ui.Ui as Ui
import qualified Derive.Warp as Warp
import Global
import Types


-- | These go back to the responder loop from the render thread to notify it
-- about the transport's state.
data Status = Playing | Stopped | Died !String
    deriving (Eq, Show)

instance Pretty Status where
    pretty = \case
        Playing -> "Playing"
        Stopped -> "Stopped"
        Died msg -> "Died: " <> txt msg

-- | Data needed by the MIDI player thread.  This is created during app setup
-- and passed directly to the play cmds by the responder loop.  When the play
-- is started, it's incorporated into the play 'Perform.Midi.Play.State'.
data Info = Info {
    -- | Send status messages back to the responder loop.
    info_send_status :: Status -> IO ()
    , info_midi_writer :: Interface.Message -> IO ()
    -- | Action that will abort any pending midi msgs written with the midi
    -- writer.
    , info_midi_abort :: IO ()
    -- | Get current RealTime according to timing system.
    , info_get_current_time :: IO RealTime
    -- | A mutable map of the currently active views, so the responder can
    -- tell the play monitor thread which views are currently opened.  It needs
    -- this so if you open a new view while it's playing, it can put the play
    -- selection on that view.
    , info_state :: MVar.MVar Ui.State
    }

-- * play control

-- | Communication from the responder to the player, to tell the player to
-- stop.
newtype PlayControl = PlayControl Thread.Flag deriving (Show)

play_control :: IO PlayControl
play_control = PlayControl <$> Thread.flag

-- | Signal to the player that you'd like it to start stopping doing whatever
-- it is that it's doing and just like stop now ok?  Is that ok?
stop_player :: PlayControl -> IO ()
stop_player (PlayControl flag) = Thread.set flag

poll_stop_player :: Thread.Seconds -> PlayControl -> IO Bool
poll_stop_player timeout (PlayControl flag) = Thread.poll timeout flag

-- * play monitor control

-- | Communication from the player to the responder, to say when it's stopped.
newtype PlayMonitorControl = PlayMonitorControl Thread.Flag

play_monitor_control :: IO PlayMonitorControl
play_monitor_control = PlayMonitorControl <$> Thread.flag

-- | Signal that the player has stopped.
player_stopped :: PlayMonitorControl -> IO ()
player_stopped (PlayMonitorControl flag) = Thread.set flag

-- | True if the player has stopped.
poll_player_stopped :: Thread.Seconds -> PlayMonitorControl -> IO Bool
poll_player_stopped timeout (PlayMonitorControl flag) = Thread.poll timeout flag

wait_player_stopped :: PlayMonitorControl -> IO ()
wait_player_stopped (PlayMonitorControl flag) = Thread.wait flag


-- * play timing

-- | Given a score time on a certain track in a certain block, give the real
-- times that it corresponds to.  There may be more than one if the block
-- has been derived in more than one place, and there may be zero if the block
-- and track combination wasn't derived at all or didn't extend to the given
-- score time.
type TempoFunction = BlockId -> TrackId -> ScoreTime -> [RealTime]

-- | This is similar to 'TempoFunction' but finds the warp for the given block
-- that occurs closest to the given RealTime.  Callers can use the warp to
-- find multiple real times on that block.
--
-- 'TempoFunction' simply returns all real times for a given score time, with
-- no control over whether they come from the same block or not.
type ClosestWarpFunction = BlockId -> TrackId -> RealTime -> Warp.Warp

-- | Return the ScoreTime play position in the various playing blocks at the
-- given physical time.  If the RealTime is past the end of all playing blocks,
-- return [].  The play monitor thread polls this periodically for all
-- displayed blocks and updates the play selection accordingly.
--
-- Since a given block may be playing in multiple places at the same time (e.g.
-- for a block that is played like an instrument, if the notes overlap), the
-- same BlockId may occur more than once in the output list.
type InverseTempoFunction =
    Stop -> RealTime -> [(BlockId, [(TrackId, ScoreTime)])]

-- | Configure 'InverseTempoFunction'.  TODO think of better names
data Stop =
    -- | Stop emitting ScoreTime as soon as the events on the block end.
    -- This is used by the play monitor, since I want it to stop as soon as
    -- the score is "over" visually.
    StopAtEnd
    -- | Keep emitting score time for all blocks.  This is used to map from
    -- RealTime to ScoreTime for a particular block, regardless of the notes on
    -- it.  E.g. Msg.ImProgress uses this because synthesis continues as long
    -- as the notes ring.
    | NoStop
    deriving (Eq, Show)
