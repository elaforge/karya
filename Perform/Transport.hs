{- | The transport is the communication mechanism between the app and the
    performer.  Extensive description is in the Cmd.Play docstring.
-}
module Perform.Transport where
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM as STM
import qualified Data.IORef as IORef

import qualified Util.Thread as Thread
import qualified Midi.Midi as Midi
import Ui
import qualified Ui.State as State
import qualified Derive.Score as Score


-- | These go back to the responder loop from the render thread to notify it
-- about the transport's state.
data Status = Playing | Stopped | Died String
    deriving (Eq, Show)

-- | Data needed by the player thread.  This is created during app setup and
-- passed directly to the play cmds by the responder loop.  When the play is
-- started, it's incorporated into the play 'State'.
data Info = Info {
    -- | Send status messages back to the responder loop.
    info_send_status :: Status -> IO ()
    , info_midi_writer :: Midi.WriteMessage -> IO ()
    -- | Action that will abort any pending midi msgs written with the midi
    -- writer.
    , info_midi_abort :: IO ()
    -- | Get current RealTime according to timing system.
    , info_get_current_time :: IO RealTime
    -- | A mutable map of the currently active views, so the responder can
    -- tell the updater thread which views are currently opened.
    , info_state :: MVar.MVar State.State
    }

-- * Transport control

-- | Send msgs from the responder loop to the player thread.
-- Communication from the responder to the player (tell the player to stop),
-- and from the player to the updater (tell the updater it's stopped).

newtype PlayControl = PlayControl (STM.TMVar ())

-- Make Cmd.State showable for debugging.
instance Show PlayControl where
    show _ = "((PlayControl))"

stop_player :: PlayControl -> IO Bool
stop_player (PlayControl mv) = STM.atomically (STM.tryPutTMVar mv ())

check_for_stop :: Double -> PlayControl -> IO Bool
check_for_stop timeout (PlayControl mv) = do
    val <- Thread.take_tmvar_timeout timeout mv
    return $ case val of
        Nothing -> False
        Just _ -> True

newtype UpdaterControl = UpdaterControl (IORef.IORef Bool)

player_stopped :: UpdaterControl -> IO ()
player_stopped (UpdaterControl ref) = IORef.writeIORef ref True

check_player_stopped :: UpdaterControl -> IO Bool
check_player_stopped (UpdaterControl ref) = IORef.readIORef ref


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
type ClosestWarpFunction = BlockId -> TrackId -> RealTime -> Score.Warp

-- | Return the ScoreTime play position in the various playing blocks at the
-- given physical time.  If the RealTime is past the end of all playing
-- blocks, return [].  The updater thread polls this periodically for all
-- displayed blocks and updates the play selection accordingly.
--
-- Since a given block may be playing in multiple places at the same time (e.g.
-- for a block that is played like an instrument, if the notes overlap), the
-- same BlockId may occur more than once in the output list.
type InverseTempoFunction = RealTime -> [(BlockId, [(TrackId, ScoreTime)])]
