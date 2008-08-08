module Cmd.ControllerTrack where
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import Ui.Types
import qualified Ui.Event as Event
import qualified Ui.State as State
import qualified Ui.Track as Track

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.Selection as Selection

import qualified App.Config as Config


-- | Receive keystrokes to edit a controller track.  Abort on non-alphabetic
-- keys or if there are any modifiers, so commands still work.
cmd_controller_entry :: Msg.Msg -> Cmd.CmdId
cmd_controller_entry msg = do
    char <- Cmd.require (Msg.alphanum =<< Msg.key msg)
    keys_down <- fmap Map.keys Cmd.keys_down
    when (any (Maybe.isNothing . Cmd.modifier_key) keys_down) Cmd.abort

    (pos, _, track_id) <- Selection.get_insert_pos
    event <- get_event track_id pos (TrackPos 0)
    let event2 = event { Event.event_text = Event.event_text event ++ [char] }
    State.insert_events track_id [(pos, event2)]
    return Cmd.Done


-- | Get the event under insertion point, creating an empty one if there is
-- none.
get_event track_id pos dur = do
    track <- State.get_track track_id
    return $ Maybe.fromMaybe (Config.event "" dur)
        (Track.event_at (Track.track_events track) pos)
