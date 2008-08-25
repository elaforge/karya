module Cmd.ControllerTrack where
import Control.Monad
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Util.Seq as Seq

import Ui.Types
import qualified Ui.Event as Event
import qualified Ui.Id as Id
import qualified Ui.Key as Key
import qualified Ui.State as State
import qualified Ui.Track as Track

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.Selection as Selection

import qualified App.Config as Config


-- | Receive keystrokes to edit a controller track.
cmd_raw_edit :: Msg.Msg -> Cmd.CmdId
cmd_raw_edit msg = do
    key <- edit_key msg
    (track_id, _, pos) <- Selection.get_insert_track
    event <- get_event track_id pos (TrackPos 0)
    when (key == Key.Backspace && Event.event_text event == "")
        Cmd.abort -- pass the backspace on to the remove event command
    let event2 = event
            { Event.event_text = modify_text (Event.event_text event) key }
    State.insert_events track_id [(pos, event2)]
    return Cmd.Done


-- | Get the event under insertion point, creating an empty one if there is
-- none.
get_event :: (State.UiStateMonad m) =>
    Track.TrackId -> TrackPos -> TrackPos -> m Event.Event
get_event track_id pos dur = do
    track <- State.get_track track_id
    return $ Maybe.fromMaybe (Config.event "" dur)
        (Track.event_at (Track.track_events track) pos)

-- | Get a keystroke from a Msg.  Abort on non-alphabetic keys or if there are
-- any modifiers, so commands still work.
edit_key :: (Monad m) => Msg.Msg -> Cmd.CmdT m Key.Key
edit_key msg = do
    key <- Cmd.require (Msg.key msg)
    keys_down <- fmap Map.keys Cmd.keys_down
    -- Also abort if there are modifiers down, so commands still work.
    when (not (is_edit_key key) ||
        any (Maybe.isNothing . Cmd.modifier_key) keys_down) Cmd.abort
    return key

modify_text s Key.Backspace = Seq.rdrop 1 s
modify_text s (Key.KeyChar c) | Char.isPrint c = s ++ [c]
modify_text s _ = s

is_edit_key Key.Backspace = True
is_edit_key (Key.KeyChar c) | Id.is_identifier c = True
is_edit_key _ = False
