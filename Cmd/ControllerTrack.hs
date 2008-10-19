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


-- | Receive keystrokes to edit a controller track.
cmd_raw_edit :: Msg.Msg -> Cmd.CmdId
cmd_raw_edit msg = do
    key <- edit_key msg
    (track_id, _, pos) <- Selection.get_insert_track
    event <- get_event track_id pos (TrackPos 0)
    let event2 = event
            { Event.event_text = modify_text key (Event.event_text event) }
    if null (Event.event_text event2)
        then State.remove_event track_id pos
        else State.insert_events track_id [(pos, event2)]
    return Cmd.Done


-- | Get the event under insertion point, creating an empty one if there is
-- none.
get_event :: (State.UiStateMonad m) =>
    Track.TrackId -> TrackPos -> TrackPos -> m Event.Event
get_event track_id pos dur = do
    track <- State.get_track track_id
    return $ Maybe.fromMaybe (Event.event "" dur)
        (Track.event_at (Track.track_events track) pos)

-- | Get a keystroke from Msg, aborting if it's not appropriate.  Raw edit mode
-- snags all printable keys.  Also abort if there are modifiers, so commands
-- still work.
raw_edit_key :: (Monad m) => Msg.Msg -> Cmd.CmdT m Key.Key
raw_edit_key msg = do
    key <- Cmd.require (Msg.key msg)
    keys_down <- fmap Map.keys Cmd.keys_down
    -- Abort if there are modifiers down, so commands still work.
    -- Except shift, of course.
    let non_modifier mod = case mod of
            Cmd.KeyMod k -> case k of
                Key.KeyChar _ -> False
                Key.ShiftL -> False
                Key.ShiftR -> False
                _ -> True
            _ -> True
    when (not (is_edit_key key) || any non_modifier keys_down) Cmd.abort
    return key

-- | Like 'raw_edit_key' except only accept identifier characters, for editing
-- methods and stuff.  This way special keys like zoom and play still work.
edit_key :: (Monad m) => Msg.Msg -> Cmd.CmdT m Key.Key
edit_key msg = do
    key <- raw_edit_key msg
    case key of
        Key.KeyChar c | not (Id.is_identifier c) -> Cmd.abort
        _ -> return ()
    return key

modify_text :: Key.Key -> String -> String
modify_text Key.Backspace s = Seq.rdrop 1 s
modify_text (Key.KeyChar c) s | Char.isPrint c = s ++ [c]
modify_text _ s = s

is_edit_key Key.Backspace = True
is_edit_key (Key.KeyChar c) | Char.isPrint c = True
is_edit_key _ = False
