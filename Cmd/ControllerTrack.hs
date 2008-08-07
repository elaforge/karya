module Cmd.ControllerTrack where
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Util.Log as Log

import Ui.Types
import qualified Ui.Event as Event
import qualified Ui.Key as Key
import qualified Ui.State as State
import qualified Ui.Track as Track

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.Selection as Selection

import qualified App.Config as Config


-- | Receive keystrokes to edit a controller track.
cmd_controller_entry :: Msg.Msg -> Cmd.CmdId
cmd_controller_entry msg = do
    key <- Cmd.require (Msg.key msg)
    char <- Cmd.require $ case key of
        Key.KeyChar char -> Just char
        _ -> Nothing
    keys_down <- fmap Map.keys Cmd.keys_down
    when (any (Maybe.isNothing . Cmd.modifier_key) keys_down) $
        Cmd.abort

    (insert_pos, _, track_id) <- Selection.get_insert_pos
    track <- State.get_track track_id

    let text = Maybe.fromMaybe "" $ fmap Event.event_text
            (Track.event_at (Track.track_events track) insert_pos)
        event = Config.event (text ++ [char]) (TrackPos 0)
    Log.debug $ "modify control event at " ++ show insert_pos ++ " "
        ++ show text ++ " ++ " ++ show char
    State.insert_events track_id [(insert_pos, event)]
    return Cmd.Done

