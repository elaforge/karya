module Cmd.PitchTrack where
import qualified Data.Maybe as Maybe

import qualified Ui.Key as Key
import qualified Cmd.ControlTrack as ControlTrack
import qualified Cmd.Cmd as Cmd
import qualified Cmd.EditUtil as EditUtil
import qualified Cmd.Selection as Selection

import qualified Perform.Pitch as Pitch


-- | Raw edit is awkward because of the "[meth,]val" syntax.
cmd_raw_edit :: Pitch.ScaleId -> Cmd.Cmd
cmd_raw_edit = cmd_val_edit

cmd_val_edit :: Pitch.ScaleId -> Cmd.Cmd
cmd_val_edit scale_id msg = do
    note <- EditUtil.note_key scale_id msg
    tracksel <- Selection.get_insert_track
    cmd_val_edit_at tracksel note
    return Cmd.Done

cmd_method_edit :: Cmd.Cmd
cmd_method_edit msg = do
    tracksel <- Selection.get_insert_track
    key <- EditUtil.alpha_key msg
    cmd_method_edit_at tracksel key
    return Cmd.Done

cmd_val_edit_at :: (Monad m) => Selection.TrackSel -> Maybe Pitch.Note
    -> Cmd.CmdT m ()
cmd_val_edit_at tracksel note = do
    modify_event_at tracksel $ \(method, _) -> case note of
        Nothing -> (Nothing, Nothing)
        Just n -> (Just method, Just (Pitch.note_text n))

cmd_method_edit_at :: (Monad m) => Selection.TrackSel -> Key.Key
    -> Cmd.CmdT m ()
cmd_method_edit_at tracksel key = do
    modify_event_at tracksel $
        \(method, val) -> (EditUtil.modify_text_key key method, Just val)

-- | Record the last note entered.  Should be called by 'with_note'.
cmd_record_note_status :: Pitch.ScaleId -> Cmd.Cmd
cmd_record_note_status scale_id msg = do
    status <- case EditUtil.get_note scale_id msg of
        Just (Right (Just note)) -> return $ Pitch.note_text note
        _ -> Cmd.abort
    Cmd.set_status "note" (Just status)
    return Cmd.Continue

-- * implementation

modify_event_at :: (Monad m) => Selection.TrackSel
    -> ((String, String) -> (Maybe String, Maybe String)) -> Cmd.CmdT m ()
modify_event_at tracksel f =
    EditUtil.modify_event_at tracksel True
        (ControlTrack.unparse . f . ControlTrack.parse)
