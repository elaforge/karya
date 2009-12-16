{-# LANGUAGE ViewPatterns #-}
{- | Cmds to edit a pitch track, which is a special kind of control track.

    This module creates the pitches that are later parsed by Derive.Control.
-}
module Cmd.PitchTrack where
import qualified Control.Arrow as Arrow

import qualified Ui.Event as Event
import qualified Ui.Key as Key
import qualified Cmd.Cmd as Cmd
import qualified Cmd.ControlTrack as ControlTrack
import qualified Cmd.EditUtil as EditUtil
import qualified Cmd.InputNote as InputNote
import qualified Cmd.Msg as Msg
import qualified Cmd.Selection as Selection

import qualified Perform.Pitch as Pitch

import qualified Derive.Control as Control


-- | Raw edit is awkward because of the "[meth,]val" syntax.
cmd_raw_edit :: Pitch.ScaleId -> Cmd.Cmd
cmd_raw_edit = cmd_val_edit

cmd_val_edit :: Pitch.ScaleId -> Cmd.Cmd
cmd_val_edit scale_id msg = do
    EditUtil.fallthrough msg
    case msg of
        Msg.InputNote (InputNote.NoteOn _ key _) -> do
            sel_pos <- EditUtil.get_sel_pos
            note <- EditUtil.parse_key scale_id key
            val_edit_at sel_pos note
            Selection.advance
        (Msg.key_down -> Just Key.Backspace) -> do
            EditUtil.modify_event False (const (Nothing, True))
        _ -> Cmd.abort
    return Cmd.Done

cmd_val_edit_relative :: Cmd.Cmd
cmd_val_edit_relative msg = do
    selpos <- EditUtil.get_sel_pos
    cmd_val_edit_relative_at selpos msg

cmd_val_edit_relative_at :: EditUtil.SelPos -> Cmd.Cmd
cmd_val_edit_relative_at selpos msg = do
    EditUtil.fallthrough msg
    case msg of
        Msg.InputNote (InputNote.NoteOn _ key _) -> do
            let Pitch.Note note = Control.unparse_relative (key_to_relative key)
            modify_event_at selpos $ \(meth, _) ->
                ((Just meth, Just note), True)
        (EditUtil.raw_key -> Just key) | key /= Key.KeyChar ' ' -> do
            modify_event_at selpos $ \(meth, note) ->
                ((Just meth, EditUtil.modify_text_key key note), False)
        _ -> Cmd.abort
    return Cmd.Done

-- | Take input to a pitch relative to middle C.  This is kinda random, so I'm
-- not sure if it'll be useful.
key_to_relative :: Pitch.InputKey -> (Pitch.Octave, Double)
key_to_relative (Pitch.InputKey key) = (oct, fromIntegral nn + f)
    where
    (i, f) = properFraction key
    c = (\(Pitch.InputKey k) -> floor k) Pitch.middle_c
    (oct, nn) = (i - c) `quotRem` 12

cmd_method_edit :: Cmd.Cmd
cmd_method_edit msg = do
    EditUtil.fallthrough msg
    case msg of
        (EditUtil.method_key -> Just key) -> do
            sel_pos <- EditUtil.get_sel_pos
            method_edit_at sel_pos key
        _ -> Cmd.abort
    return Cmd.Done

val_edit_at :: (Monad m) => EditUtil.SelPos -> Pitch.Note -> Cmd.CmdT m ()
val_edit_at selpos note = modify_event_at selpos $ \(method, _) ->
    ((Just method, Just (Pitch.note_text note)), False)

method_edit_at :: (Monad m) => EditUtil.SelPos -> Key.Key -> Cmd.CmdT m ()
method_edit_at selpos key = modify_event_at selpos $ \(method, val) ->
    ((EditUtil.modify_text_key key method, Just val), False)

-- | Record the last note entered.  Should be called by 'with_note'.
cmd_record_note_status :: Pitch.ScaleId -> Cmd.Cmd
cmd_record_note_status scale_id msg = do
    case msg of
        Msg.InputNote (InputNote.NoteOn _ key _) -> do
            note <- EditUtil.parse_key scale_id key
            Cmd.set_status "note" (Just (Pitch.note_text note))
        _ -> return ()
    return Cmd.Continue

-- * implementation

modify_event_at :: (Monad m) => EditUtil.SelPos
    -> ((String, String) -> ((Maybe String, Maybe String), Bool))
    -> Cmd.CmdT m ()
modify_event_at selpos f = EditUtil.modify_event_at selpos True
    (Arrow.first unparse . f . parse)

-- | Modify event text.  This is not used within this module but is exported
-- for others as a more general variant of 'modify_event_at'.
modify :: ((String, String) -> (String, String)) -> Event.Event -> Event.Event
modify f event = Event.set_string text event
    where
    text = maybe "" id (process (Event.event_string event))
    process = unparse . justify . f . parse
    justify (a, b) = (Just a, Just b)

parse :: String -> (String, String)
parse = ControlTrack.parse

unparse :: (Maybe String, Maybe String) -> Maybe String
unparse = ControlTrack.unparse
