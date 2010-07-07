{-# LANGUAGE ViewPatterns #-}
{- | Cmds to edit a pitch track, which is a special kind of control track.

    This module creates the pitches that are later parsed by Derive.Control.
-}
module Cmd.PitchTrack where
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Util.Control
import qualified Util.Seq as Seq

import qualified Ui.Event as Event
import qualified Ui.Key as Key
import qualified Cmd.Cmd as Cmd
import qualified Cmd.EditUtil as EditUtil
import qualified Cmd.InputNote as InputNote
import qualified Cmd.Msg as Msg
import qualified Cmd.Selection as Selection

import qualified Perform.Pitch as Pitch


cmd_raw_edit :: Pitch.ScaleId -> Cmd.Cmd
cmd_raw_edit = EditUtil.raw_edit True

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
            EditUtil.modify_event False True (const (Nothing, True))
        _ -> Cmd.abort
    return Cmd.Done

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
modify_event_at selpos f = EditUtil.modify_event_at selpos True True
    (first unparse . f . parse)

-- | Modify event text.  This is not used within this module but is exported
-- for others as a more general variant of 'modify_event_at'.
modify :: ((String, String) -> (String, String)) -> Event.Event -> Event.Event
modify f event = Event.set_string text event
    where
    text = maybe "" id (process (Event.event_string event))
    process = unparse . justify . f . parse
    justify (a, b) = (Just a, Just b)

-- | Try to figure out the call part of the expression and split it from the
-- rest.
--
-- Like 'Derive.ControlTrack.parse', this is merely a heuristic.  It tries to
-- get the simple case right, but may be fooled by complex expressions.
parse :: String -> (String, String)
parse s
    | '(' `notElem` s =
        if " " `List.isSuffixOf` s then (pre, "") else ("", s)
    | otherwise = (pre, drop 1 post)
    where (pre, post) = break (==' ') s

unparse :: (Maybe String, Maybe String) -> Maybe String
unparse (method, val) = case (pre, post) of
        ("", "") -> Nothing
        -- If the method is gone, the note no longer needs its *, due to
        -- 'Derive.Control.mangle_pitch_call'.
        ("", '(':rest) -> Just $ Seq.rdrop 1 rest
        ("", _:_) -> Just post
        (_:_, "") -> Just $ pre ++ " "
        (_:_, '(':_) -> Just $ pre ++ ' ' : post
        -- And add parens if the method is new.
        (_:_, _:_) -> Just $ pre ++ ' ' : '(' : post ++ ")"
    where
    pre = Maybe.fromMaybe "" method
    post = Maybe.fromMaybe "" val
