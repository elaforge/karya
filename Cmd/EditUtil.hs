{-# LANGUAGE ViewPatterns #-}
-- | Utilities for editing events.
module Cmd.EditUtil where
import Control.Monad
import qualified Data.Char as Char
import qualified Data.Map as Map

import qualified Util.Seq as Seq

import Ui
import qualified Ui.Event as Event
import qualified Ui.Key as Key
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.Types as Types

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.Selection as Selection

import qualified Perform.Pitch as Pitch


-- * events

-- | Get the event under insertion point, creating an empty one if there is
-- none.
get_event :: (State.UiStateMonad m) =>
    Bool -> TrackId -> TrackPos -> TrackPos -> m Event.Event
get_event modify_dur track_id pos dur = do
    track <- State.get_track track_id
    let modify = if modify_dur then Event.set_duration dur else id
    return $ maybe (Event.event "" dur) modify
        (Track.event_at pos (Track.track_events track))

modify_event :: (Monad m) =>
    Bool -> Bool -> (String -> (Maybe String, Bool)) -> Cmd.CmdT m ()
modify_event zero_dur modify_dur f = do
    sel <- get_sel_pos
    modify_event_at sel zero_dur modify_dur f

modify_event_at :: (Monad m) => SelPos
    -> Bool -- ^ Created event has 0 dur, otherwise until next time step.
    -> Bool -- ^ If True, modify the duration of an existing event.
    -> (String -> (Maybe String, Bool)) -> Cmd.CmdT m ()
modify_event_at (tracknum, track_id, pos) zero_dur modify_dur f = do
    direction <- Cmd.gets Cmd.state_note_direction
    end_pos <- if zero_dur then return pos
        else Selection.step_from tracknum pos direction
    event <- get_event modify_dur track_id pos (end_pos - pos)
    -- TODO I could have the modifier take Text, if it were worth it.
    let (val, advance) = f (Event.event_string event)
    case val of
        Nothing -> State.remove_event track_id pos
        Just new_text -> State.insert_events track_id
            [(pos, Event.set_string new_text event)]
    when advance Selection.advance

type SelPos = (TrackNum, TrackId, TrackPos)

get_sel_pos :: (Monad m) => Cmd.CmdT m SelPos
get_sel_pos = do
    (_, tracknum, track_id, pos) <- Selection.get_insert
    return (tracknum, track_id, pos)

-- * msgs

-- | Extract a key for method input.  [a-z0-9.-]
method_key :: Msg.Msg -> Maybe Key.Key
method_key = extract_key $ \c -> Char.isAlphaNum c || c `elem` "-_."

-- | Extract a key for control value input.  [0-9.-]
val_key :: Msg.Msg -> Maybe Key.Key
val_key = extract_key $ \c -> Char.isDigit c || c `elem` "-_."


-- | Extract a key for raw input.  Any printable character plus backspace.
raw_key :: Msg.Msg -> Maybe Key.Key
raw_key = extract_key Char.isPrint

extract_key :: (Char -> Bool) -> Msg.Msg -> Maybe Key.Key
extract_key f (Msg.key_down -> Just key) = if ok then Just key else Nothing
    where
    ok = case key of
        Key.Backspace -> True
        Key.KeyChar c | f c -> True
        _ -> False
extract_key _ _ = Nothing


-- | When edit mode is on, the edit cmds tend to catch all msgs.  However, some
-- msgs should go through anyway.
fallthrough :: (Monad m) => Msg.Msg -> Cmd.CmdT m ()
fallthrough msg = do
    keys_down <- fmap Map.keys Cmd.keys_down
    -- Abort if there are modifiers down, so commands still work.
    -- Except shift, of course.
    let is_mod mod = case mod of
            Cmd.KeyMod k -> case k of
                Key.KeyChar _ -> False
                Key.ShiftL -> False
                Key.ShiftR -> False
                _ -> True
            _ -> True
    when (any is_mod keys_down) Cmd.abort

    -- When clearing a range, let the global Edit.cmd_clear_selected handle it.
    let is_backspace = Msg.key_down msg == Just Key.Backspace
    (_, sel) <- Selection.get
    when (is_backspace && not (Types.sel_is_point sel)) Cmd.abort

parse_key :: (Monad m) => Pitch.ScaleId -> Pitch.InputKey
    -> Cmd.CmdT m Pitch.Note
parse_key scale_id input = do
    let me = "EditUtil.parse_key"
    scale <- Cmd.get_scale me scale_id
    let msg = me ++ ": " ++ show input ++ " out of range for " ++ show scale_id
    maybe (Cmd.throw msg) return (Pitch.scale_input_to_note scale input)


-- * modify

-- | Since there's no use for leading spaces, just a space makes an empty
-- event.  Backspacing an empty event deletes it.
modify_text_key :: Key.Key -> String -> Maybe String
modify_text_key key s = case key of
    Key.Backspace -> backspace s
    Key.KeyChar ' ' | null s -> Just ""
    Key.KeyChar c | Char.isPrint c -> Just (s ++ [c])
    _ -> Just s

backspace :: String -> Maybe String
backspace s
    | null s = Nothing
    | is_note = Just rest
    | otherwise = Just (Seq.rdrop 1 s)
    where
    (pre, post) = break (==note_prefix) (reverse s)
    -- If post isn't null, then it must begin with note_prefix.
    is_note = not $ any Char.isSpace pre || null post
    -- drop note_prefix and leading (i.e. trailing) spaces
    rest = reverse $ dropWhile Char.isSpace (drop 1 post)

modify_text_note :: Pitch.Note -> String -> Maybe String
modify_text_note note s = Just $
    s ++ leading ++ note_prefix : Pitch.note_text note
    where leading = if null s then "" else " "

note_prefix :: Char
note_prefix = '*'
