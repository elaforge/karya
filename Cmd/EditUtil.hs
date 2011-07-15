{-# LANGUAGE ViewPatterns #-}
-- | Utilities for editing events.
module Cmd.EditUtil where
import Control.Monad
import qualified Data.Char as Char
import qualified Data.Map as Map

import qualified Util.Seq as Seq
import Ui
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Key as Key
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.Types as Types

import qualified Cmd.Cmd as Cmd
import qualified Cmd.InputNote as InputNote
import qualified Cmd.Msg as Msg
import qualified Cmd.Perf as Perf
import qualified Cmd.Selection as Selection
import qualified Cmd.TimeStep as TimeStep

import qualified Derive.Scale as Scale
import qualified Derive.Score as Score
import qualified Perform.Pitch as Pitch


-- * raw edit

raw_edit :: Bool -> Cmd.Cmd
raw_edit zero_dur msg = do
    fallthrough msg
    case msg of
        Msg.InputNote (InputNote.NoteOn _ key _) -> do
            note <- parse_key key
            modify_event zero_dur False $ \txt ->
                (modify_text_note note txt, False)
        (raw_key -> Just key) ->
            modify_event zero_dur False $ \txt ->
                (modify_text_key key txt, False)
        _ -> Cmd.abort
    return Cmd.Done

-- * events

-- | Get the event under insertion point, creating an empty one if there is
-- none.
get_event :: (State.M m) =>
    Bool -> TrackId -> ScoreTime -> ScoreTime -> m Event.Event
get_event modify_dur track_id pos dur = do
    track <- State.get_track track_id
    let modify = if modify_dur then Event.set_duration dur else id
    return $ maybe (Event.event "" dur) modify
        (Events.at pos (Track.track_events track))

modify_event :: (Cmd.M m) =>
    Bool -> Bool -> (String -> (Maybe String, Bool)) -> m ()
modify_event zero_dur modify_dur f = do
    sel <- get_sel_pos
    modify_event_at sel zero_dur modify_dur f

modify_event_at :: (Cmd.M m) => SelPos
    -> Bool -- ^ Created event has 0 dur, otherwise until next time step.
    -> Bool -- ^ If True, modify the duration of an existing event.
    -> (String -> (Maybe String, Bool))
    -- ^ return transformed event text or Nothing to delete the event, and
    -- whether or not to advance the selection after modification
    -> m ()
modify_event_at (tracknum, track_id, pos) zero_dur modify_dur f = do
    direction <- Cmd.gets (Cmd.state_note_direction . Cmd.state_edit)
    dur <- if zero_dur
        then return $ if direction == TimeStep.Advance then 0 else -0
        else do
            step <- Cmd.gets (Cmd.state_note_duration . Cmd.state_edit)
            end <- Selection.step_from tracknum pos direction step
            return (end - pos)
    event <- get_event modify_dur track_id pos dur
    -- TODO I could have the modifier take Text, if it were worth it.
    let (val, advance) = f (Event.event_string event)
    case val of
        Nothing -> State.remove_event track_id pos
        Just new_text -> State.insert_events track_id
            [(pos, Event.set_string new_text event)]
    when advance Selection.advance

type SelPos = (TrackNum, TrackId, ScoreTime)

get_sel_pos :: (Cmd.M m) => m SelPos
get_sel_pos = do
    (_, tracknum, track_id, pos) <- Selection.get_insert
    return (tracknum, track_id, pos)

lookup_instrument :: (Cmd.M m) => m (Maybe Score.Instrument)
lookup_instrument = do
    (block_id, _, track_id, _) <- Selection.get_insert
    Perf.lookup_instrument block_id track_id

get_scale_id :: (Cmd.M m) => m Pitch.ScaleId
get_scale_id = do
    (block_id, _, track_id, _) <- Selection.get_insert
    Perf.get_scale_id block_id track_id

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
        Key.Char c | f c -> True
        _ -> False
extract_key _ _ = Nothing


-- | Let keys that have a modifier down fall through.
--
-- When edit mode is on, the edit cmds tend to catch all msgs.  However, some
-- msgs should go through anyway.
fallthrough :: (Cmd.M m) => Msg.Msg -> m ()
fallthrough msg = do
    keys_down <- fmap Map.keys Cmd.keys_down
    -- Abort if there are modifiers down, so commands still work.
    -- Except shift, of course.  Oh, and midi, otherwise a note off would
    -- always fall through.
    let is_mod mod = case mod of
            Cmd.KeyMod m -> case m of
                Key.Shift -> False
                _ -> True
            Cmd.MidiMod _ _ -> False
            _ -> True
    when (any is_mod keys_down) Cmd.abort

    -- When clearing a range, let the global Edit.cmd_clear_selected handle it.
    let is_backspace = Msg.key_down msg == Just Key.Backspace
    (_, sel) <- Selection.get
    when (is_backspace && not (Types.sel_is_point sel)) Cmd.abort

parse_key :: (Cmd.M m) => Pitch.InputKey -> m Pitch.Note
parse_key input = do
    scale_id <- get_scale_id
    let me = "EditUtil.parse_key"
    scale <- Cmd.get_scale me scale_id
    let msg = me ++ ": " ++ show input ++ " out of range for " ++ show scale_id
    maybe (Cmd.throw msg) return (Scale.scale_input_to_note scale input)


-- * modify

-- | Since there's no use for leading spaces, just a space makes an empty
-- event.  Backspacing an empty event deletes it.
modify_text_key :: Key.Key -> String -> Maybe String
modify_text_key key s = case key of
    Key.Backspace -> backspace s
    Key.Char ' ' | null s -> Just ""
    Key.Char c | Char.isPrint c -> Just (s ++ [c])
    _ -> Just s

backspace :: String -> Maybe String
backspace s
    | null s = Nothing
    | otherwise = Just (Seq.rdrop 1 s)

modify_text_note :: Pitch.Note -> String -> Maybe String
modify_text_note note s = Just $
    s ++ leading ++ "(" ++ Pitch.note_text note ++ ")"
    where leading = if null s then "" else " "
