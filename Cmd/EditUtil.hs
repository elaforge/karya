{-# LANGUAGE ViewPatterns #-}
-- | Utilities for editing events.
module Cmd.EditUtil where
import qualified Data.Char as Char
import qualified Data.Map as Map

import Util.Control
import qualified Util.Seq as Seq
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
import Types


-- * raw edit

raw_edit :: Bool -> Cmd.Cmd
raw_edit zero_dur msg = do
    fallthrough msg
    case msg of
        Msg.InputNote (InputNote.NoteOn _ key _) -> do
            note <- parse_key key
            modify_event zero_dur False $ \txt ->
                (modify_text_note note (fromMaybe "" txt), False)
        (raw_key -> Just key) ->
            modify_event zero_dur False $ \txt ->
                (modify_text_key key (fromMaybe "" txt), False)
        _ -> Cmd.abort
    return Cmd.Done

-- * events

-- | Get the event under insertion point, creating an empty one if there is
-- none.
get_event :: (State.M m) =>
    Bool -> TrackId -> ScoreTime -> ScoreTime -> m (Event.Event, Bool)
get_event modify_dur track_id pos dur = do
    track <- State.get_track track_id
    let modify = if modify_dur then Event.set_duration dur else id
    return $ maybe ((Event.event "" dur), True) (\evt -> (modify evt, False))
        (Events.at pos (Track.track_events track))

-- | Modify event text.
type Modify = Maybe String
    -- ^ Existing text, Nothing if the event will be created.
    -> (Maybe String, Bool)
    -- ^ Nothing deletes the event, True to advance cursor

modify_event :: (Cmd.M m) => Bool -> Bool -> Modify -> m ()
modify_event zero_dur modify_dur f = do
    pos <- Selection.get_insert_pos
    modify_event_at pos zero_dur modify_dur f

modify_event_at :: (Cmd.M m) => State.Pos
    -> Bool -- ^ Created event has 0 dur, otherwise until next time step.
    -> Bool -- ^ If True, modify the duration of an existing event.
    -> Modify -> m ()
modify_event_at (State.Pos block_id tracknum pos) zero_dur modify_dur f = do
    direction <- Cmd.gets (Cmd.state_note_direction . Cmd.state_edit)
    dur <- if zero_dur
        then return $ if direction == TimeStep.Advance then 0 else -0
        else do
            step <- Cmd.gets (Cmd.state_note_duration . Cmd.state_edit)
            end <- Selection.step_from tracknum pos direction step
            return (end - pos)
    track_id <- State.get_event_track_at "modify_event_at" block_id tracknum
    (event, created) <- get_event modify_dur track_id pos dur
    -- TODO I could have the modifier take Text, if it were worth it.
    let (val, advance) = f $
            if created then Nothing else Just (Event.event_string event)
    case val of
        Nothing -> State.remove_event track_id pos
        Just new_text -> State.insert_events track_id
            [(pos, Event.set_string new_text event)]
    when advance Selection.advance

remove_event :: (Cmd.M m) => Bool -> m ()
remove_event advance = do
    pos <- Selection.get_insert_pos
    remove_event_at pos advance

-- | Special case of 'modify_event_at' to only remove events.
remove_event_at :: (Cmd.M m) => State.Pos -> Bool -> m ()
remove_event_at pos advance =
    modify_event_at pos False False (const (Nothing, advance))

-- | Insert an event, but only if there isn't already a non-empty one there.
soft_insert :: (Cmd.M m) => String -> m ()
soft_insert text = modify_event True True $ \old_text ->
    if null (fromMaybe "" old_text) then (Just text, True)
        else (old_text, False)

lookup_instrument :: (Cmd.M m) => m (Maybe Score.Instrument)
lookup_instrument = do
    (block_id, _, track_id, _) <- Selection.get_insert
    Perf.lookup_instrument block_id (Just track_id)

get_scale_id :: (Cmd.M m) => m Pitch.ScaleId
get_scale_id = do
    (block_id, _, track_id, _) <- Selection.get_insert
    Perf.get_scale_id block_id (Just track_id)

get_key :: (Cmd.M m) => m (Maybe Pitch.Key)
get_key = do
    (block_id, _, track_id, _) <- Selection.get_insert
    Perf.get_key block_id (Just track_id)

-- * msgs

-- | Extract a key for method input.  [a-z0-9._-]
method_key :: Msg.Msg -> Maybe Key.Key
method_key = extract_key (not . Char.isSpace)

-- | Extract a key for control value input.  [0-9._-]
num_key :: Msg.Msg -> Maybe Key.Key
num_key = extract_key $ \c -> Char.isDigit c || c `elem` "_.-"

-- | Is the key appropriate for editing decimal numbers?
is_num_key Key.Backspace = True
is_num_key (Key.Char c) = Char.isDigit c || c `elem` "_.-"
is_num_key _ = False

alphanum_key :: Msg.Msg -> Maybe Key.Key
alphanum_key = extract_key $ \c -> Char.isAlphaNum c || c `elem` "_.-"

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

-- | Convert an InputKey to the symbolic Note that it should be.
--
-- Due to enharmonics this can depend on the current key and even be
-- ambiguous.
parse_key :: (Cmd.M m) => Pitch.InputKey -> m Pitch.Note
parse_key input = do
    scale_id <- get_scale_id
    let me = "EditUtil.parse_key"
    scale <- Cmd.get_scale me scale_id
    key <- get_key
    let msg = me ++ ": " ++ show input ++ " out of range for " ++ show scale_id
    maybe (Cmd.throw msg) return (Scale.scale_input_to_note scale key input)


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
