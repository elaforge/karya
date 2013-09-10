-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ViewPatterns #-}
-- | Utilities for editing events.
module Cmd.EditUtil where
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Text as Text

import Util.Control
import qualified Util.Pretty as Pretty
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


-- | block tracknum start duration
data Pos = Pos !BlockId !TrackNum !TrackTime !TrackTime
    deriving (Eq, Show)

get_pos :: (Cmd.M m) => m Pos
get_pos = do
    (view_id, sel) <- Selection.get
    block_id <- State.block_id_of view_id
    let (start, end) = Types.sel_range sel
    return $ Pos block_id (Selection.point_track sel) (Selection.point sel)
        (end - start)

-- * raw edit

raw_edit :: Bool -> Cmd.Cmd
raw_edit zero_dur msg = do
    fallthrough msg
    case msg of
        Msg.InputNote (InputNote.NoteOn _ input _) -> do
            note <- input_to_note input
            modify_event zero_dur False $ \txt ->
                (modify_text_note note (fromMaybe "" txt), False)
        (raw_key -> Just (mods, key)) ->
            modify_event zero_dur False $ \txt ->
                (modify_text_key mods key (fromMaybe "" txt), False)
        _ -> Cmd.abort
    return Cmd.Done

-- * events

-- | Get the event under insertion point, creating an empty one if there is
-- none.
get_event :: (State.M m) =>
    Bool -> TrackId -> TrackTime -> TrackTime -> m (Event.Event, Bool)
get_event modify_dur track_id pos dur = do
    track <- State.get_track track_id
    let modify = if modify_dur then Event.set_duration dur else id
    return $ maybe (Event.event pos dur "", True)
        (\evt -> (modify evt, False))
        (Events.at pos (Track.track_events track))

-- | Modify event text.
type Modify = Maybe Text
    -- ^ Existing text, Nothing if the event will be created.
    -> (Maybe Text, Bool)
    -- ^ Nothing deletes the event, True to advance cursor

modify_event :: (Cmd.M m) => Bool -> Bool -> Modify -> m ()
modify_event zero_dur modify_dur f = do
    pos <- get_pos
    modify_event_at pos zero_dur modify_dur f

modify_event_at :: (Cmd.M m) => Pos
    -> Bool -- ^ Created event has 0 dur, otherwise it's the duration of the
    -- selection, or until next time step if the selection has 0 duration.
    -> Bool -- ^ If True, modify the duration of an existing event.
    -> Modify -> m ()
modify_event_at (Pos block_id tracknum start dur) zero_dur modify_dur f = do
    dir <- Cmd.gets (Cmd.state_note_direction . Cmd.state_edit)
    dur <- if zero_dur
        then return $ if dir == TimeStep.Advance then 0 else -0
        else if dur > 0 then return dur
        else do
            step <- Cmd.gets (Cmd.state_note_duration . Cmd.state_edit)
            end <- Selection.step_from tracknum start
                (TimeStep.direction dir) step
            return (end - start)
    track_id <- State.get_event_track_at block_id tracknum
    (event, created) <- get_event modify_dur track_id start dur
    -- TODO I could have the modifier take Text, if it were worth it.
    let (val, advance) = f $
            if created then Nothing else Just (Event.event_text event)
    case val of
        Nothing -> State.remove_event track_id start
        Just new_text -> State.insert_event track_id
            (Event.set_text new_text event)
    when advance Selection.advance

remove_event :: (Cmd.M m) => Bool -> m ()
remove_event advance = do
    pos <- get_pos
    remove_event_at pos advance

-- | Special case of 'modify_event_at' to only remove events.
remove_event_at :: (Cmd.M m) => Pos -> Bool -> m ()
remove_event_at pos advance =
    modify_event_at pos False False (const (Nothing, advance))

-- | Insert an event, but only if there isn't already a non-empty one there.
soft_insert :: (Cmd.M m) => Text -> m ()
soft_insert text = modify_event True True $ \old_text ->
    if Text.null (fromMaybe "" old_text) then (Just text, True)
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

data Key = Backspace | Key Char
    deriving (Eq, Show)

-- | Extract a key for method input.  [a-z0-9._-]
method_key :: Msg.Msg -> Maybe Key
method_key = extract_key (not . Char.isSpace)

-- | Extract a key for control value input.  [0-9._-]
num_key :: Msg.Msg -> Maybe Key
num_key = extract_key $ \c -> Char.isDigit c || c `elem` "_.-"

-- | Is the key appropriate for editing decimal numbers?
is_num_key :: Key -> Bool
is_num_key Backspace = True
is_num_key (Key c) = Char.isDigit c || c `elem` "_.-"

-- | Is the key appropriate for editing control track hex numbers?
hex_key :: Msg.Msg -> Maybe Key
hex_key = extract_key $ \c -> Char.isDigit c || c `elem` "abcdefg"

alphanum_key :: Msg.Msg -> Maybe Key
alphanum_key = extract_key $ \c -> Char.isAlphaNum c || c `elem` "_.-"

-- | Extract a key for raw input.
raw_key :: Msg.Msg -> Maybe ([Key.Modifier], Key)
raw_key msg = case extract_key Char.isPrint msg of
    Just key -> Just (fromMaybe [] (Msg.key_mods msg), key)
    Nothing -> Nothing

extract_key :: (Char -> Bool) -> Msg.Msg -> Maybe Key
extract_key f (Msg.text -> Just (key, text)) = case key of
    Key.Backspace -> Just Backspace
    _ -> case text of
        Just c | f c -> Just (Key c)
        _ -> Nothing
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
input_to_note :: (Cmd.M m) => Pitch.Input -> m Pitch.Note
input_to_note input = do
    scale_id <- get_scale_id
    let me = "EditUtil.input_to_note"
    scale <- Cmd.get_scale me scale_id
    key <- get_key
    let msg = me <> ": " <> Pretty.pretty input <> " out of range for "
            <> show scale_id
    maybe (Cmd.throw msg) return (Scale.scale_input_to_note scale key input)


-- * modify

-- | Since there's no use for leading spaces, just a space makes an empty
-- event.  Backspacing an empty event returns Nothing, which should delete the
-- event itself.
modify_text_key :: [Key.Modifier] -> Key -> Text -> Maybe Text
modify_text_key mods key s = case key of
    Backspace
        | Key.Shift `elem` mods -> backspace_expr s
        | otherwise -> backspace s
    Key ' ' | Text.null s -> Just ""
    Key c
        -- It really shouldn't be non-printable, but check just in case.
        | Char.isPrint c -> Just (Text.snoc s c)
        | otherwise -> Just s

backspace :: Text -> Maybe Text
backspace s
    | Text.null s = Nothing
    | otherwise = Just $ Text.take (Text.length s - 1) s

backspace_expr :: Text -> Maybe Text
backspace_expr s
    | Text.null s = Nothing
    | otherwise = Just $ drop_expr s

-- | Drop a parenthesized expression, or a `symbol` up to its matching
-- backtick.
drop_expr :: Text -> Text
drop_expr expr = Text.reverse $ txt rev
    where
    rev = case untxt $ Text.reverse $ Text.dropWhileEnd (==' ') expr of
        ')' : s -> dropWhile (==' ') (go 1 s)
        '`' : s -> case dropWhile (/='`') s of
            '`' : rest -> rest
            "" -> s
            _ -> s
        s -> drop 1 s
    go 0 s = s
    go nest s = case s of
        "" -> untxt $ Text.drop 1 expr -- No balanced paren, just drop 1.
        -- These are backwards because the string is reversed.
        ')' : s -> go (nest+1) s
        '(' : s -> go (nest-1) s
        _ : s -> go nest s

modify_text_note :: Pitch.Note -> Text -> Maybe Text
modify_text_note note s =
    Just $ s <> leading <> "(" <> Pitch.note_text note <> ")"
    where leading = if Text.null s then "" else " "
