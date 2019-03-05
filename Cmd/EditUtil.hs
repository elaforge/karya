-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ViewPatterns #-}
-- | Utilities for editing events.
module Cmd.EditUtil where
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.Perf as Perf
import qualified Cmd.Selection as Selection

import qualified Derive.DeriveT as DeriveT
import qualified Derive.Scale as Scale
import qualified Derive.ScoreT as ScoreT

import qualified Perform.Pitch as Pitch
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Key as Key
import qualified Ui.Sel as Sel
import qualified Ui.Types as Types
import qualified Ui.Ui as Ui

import           Global
import           Types


-- | block tracknum start duration
--
-- The duration from the selection, so if it's zero, then an event duration
-- will be inferred by 'get_duration' based on the 'Cmd.state_note_duration'.
data Pos = Pos !BlockId !TrackNum !TrackTime !TrackTime
    deriving (Eq, Show)

instance Pretty Pos where pretty = showt

get_pos :: Cmd.M m => m Pos
get_pos = do
    (view_id, sel) <- Selection.get_view
    block_id <- Ui.block_id_of view_id
    let (start, dur) = let (s, e) = Sel.range sel
            in case Sel.orientation sel of
                Sel.Negative
                    | s == e -> (e, -0)
                    | otherwise -> (e, s - e)
                _ -> (s, e - s)
    return $ Pos block_id (Selection.sel_point_track sel) start dur

-- * events

-- | Get the event under insertion point, creating an empty one if there is
-- none.
get_or_create_event :: Ui.M m => Bool -> TrackId -> TrackTime -> TrackTime
    -> m (Event.Event, Bool)
get_or_create_event modify_dur track_id pos dur = do
    event <- Events.at pos (Event.orientation_of dur) <$> Ui.get_events track_id
    let modify = if modify_dur then Event.duration_ #= dur else id
    return $ maybe (Event.event pos dur "", True) (\evt -> (modify evt, False))
        event

-- | Modify event text.
type Modify = Maybe Text
    -- ^ Existing text, Nothing if the event will be created.
    -> (Maybe Text, Bool)
    -- ^ Nothing deletes the event, True to advance cursor

modify_event :: Cmd.M m => Bool -> Bool -> Modify -> m ()
modify_event zero_dur modify_dur modify = do
    pos <- get_pos
    modify_event_at pos zero_dur modify_dur modify

modify_event_at :: Cmd.M m => Pos
    -> Bool -- ^ If the selection is 0, then True means create a 0 dur event,
    -- otherwise use the time step. f the selection is nonzero, always use
    -- its duration.
    -> Bool -- ^ If True, modify the duration of an existing event.
    -> Modify -> m ()
modify_event_at (Pos block_id tracknum start dur) zero_dur modify_dur modify =do
    dur <- infer_duration dur
    track_id <- Ui.get_event_track_at block_id tracknum
    (event, created) <- get_or_create_event modify_dur track_id start dur
    let (val, advance) = modify $
            if created then Nothing else Just (Event.text event)
    case val of
        Nothing -> Ui.remove_events_range track_id
            (Events.Point start (Event.orientation_of dur))
        Just new_text -> Ui.insert_event track_id
            (Event.text_ #= new_text $ event)
    when advance Selection.advance
    where
    infer_duration dur
        | dur /= 0 = return dur
        | zero_dur = ifM ((==Types.Positive) <$> get_orientation)
            (return 0) (return (-0))
        | otherwise = get_duration tracknum start

get_duration :: Cmd.M m => TrackNum -> TrackTime -> m TrackTime
get_duration tracknum start = do
    orient <- get_orientation
    step <- Cmd.gets (Cmd.state_note_duration . Cmd.state_edit)
    end <- Selection.step_from tracknum start
        (if orient == Types.Positive then 1 else -1) step
    return (end - start)

get_orientation :: Cmd.M m => m Types.Orientation
get_orientation = Cmd.gets $ Cmd.state_note_orientation . Cmd.state_edit

remove_event :: Cmd.M m => Bool -> m ()
remove_event advance = do
    pos <- get_pos
    remove_event_at pos advance

-- | Special case of 'modify_event_at' to only remove events.
remove_event_at :: Cmd.M m => Pos -> Bool -> m ()
remove_event_at pos advance =
    modify_event_at pos False False (const (Nothing, advance))

-- | Insert an event, but only if there isn't already a non-empty one there.
soft_insert :: Cmd.M m => Text -> m ()
soft_insert text = modify_event True True $ \old_text ->
    if Text.null (fromMaybe "" old_text) then (Just text, True)
        else (old_text, False)

lookup_instrument :: Cmd.M m => m (Maybe ScoreT.Instrument)
lookup_instrument = Perf.lookup_instrument =<< Selection.track

-- * msgs

data Key = Backspace | Key Char
    deriving (Eq, Show)

-- | Extract a key for method input.  [a-z0-9._-]
method_key :: Msg.Msg -> Maybe Key
method_key = extract_key (not . Char.isSpace)

-- | Extract a key for control value input.  [0-9._-]
num_key :: Msg.Msg -> Maybe Key
num_key = extract_key $ \c -> Char.isDigit c || c `elem` ("_.-" :: [Char])

-- | Is the key appropriate for editing decimal numbers?
is_num_key :: Key -> Bool
is_num_key Backspace = True
is_num_key (Key c) = Char.isDigit c || c `elem` ("_.-" :: [Char])

-- | Is the key appropriate for editing control track hex numbers?
-- Also includes @-@ for negation.
hex_key :: Msg.Msg -> Maybe Key
hex_key = extract_key $ \c -> Char.isDigit c || c `elem` ("-abcdefg" :: [Char])

alphanum_key :: Msg.Msg -> Maybe Key
alphanum_key = extract_key $ \c ->
    Char.isAlphaNum c || c `elem` ("_.-" :: [Char])

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
fallthrough :: Cmd.M m => Msg.Msg -> m ()
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
    sel <- Selection.get
    when (is_backspace && not (Sel.is_point sel)) Cmd.abort

-- | Convert an InputKey to the symbolic Note that it should be.
--
-- Due to enharmonics this can depend on the current key and even be
-- ambiguous.
input_to_note :: Cmd.M m => Pitch.Input -> m Pitch.Note
input_to_note input = do
    track <- Selection.track
    scale <- Perf.get_scale track
    env <- Perf.get_environ track
    case Scale.scale_input_to_note scale env input of
        -- This just means the key isn't in the scale, it happens a lot so no
        -- need to shout about it.
        Left DeriveT.InvalidInput -> Cmd.abort
        Left err -> Cmd.throw $ "input_to_note " <> pretty input <> " for "
            <> pretty (Scale.scale_id scale) <> ": " <> pretty err
        Right note -> return note

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
