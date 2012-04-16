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
import qualified Ui.Events as Events
import qualified Ui.Key as Key
import qualified Ui.State as State
import qualified Ui.Track as Track

import qualified Cmd.Cmd as Cmd
import qualified Cmd.EditUtil as EditUtil
import qualified Cmd.InputNote as InputNote
import qualified Cmd.ModifyEvents as ModifyEvents
import qualified Cmd.Msg as Msg
import qualified Cmd.Perf as Perf
import qualified Cmd.Selection as Selection

import qualified Derive.Derive as Derive
import qualified Derive.TrackInfo as TrackInfo
import qualified Perform.Pitch as Pitch
import qualified App.Config as Config
import Types


-- * entry

cmd_raw_edit :: Cmd.Cmd
cmd_raw_edit = Cmd.name "pitch track raw edit" . EditUtil.raw_edit True

cmd_val_edit :: Cmd.Cmd
cmd_val_edit msg = Cmd.name "pitch track val edit" $ do
    EditUtil.fallthrough msg
    case msg of
        Msg.InputNote (InputNote.NoteOn _ key _) -> do
            pos <- Selection.get_insert_pos
            note <- EditUtil.parse_key key
            val_edit_at pos note
            whenM (Cmd.gets (Cmd.state_advance . Cmd.state_edit))
                Selection.advance
        Msg.InputNote (InputNote.PitchChange _ key) -> do
            pos <- Selection.get_insert_pos
            note <- EditUtil.parse_key key
            val_edit_at pos note
        (Msg.key_down -> Just Key.Backspace) ->
            EditUtil.modify_event False True (const (Nothing, True))
        _ -> Cmd.abort
    return Cmd.Done

cmd_method_edit :: Cmd.Cmd
cmd_method_edit msg = Cmd.name "pitch track method edit" $ do
    EditUtil.fallthrough msg
    case msg of
        (EditUtil.method_key -> Just key) -> do
            pos <- Selection.get_insert_pos
            method_edit_at pos key
        _ -> Cmd.abort
    return Cmd.Done

val_edit_at :: (Cmd.M m) => State.Pos -> Pitch.Note -> m ()
val_edit_at pos note = modify_event_at pos $ \(method, _) ->
    ((Just method, Just (Pitch.note_text note)), False)

method_edit_at :: (Cmd.M m) => State.Pos -> Key.Key -> m ()
method_edit_at pos key = modify_event_at pos $ \(method, val) ->
    ((EditUtil.modify_text_key key method, Just val), False)

-- | Record the last note entered.  Should be called by 'with_note'.
cmd_record_note_status :: Cmd.Cmd
cmd_record_note_status msg = do
    case msg of
        Msg.InputNote (InputNote.NoteOn _ key _) -> do
            note <- EditUtil.parse_key key
            Cmd.set_status Config.status_note (Just (Pitch.note_text note))
        _ -> return ()
    return Cmd.Continue

-- * implementation

modify_event_at :: (Cmd.M m) => State.Pos
    -> ((String, String) -> ((Maybe String, Maybe String), Bool))
    -> m ()
modify_event_at pos f = EditUtil.modify_event_at pos True True
    (first unparse . f . parse. Maybe.fromMaybe "")

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

-- | Put the parsed halves back together.  Return Nothing if the event should
-- be deleted.
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

-- | Try to figure out where the note part is in event text and modify that
-- with the given function.
modify_note :: (Pitch.Note -> Maybe Pitch.Note) -> String
    -> Maybe String
    -- ^ Nothing if the note transformer returned Nothing.  Since you are
    -- supposed to return a valid Note there is no provision to delete the
    -- event.
modify_note f text = do
    new <- splice <$> f note
    return $ maybe "" id $ unparse (Just call, Just new)
    where
    (call, val) = parse text
    (pre, post) = break (`elem` " )") val
    note = Pitch.Note $ if "(" `List.isPrefixOf` pre then drop 1 pre else pre
    splice (Pitch.Note note)
        | "(" `List.isPrefixOf` val = '(' : note ++ post
        | otherwise = note ++ post


-- * edits

-- | Naturally transposition is way more complicated that I thought.
--
-- The entire command will abort if there is a pitch that can't be transposed,
-- for safety.  But so I can select multiple pitch tracks or selected a merged
-- note track, I skip non-pitch tracks.
transpose_selection :: (Cmd.M m) => Pitch.Octave -> Pitch.Degree -> m ()
transpose_selection octaves degrees = do
    block_id <- Cmd.get_focused_block
    ModifyEvents.tracks_sorted $ \track_id events -> do
        is_pitch <- is_pitch_track track_id
        if not is_pitch then return Nothing else do
        scale_id <- Perf.get_scale_id block_id track_id
        Just <$> transpose_events block_id track_id scale_id
            octaves degrees events

transpose_events :: (Cmd.M m) => BlockId -> TrackId -> Pitch.ScaleId
    -> Pitch.Octave -> Pitch.Degree -> [Events.PosEvent] -> m [Events.PosEvent]
transpose_events block_id track_id scale_id octaves degrees events = do
    scale <- Cmd.get_scale "transpose_selection" scale_id
    let transposed = map (transpose scale octaves degrees) events
        failed = [event | (event, Nothing) <- zip events transposed]
    unless (null failed) $
        Cmd.throw $ "transpose failed on events at: "
            ++ Seq.join ", " (map (Cmd.log_event block_id track_id) failed)
    return $ Maybe.catMaybes transposed

transpose :: Derive.Scale -> Pitch.Octave -> Pitch.Degree -> Events.PosEvent
    -> Maybe Events.PosEvent
transpose scale octaves degrees (pos, event) =
    case modify_note f (Event.event_string event) of
        Nothing -> Nothing
        Just text -> Just (pos, Event.set_string text event)
    where f = Derive.scale_transpose scale octaves degrees

is_pitch_track :: (State.M m) => TrackId -> m Bool
is_pitch_track track_id = do
    title <- Track.track_title <$> State.get_track track_id
    return $ case TrackInfo.parse_control title of
        Right (TrackInfo.Pitch {}) -> True
        _ -> False
