{-# LANGUAGE ViewPatterns #-}
-- | Utilities for editing events.
module Cmd.EditUtil where
import Control.Monad
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Util.Seq as Seq

import qualified Ui.Event as Event
import qualified Ui.Id as Id
import qualified Ui.Key as Key
import qualified Ui.State as State
import qualified Ui.Track as Track
import Ui.Types

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.Selection as Selection
import qualified Cmd.TimeStep as TimeStep

import qualified Derive.Scale as Scale

import qualified Perform.Pitch as Pitch

-- * events

-- | Get the event under insertion point, creating an empty one if there is
-- none.
get_event :: (State.UiStateMonad m) =>
    Track.TrackId -> TrackPos -> TrackPos -> m Event.Event
get_event track_id pos dur = do
    track <- State.get_track track_id
    return $ Maybe.fromMaybe (Event.event "" dur)
        (Track.event_at (Track.track_events track) pos)

modify_event :: (Monad m) => (String -> Maybe String)
    -> Cmd.CmdT m ()
modify_event f = do
    at <- Selection.get_insert_track
    modify_event_at at False f

modify_event_at :: (Monad m) => Selection.TrackSel
    -> Bool -- ^ Created event has 0 dur, otherwise until next time step.
    -> (String -> Maybe String) -> Cmd.CmdT m ()
modify_event_at (track_id, tracknum, pos) zero_dur f = do
    end_pos <- if zero_dur
        then return pos
        else Selection.step_from tracknum pos TimeStep.Advance
    event <- get_event track_id pos (end_pos - pos)
    case f (Event.event_text event) of
        Nothing -> State.remove_event track_id pos
        Just new_text -> State.insert_events track_id
            [(pos, event { Event.event_text = new_text })]

-- * msgs

data EditKey = Key Key.Key | Note Pitch.Note deriving (Show)

-- | Get a keystroke from Msg, aborting if it's not appropriate.  Raw edit mode
-- snags all printable keys.  Also abort if there are modifiers, so commands
-- still work.
edit_key :: (Monad m) => Pitch.ScaleId -> Msg.Msg -> Cmd.CmdT m EditKey
edit_key scale_id msg = do
    key <- Cmd.require =<< either State.throw return (edit_key_of scale_id msg)
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
    when (not (is_printable key) || any is_mod keys_down) Cmd.abort
    return key

-- | Like 'edit_key' except only accept identifier characters, for editing
-- methods and the like.  This way special keys like zoom and play still work.
alpha_key :: (Monad m) => Msg.Msg -> Cmd.CmdT m Key.Key
alpha_key msg = do
    key <- Cmd.require $ get_key msg
    case key of
        Key.KeyChar c | Id.is_identifier c -> return ()
        Key.Backspace -> return ()
        _ -> Cmd.abort
    return key

-- | Like 'edit_key' except only accept Notes, and backspace, which is returned
-- as Nothing.
note_key :: (Monad m) => Pitch.ScaleId -> Msg.Msg
    -> Cmd.CmdT m (Maybe Pitch.Note)
note_key scale_id msg =
    either State.throw return =<< Cmd.require (get_note scale_id msg)

get_note :: Pitch.ScaleId -> Msg.Msg -> Maybe (Either String (Maybe Pitch.Note))
get_note scale_id (Msg.KeyNumber keynum) = Just $ do
    scale <- maybe (Left $ "scale not found for " ++ show scale_id) Right $
        Map.lookup scale_id Scale.scale_map
    fmap Just $ Pitch.scale_key_to_note scale keynum
get_note _ (Msg.key -> Just Key.Backspace) = Just (Right Nothing)
get_note _ _ = Nothing

get_key :: Msg.Msg -> Maybe Key.Key
get_key (Msg.key -> Just key) = Just key
get_key _ = Nothing

edit_key_of :: Pitch.ScaleId -> Msg.Msg -> Either String (Maybe EditKey)
    -- No, these are not overlapped, ghc is wrong.
edit_key_of _ (Msg.key -> Just key) = Right (Just (Key key))
edit_key_of scale_id (get_note scale_id -> Just err_note) = case err_note of
    Left err -> Left err
    Right (Just note) -> Right (Just (Note note))
    -- Right Nothing means Backspace, which should have been caught above
    _ -> Right Nothing
edit_key_of _ _ = Right Nothing

is_printable :: EditKey -> Bool
is_printable (Note _) = True
is_printable (Key key) = case key of
    Key.Backspace -> True
    Key.KeyChar c -> Char.isPrint c
    _ -> False

-- * modify

-- | Since there's no use for leading spaces, just a space makes an empty
-- event.  Backspacing an empty event deletes it.
modify_text :: EditKey -> String -> Maybe String
modify_text (Note note) s =
    Just (s ++ leading ++ note_prefix : Pitch.note_text note)
    where leading = if null s then "" else " "
modify_text (Key key) s = modify_text_key key s

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

note_prefix :: Char
note_prefix = '*'
