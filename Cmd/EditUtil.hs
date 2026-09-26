-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
-- | Utilities for editing events.
module Cmd.EditUtil (
    Pos(..), getPos
    -- * events
    , Modify
    , modifyEventAt, modifyEventAtTrigger
    , softInsert
    , lookupInstrument
    -- * msgs
    , Key(..)
    , methodKey
    , numKey, isNumKey
    , hexKey
    , Fallthrough(..)
    , fallthrough
    , inputToNote, inputsToNotes
    -- * modify
    , modifyTextKey
#ifdef TESTING
    , module Cmd.EditUtil
#endif
) where
import qualified Data.Char as Char
import qualified Data.Either as Either
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.Perf as Perf
import qualified Cmd.Selection as Selection

import qualified Derive.DeriveT as DeriveT
import qualified Derive.Scale as Scale
import qualified Derive.ScoreT as ScoreT

import qualified Instrument.Common as Common
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
-- This is like 'Selection.Context', except focused on a single track.
data Pos = Pos !BlockId !TrackNum !TrackTime !TrackTime
    deriving (Eq, Show)

instance Pretty Pos where pretty = showt

getPos :: Cmd.M m => m Pos
getPos = do
    (viewId, sel) <- Selection.getViewSel
    blockId <- Ui.blockIdOf viewId
    let (start, dur) = let (s, e) = Sel.range sel
            in case Sel.orientation sel of
                Sel.Negative
                    | s == e -> (e, -0)
                    | otherwise -> (e, s - e)
                _ -> (s, e - s)
    pure $ Pos blockId (Selection.selPointTrack sel) start dur

-- * events

-- | Get the event under insertion point, creating an empty one if there is
-- none.
getOrCreateEvent :: Ui.M m => Bool -> TrackId -> TrackTime -> TrackTime
    -> m (Event.Event, Bool)
getOrCreateEvent modifyDur trackId pos dur = do
    mbEvent <- Events.at pos (Event.orientationOf dur) <$>
        Ui.getEvents trackId
    let modify = if modifyDur then Event.duration_ #= dur else id
    pure $ maybe (Event.event pos dur "", True) (\evt -> (modify evt, False))
        mbEvent

-- | Modify event text.
type Modify = Maybe Text
    -- ^ Existing text, Nothing if the event will be created.
    -> (Maybe Text, Bool)
    -- ^ Nothing deletes the event, True to advance cursor

modifyEvent :: Cmd.M m => Bool -> Bool -> Modify -> m ()
modifyEvent zeroDur modifyDur modify = do
    pos <- getPos
    modifyEventAt pos zeroDur modifyDur modify

modifyEventAt :: Cmd.M m => Pos
    -> Bool -- ^ If the selection is 0, then True means create a 0 dur event,
    -- otherwise use the time step. If the selection is nonzero, always use
    -- its duration.
    -> Bool -- ^ If True, modify the duration of an existing event.
    -> Modify -> m ()
modifyEventAt (Pos blockId tracknum start dur) zeroDur modifyDur modify =do
    dur <- inferDuration dur
    trackId <- Ui.getEventTrackAt blockId tracknum
    (event, created) <- getOrCreateEvent modifyDur trackId start dur
    let (val, advance) = modify $
            if created then Nothing else Just (Event.text event)
    case val of
        Nothing -> Ui.removeEventsRange trackId
            (Events.Point start (Event.orientationOf dur))
        Just newText -> Ui.insertEvent trackId
            (Event.text_ #= newText $ event)
    when advance Selection.advance
    where
    inferDuration dur
        | dur /= 0 = pure dur
        | zeroDur = ifM ((==Types.Positive) <$> getOrientation)
            (pure 0) (pure (-0))
        | otherwise = getDuration tracknum start

-- | Like 'modify_event_at', but take 'triggered_inst' into account.
modifyEventAtTrigger :: Cmd.M m => Pos -> Bool -> Bool -> Modify -> m ()
modifyEventAtTrigger pos zeroDur modifyDur f = do
    triggerInst <- triggeredInst =<< lookupInstrument
    modifyEventAt pos (zeroDur || triggerInst) modifyDur f

-- | Instruments with the triggered flag set don't pay attention to note off,
-- so I can make the duration 0.
triggeredInst :: Cmd.M m => Maybe ScoreT.Instrument -> m Bool
triggeredInst Nothing = pure False -- don't know, but guess it's not
triggeredInst (Just inst) = Set.member Common.Triggered <$> commonFlags inst

commonFlags :: Cmd.M m => ScoreT.Instrument -> m (Set Common.Flag)
commonFlags inst = maybe mempty flags <$> Cmd.lookupInstrument inst
    where flags = Common.commonFlags . Cmd.instCommon

getDuration :: Cmd.M m => TrackNum -> TrackTime -> m TrackTime
getDuration tracknum start = do
    orient <- getOrientation
    step <- Cmd.gets (Cmd.stateNoteDuration . Cmd.stateEdit)
    end <- Selection.stepFrom tracknum start
        (if orient == Types.Positive then 1 else -1) step
    pure (end - start)

getOrientation :: Cmd.M m => m Types.Orientation
getOrientation = Cmd.gets $ Cmd.stateNoteOrientation . Cmd.stateEdit

-- | Insert an event, but only if there isn't already a non-empty one there.
softInsert :: Cmd.M m => Text -> m ()
softInsert text = modifyEvent True True $ \oldText ->
    if Text.null (fromMaybe "" oldText) then (Just text, True)
        else (oldText, False)

lookupInstrument :: Cmd.M m => m (Maybe ScoreT.Instrument)
lookupInstrument = Perf.lookupInstrument =<< Selection.track

-- * msgs

data Key = Backspace | Key Char
    deriving (Eq, Show)

-- | Extract a key for method input.  [a-z0-9._-]
methodKey :: Msg.Msg -> Maybe Key
methodKey = extractKey (not . Char.isSpace)

-- | Extract a key for control value input.  [0-9._-]
numKey :: Msg.Msg -> Maybe Key
numKey = extractKey $ \c -> Char.isDigit c || c `elem` ("_.-" :: [Char])

-- | Is the key appropriate for editing decimal numbers?
isNumKey :: Key -> Bool
isNumKey Backspace = True
isNumKey (Key c) = Char.isDigit c || c `elem` ("_.-" :: [Char])

-- | Is the key appropriate for editing control track hex numbers?
-- Also includes @-@ for negation.
hexKey :: Msg.Msg -> Maybe Key
hexKey = extractKey $ \c -> Char.isDigit c || c `elem` ("-abcdefg" :: [Char])

extractKey :: (Char -> Bool) -> Msg.Msg -> Maybe Key
extractKey f (Msg.text -> Just (key, text)) = case key of
    Key.Backspace -> Just Backspace
    _ -> case text of
        Just c | f c -> Just (Key c)
        _ -> Nothing
extractKey _ _ = Nothing

data Fallthrough = WantBackspace | NoBackspace

-- | Let keys that have a modifier down fall through.
--
-- When edit mode is on, the edit cmds tend to catch all msgs.  However, some
-- msgs should go through anyway.
fallthrough :: Cmd.M m => Fallthrough -> Msg.Msg -> m ()
fallthrough wantBackspace msg = do
    keysDown <- fmap Map.keys Cmd.keysDown
    when (any isMod keysDown) --  || Msg.key_down msg == Just Key.Backspace)
        Cmd.abort
    when (Msg.keyDown msg == Just Key.Backspace) $ case wantBackspace of
        -- When a range is selected, always let the global
        -- Edit.cmd_clear_selected handle it.
        WantBackspace -> whenM (not . Sel.isPoint <$> Selection.get) Cmd.abort
        NoBackspace -> Cmd.abort
    where
    -- Abort if there are modifiers down, so commands still work.
    -- Except shift, of course.  Oh, and midi, otherwise a note off would
    -- always fall through.
    isMod = \case
        Cmd.KeyMod m -> case m of
            Key.Shift -> False
            _ -> True
        Cmd.MidiMod _ _ -> False
        _ -> True

-- | Convert an InputKey to the symbolic Note that it should be.
--
-- Due to enharmonics this can depend on the current key and even be
-- ambiguous.
inputToNote :: Cmd.M m => Pitch.Input -> m Pitch.Note
inputToNote input = do
    track <- Selection.track
    scale <- Perf.getScale track
    env <- Perf.getEnviron track
    case Scale.scaleInputToNote scale env input of
        -- This just means the key isn't in the scale, it happens a lot so no
        -- need to shout about it.
        Left DeriveT.InvalidInput -> Cmd.abort
        Left err -> Cmd.throw $ "input_to_note " <> pretty input <> " for "
            <> pretty (Scale.scaleId scale) <> ": " <> pretty err
        Right note -> pure note

-- | Like 'input_to_note', but more efficient for multiple inputs.
inputsToNotes :: Cmd.M m => [(key, Pitch.Input)] -> m [(key, Pitch.Note)]
inputsToNotes keyInputs = do
    track <- Selection.track
    scale <- Perf.getScale track
    env <- Perf.getEnviron track
    pure $ Either.rights $
        map (traverse (Scale.scaleInputToNote scale env)) keyInputs

-- * modify

-- | Since there's no use for leading spaces, just a space makes an empty
-- event.  Backspacing the last character return Nothing, which deletes the
-- event.  If I want a "" event I can create one with space.
modifyTextKey :: [Key.Modifier] -> Key -> Text -> Maybe Text
modifyTextKey mods key s = case key of
    Backspace
        | Key.Shift `elem` mods -> backspaceExpr s
        | otherwise -> backspace s
    Key ' ' | Text.null s -> Just ""
    Key c
        -- It really shouldn't be non-printable, but check just in case.
        | Char.isPrint c -> Just (Text.snoc s c)
        | otherwise -> Just s

backspace :: Text -> Maybe Text
backspace s
    | Text.null s2 = Nothing
    | otherwise = Just s2
    where s2 = Text.take (Text.length s - 1) s

backspaceExpr :: Text -> Maybe Text
backspaceExpr s
    | Text.null s = Nothing
    | otherwise = Just $ dropExpr s

-- | Drop a parenthesized expression, or a `symbol` up to its matching
-- backtick.
dropExpr :: Text -> Text
dropExpr expr = Text.reverse $ txt rev
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
