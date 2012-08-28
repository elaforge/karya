module Cmd.Msg where
import Control.Monad
import qualified Data.Map as Map
import qualified System.IO as IO

import qualified Util.Pretty as Pretty
import qualified Midi.Midi as Midi
import qualified Ui.Key as Key
import qualified Ui.Track as Track
import qualified Ui.UiMsg as UiMsg

import qualified Cmd.InputNote as InputNote
import qualified Derive.Derive as Derive
import qualified Derive.Stack as Stack
import qualified Derive.TrackWarp as TrackWarp

import qualified Perform.Transport as Transport
import Types


data Msg =
    -- | Message from the UI.
    Ui UiMsg.UiMsg
    -- | Incoming midi message.
    | Midi Midi.ReadMessage
    -- | Incoming abstract note.  This is meant to enter a note and is not
    -- a MIDI NoteOn, though NoteOns may very well be translated into these.
    -- These are not generated from any subsystem, but instead translated from
    -- other msgs (such as Midi).
    | InputNote InputNote.Input
    -- | Message from the transport/play thread.
    | Transport Transport.Status
    -- | Message about the derivation status, from the background derivation
    -- threads.
    | DeriveStatus BlockId DeriveStatus
    -- | Message from the language control socket, includes the socket handle
    -- than can be used to write a response.  Whoever responds to Socket should
    -- close the handle.
    | Socket IO.Handle String
    deriving (Show)

instance Pretty.Pretty Msg where
    pretty (Ui msg) = "Ui: " ++ Pretty.pretty msg
    pretty (Midi msg) = "Midi: " ++ Pretty.pretty msg
    pretty (InputNote msg) = "Input: " ++ show msg
    pretty (DeriveStatus bid status) = "DeriveStatus: " ++ show bid ++ ": "
        ++ Pretty.pretty status
    pretty msg = show msg

data DeriveStatus =
    -- | The current derivation is out of date, but work has not yet started
    -- on a replacement.  The new Performance is already in
    -- 'Cmd.state_current_performance' but not in 'Cmd.state_performance' yet.
    OutOfDate Performance
    | Deriving
    | DeriveComplete Performance
    | Killed
    | LilypondComplete StackMap
    deriving (Show)

instance Pretty.Pretty DeriveStatus where pretty = show

-- Performance should be in "Cmd.Cmd", but that would be a circular import.

-- | This holds the final performance for a given block.  It is used to
-- actually play music, and poked and prodded in a separate thread to control
-- its evaluation.
--
-- This is basically the same as Derive.Result.  I could make them be the
-- same, but Performance wasn't always the same and may not be the same in the
-- future.
data Performance = Performance {
    perf_derive_cache :: Derive.Cache
    , perf_events :: Derive.Events
    , perf_track_dynamic :: Derive.TrackDynamic
    , perf_integrated :: [Derive.Integrated]
    -- | Score damage on top of the Performance, used by the derive cache.
    -- This is empty when the Performance is first created and collects
    -- thereafter.
    , perf_score_damage :: Derive.ScoreDamage
    , perf_warps :: [TrackWarp.Collection]
    , perf_track_signals :: Track.TrackSignals
    }

instance Show Performance where
    show perf = "((Performance " ++ Pretty.pretty len ++ "))"
        where len = Derive.cache_size (perf_derive_cache perf)

-- | Map (row, col) in a written .ly score to stack position of the generating
-- event.  Lilypond can have the generated PDF can report clicks in (row, col)
-- coordinates, but I need to map them back to stacks to highlight the clicked
-- note.
type StackMap = Map.Map (Int, Int) Stack.UiFrame

-- * views

-- To reduce dependency on the exact structure of 'Msg', and to avoid long hard
-- to read cases, use the view functions here.  They can be conveniently used
-- with the ViewPatterns feature.

mouse :: Msg -> Maybe UiMsg.MsgEvent
mouse (Ui (UiMsg.UiMsg _ (UiMsg.MsgEvent mouse@(UiMsg.Mouse {})))) =
    Just mouse
mouse _ = Nothing

mouse_down :: Msg -> Bool
mouse_down msg = case mouse msg of
    Just (UiMsg.Mouse { UiMsg.mouse_state = UiMsg.MouseDown _ }) -> True
    _ -> False

key :: Msg -> Maybe (UiMsg.KbdState, Key.Key)
key (Ui (UiMsg.UiMsg _ (UiMsg.MsgEvent (UiMsg.Kbd state _ key)))) =
    Just (state, key)
key _ = Nothing

key_down :: Msg -> Maybe Key.Key
key_down msg = case key msg of
    Just (UiMsg.KeyDown, k) -> Just k
    _ -> Nothing

char :: Msg -> Maybe (UiMsg.KbdState, Char)
char msg = case key msg of
    Just (state, Key.Char c) -> Just (state, c)
    _ -> Nothing

char_down :: Msg -> Maybe Char
char_down msg = case char msg of
    Just (UiMsg.KeyDown, c) -> Just c
    _ -> Nothing

midi :: Msg -> Maybe Midi.Message
midi (Midi (Midi.ReadMessage { Midi.rmsg_msg = msg })) = Just msg
midi _ = Nothing

context :: Msg -> Maybe UiMsg.Context
context (Ui (UiMsg.UiMsg context _)) = Just context
context _ = Nothing

context_track :: Msg -> Maybe (TrackNum, UiMsg.Track)
context_track = UiMsg.ctx_track <=< context

context_track_pos :: Msg -> Maybe (TrackNum, ScoreTime)
context_track_pos msg = context_track msg >>= \(tracknum, t) -> case t of
    UiMsg.Track pos -> Just (tracknum, pos)
    _ -> Nothing
