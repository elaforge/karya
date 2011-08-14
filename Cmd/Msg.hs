module Cmd.Msg where
import Control.Monad
import qualified Data.Map as Map
import qualified System.IO as IO

import qualified Util.Pretty as Pretty
import qualified Midi.Midi as Midi
import Ui
import qualified Ui.Key as Key
import qualified Ui.Track as Track
import qualified Ui.UiMsg as UiMsg

import qualified Cmd.InputNote as InputNote
import qualified Perform.Transport as Transport


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

data DeriveStatus = OutOfDate | Deriving | DeriveFailed
    | DeriveComplete Track.TrackSignals
    deriving (Show)

instance Pretty.Pretty DeriveStatus where
    pretty (DeriveComplete signals) =
        "DeriveComplete " ++ Pretty.pretty (Map.keys signals)
    pretty status = show status

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
