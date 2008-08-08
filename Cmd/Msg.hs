module Cmd.Msg where
import qualified Data.Char as Char
import qualified System.IO as IO

import qualified Ui.Key as Key
import qualified Ui.UiMsg as UiMsg
import qualified Midi.Midi as Midi
import qualified Perform.Transport as Transport


data Msg =
    -- | Message from the UI.
    Ui UiMsg.UiMsg
    -- | Incoming midi message.
    | Midi Midi.ReadMessage
    -- | Message from the transport/play thread.
    | Transport Transport.Status
    -- | Message from the language control socket, includes the socket handle
    -- than can be used to write a response.  Whoever responds to Socket should
    -- close the handle.
    | Socket IO.Handle String
    deriving (Show)

-- * player msgs

-- To reduce dependency on the exact structure of 'Msg', and to avoid long hard
-- to read cases, use the "view" functions here.
--
-- Unfortunately haskell doesn't support view functions in cases yet.

mouse (Ui (UiMsg.UiMsg _ (UiMsg.MsgEvent mouse@(UiMsg.Mouse {})))) =
    Just mouse
mouse _ = Nothing

key (Ui (UiMsg.UiMsg _ (UiMsg.MsgEvent (UiMsg.Kbd UiMsg.KeyDown key)))) =
    Just key
key _ = Nothing

alphanum (Key.KeyChar c)
    | Char.isAlphaNum c = Just c
    | otherwise = Nothing
alphanum _ = Nothing

context (Ui (UiMsg.UiMsg context _)) = Just context
context _ = Nothing

context_track_pos msg = do
    context <- context msg
    case context of
        UiMsg.Context { UiMsg.ctx_track = Just track, UiMsg.ctx_pos = Just pos }
            -> Just (track, pos)
        _ -> Nothing
