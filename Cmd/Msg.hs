module Cmd.Msg where

import qualified Ui.Block as Block
import qualified Ui.UiMsg as UiMsg
import qualified Midi.Midi as Midi


data Msg = Ui UiMsg.UiMsg | Midi Midi.ReadMessage -- MOsc OscMsg.OscMsg
    deriving (Show)

-- To reduce dependency on the exact structure of 'Msg', and to avoid long hard
-- to read cases, use the "view" functions here.
--
-- Unfortunately haskell doesn't support view functions in cases yet.

mouse (Ui (UiMsg.UiMsg _ (UiMsg.MsgEvent mouse@(UiMsg.Mouse {})))) =
    Just mouse
mouse _ = Nothing

context (Ui (UiMsg.UiMsg context _)) = Just context
context _ = Nothing

context_track_pos msg = do
    context <- context msg
    case context of
        UiMsg.Context { UiMsg.ctx_track = Just track, UiMsg.ctx_pos = Just pos }
            -> Just (track, pos)
        _ -> Nothing
