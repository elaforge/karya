module Cmd.Msg where

import qualified Ui.UiMsg as UiMsg
import qualified Midi.Midi as Midi


data Msg = Ui UiMsg.UiMsg | Midi Midi.ReadMessage -- MOsc OscMsg.OscMsg
    deriving (Show)
