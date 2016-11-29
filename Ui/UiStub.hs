-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
-- | Like "Ui.BlockCStub" for "Ui.Ui".
module Ui.UiStub (
    Fltk, fltk, Channel, event_loop, send_action, quit_ui_thread
) where
import qualified Control.Applicative as Applicative
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM as STM
import qualified Control.Monad.Trans as Trans

import qualified Ui.UiMsg as UiMsg
import Global


newtype Fltk a = Fltk (IO a)
    deriving (Applicative.Applicative, Functor, Monad, Trans.MonadIO)

fltk :: IO a -> Fltk a
fltk = Fltk

type Channel = MVar.MVar [(Fltk (), Text)]

type QuitRequest = MVar.MVar ()

event_loop :: Channel -> QuitRequest -> STM.TChan UiMsg.UiMsg -> IO ()
event_loop ui_chan quit_request msg_chan = return ()

send_action :: Channel -> Text -> Fltk () -> IO ()
send_action ui_chan description act = return ()

quit_ui_thread :: QuitRequest -> IO ()
quit_ui_thread quit_request = return ()
