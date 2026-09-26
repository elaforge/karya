-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
-- | Like "Ui.BlockCStub" for "Ui.Ui".
module Ui.FltkStub (
    Fltk, fltk, Channel, eventLoop, sendAction, quitUiThread
) where
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM as STM

import qualified Ui.UiMsg as UiMsg

import           Global


newtype Fltk a = Fltk (IO a)
    deriving (Applicative, Functor, Monad, MonadIO)

fltk :: IO a -> Fltk a
fltk = Fltk

type Channel = MVar.MVar [(Fltk (), Text)]

type QuitRequest = MVar.MVar ()

eventLoop :: Channel -> QuitRequest -> STM.TChan UiMsg.UiMsg -> IO ()
eventLoop uiChan quitRequest msgChan = pure ()

sendAction :: Channel -> Text -> Fltk () -> IO ()
sendAction uiChan description act = pure ()

quitUiThread :: QuitRequest -> IO ()
quitUiThread quitRequest = pure ()
