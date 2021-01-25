-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
-- | This has the FLTK event thread, and communication with it.
module Ui.Fltk (
    Fltk, fltk, Channel, event_loop, send_action, quit_ui_thread
) where
#ifdef STUB_OUT_FLTK
import Ui.FltkStub
#else

import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as Exception

import qualified Foreign
import qualified Foreign.C as C

import qualified Util.FFI as FFI
import qualified Util.Log as Log
import qualified Ui.UiMsg as UiMsg
import qualified Ui.UiMsgC as UiMsgC

import           Global


-- | See NOTE [ui-loop-timing]
action_timing :: Bool
action_timing = False

-- | You should only talk to FLTK from the main thread, which is also the FLTK
-- event thread.  So to call a FLTK function, you have to put it on
-- the UI 'Channel', where the FLTK thread will pick it up.  This also
-- serializes them, so I don't have to worry about concurrency at the Fltk
-- layer.  Since FLTK operations are wrapped in Fltk, and only this module can
-- unwrap a Fltk, this should enforce that you can't cell them willy-nilly.
newtype Fltk a = Fltk (IO a)
    deriving (Applicative, Functor, Monad, MonadIO)

fltk :: IO a -> Fltk a
fltk = Fltk

-- | Channel to communicate with the FLTK event loop.  Yes it's not a real
-- channel, but I want to get all actions in one go, and an MVar is suitable
-- for that.
type Channel = MVar.MVar [(Fltk (), Text)]

-- | Putting something into this mvar signals the UI thread to quit.
type QuitRequest = MVar.MVar ()

-- | Run the FLTK event loop thread, passing it a channel that produces msgs,
-- and go into the UI polling loop.  This is intended to be run from the main
-- thread, since some UIs don't work properly unless run from the main thread.
-- When the app exits, the ui loop will be aborted.
event_loop :: Channel -> QuitRequest -> STM.TChan UiMsg.UiMsg -> IO ()
event_loop ui_chan quit_request msg_chan = do
    finalizer <- c_make_free_fun_ptr FFI.freeFunPtr
    c_initialize finalizer
    while_ (fmap not (MVar.isEmptyMVar quit_request)) $
        fltk_event_loop ui_chan msg_chan

-- | When I do anything that will destroy previous callbacks, I have to pass
-- yet another callback which will be used to mark the old callbacks as done,
-- so that the haskell GC knows it can collect the data those callbacks use.
type FunPtrFinalizer a = Foreign.FunPtr a -> IO ()
foreign import ccall "wrapper"
    c_make_free_fun_ptr :: FunPtrFinalizer a
        -> IO (Foreign.FunPtr (FunPtrFinalizer a))

-- | Send the UI to the ui thread to run asynchronously.
send_action :: Channel -> Text -> Fltk () -> IO ()
send_action ui_chan description act = do
    MVar.modifyMVar_ ui_chan $ return . ((act, description) :)
    awake

{- NOTE [ui-loop-timing]
    Enable GUI timing by incrementing Timing::level in fltk/util.h.
    It will write to seq.events, then use tools/parse_timing.py to analyze it,
    or just look at it directly, to find where the big jumps are.

    util::timing emits timing events for the UI event loop, but it's confusing
    becasue things happen inside Fl::wait():

        Fl::wait() enter                [libfltk]
        "Block::draw"                   [c++]
        waiting for OS event            [OS] <-- blocking
        events collect in MsgCollector  [c++]
        "evt-xyz"
        Fl::wait() return
        "events"
        handle_actions (mutate fltk data via FFI)       [haskell]
        get UI msgs from MsgCollector, put on msg_chan  [haskell]
        "haskell"
        Fl::wait() enter

    So the single cycle goes "events", "haskell", "Block::draw".

    UI latency can come from:
    . handle_actions, if not already in normal form
    . Block::draw()

    Cmds run in their own async loop, which can also suffer from latency.  In
    that case, the UI is still responsive in theory, but not in practice since
    even selections go through cmd.
-}

-- | The FLTK event loop.
fltk_event_loop :: Channel -> STM.TChan UiMsg.UiMsg -> IO ()
fltk_event_loop ui_chan msg_chan = do
    wait
    -- I think that fltk will wake up once for every call to awake, so I
    -- shouldn't have to worry about another awake call coming in right
    -- here.
    handle_actions ui_chan
    ui_msgs <- UiMsgC.get_ui_msgs
    -- TODO
    -- when (length handled > 0 && length ui_msgs > 0)
    -- This means there is a possibility for a race.  If a haskell action says
    -- to e.g. set scroll to X and fltk says the scroll was set to Y, then
    -- haskell will record the scroll as Y while fltk has it as X.  I could
    -- mitigate this by cancelling out UiMsgs that are outdated by an incoming
    -- haskell action, but at the moment it doesn't seem worth the effort.
    --
    -- This is possible for UpdateTrackScroll and UpdateTimeScroll, but the
    -- effects should be fairly benign, and fixed as soon as there is any
    -- scrolling or zooming.  It would look like e.g. play from the top of
    -- the view playing from the wrong point.
    unless (null ui_msgs) $
        STM.atomically $ mapM_ (STM.writeTChan msg_chan) ui_msgs

-- | Synchronously take actions out of the 'Channel' and run them.  This could
-- be asynchronous, but this way if the FLTK event loop wedges up then the UI
-- will also wedge up.  That's not exactly good, but it lets me know something
-- has gone wrong quickly.
handle_actions :: Channel -> IO ()
handle_actions ui_chan = MVar.modifyMVar_ ui_chan $ \actions -> do
    -- Since actions are added to the front, reverse them before executing.
    Exception.handle handle $
        forM_ (reverse actions) $ \(Fltk action, name) -> do
            when action_timing $ FFI.withText name $ \namep ->
                c_timing 1 namep
            action
    return []
    where
    handle :: Exception.SomeException -> IO ()
    handle exc = Log.error $ "exception in event_loop: " <> showt exc

quit_ui_thread :: QuitRequest -> IO ()
quit_ui_thread quit_request = do
    MVar.tryTakeMVar quit_request
    awake -- get it out of wait

foreign import ccall "initialize"
    c_initialize :: Foreign.FunPtr (FunPtrFinalizer a) -> IO ()
foreign import ccall "ui_wait" wait :: IO ()
foreign import ccall "ui_awake" awake :: IO ()

foreign import ccall unsafe "timing" c_timing :: C.CInt -> C.CString -> IO ()

#endif
