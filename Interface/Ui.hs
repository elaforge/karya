{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fglasgow-exts #-}
module Interface.Ui where
import qualified Control.Monad as Monad
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM.TChan as TChan
import qualified Control.Concurrent.STM as STM

import Interface.Types
import qualified Interface.Util as Util
import qualified Interface.UiMsg as UiMsg
import qualified Interface.MidiMsg as MidiMsg
import qualified Interface.OscMsg as OscMsg

{-
The problem is to always execute actions from the same thread.  Solutions:

Need to be single threaded:
create view
modify view (model modification counts too)

Don't need to be single threaded:
query model
query view (make sure that all values are stored in the class, not the widget,
so e.g. title is set by fl_input callback)

Onle the write actions should trigger coming out of wait()... but then I guess
even reads should be serialized.  If there are certain things that get called
a lot maybe I could put the lock in the c++?

In any case, it would be good to abstract away 'send_action' so I just call the
functions and they work right.

X Passing an existentially quantified type on the chan doesn't work, since all
types passed must support the same operations.

- unsafeCoerce# the operation to Any, then unsafeCoerce# the mvar contents back
again

- do the serialization in the c interface: send a function
(\f mvar -> f >>= putMVar mvar) (c_function args) retbox as a nullary foreign
export (can I do this?), and then pass it to awake().  wait() calls the passed
function when it returns.

- Figure out how to call fltk from multiple threads.  I can use Fl::lock(), but
won't it block in wait()?  No, I think wait() unlocks.  Which platforms don't
like reentrant gui calls?

- Don't return values.  

- Just require that all gui calls be in the same thread

- each view operation has its own (TMVar Bool, TVar result).  The ui thread
waits on all TMVars


case labelled op_vars of
    create_block_view -> 


-}

-- Start up the ui thread
initialize :: IO (Concurrent.ThreadId, forall a. UI a -> IO a, TChan.TChan Msg)
initialize = do
    act_chan <- TChan.newTChanIO
    msg_chan <- TChan.newTChanIO
    th_id <- Util.start_os_thread "ui handler" (ui_thread act_chan msg_chan)
    return (th_id, send_action act_chan, msg_chan)

send_action act_chan act = do
    putStrLn "wrote act"
    retbox <- MVar.newEmptyMVar
    write act_chan (act, retbox)
    awake
    MVar.takeMVar retbox

-- The ui thread's polling cycle.
ui_thread act_chan msg_chan = do
    c_initialize
    Monad.forever $ do
        wait
        handle_actions act_chan
        handle_msgs msg_chan

kill_ui_thread th_id = do
    Concurrent.killThread th_id
    awake -- get it out of wait

foreign import ccall unsafe "initialize" c_initialize :: IO ()
-- "wait" must be safe since it blocks.
foreign import ccall safe "ui_msg_wait" wait :: IO ()
foreign import ccall unsafe "ui_msg_awake" awake :: IO ()

handle_actions act_chan = STM.atomically (read_all act_chan)
    >>= mapM (\(act, retbox) -> act >>= MVar.putMVar retbox)

read_all chan = fmap Just (TChan.readTChan chan) `STM.orElse` return Nothing
    >>= maybe (return []) (\v -> fmap (v:) (read_all chan))


data Msg = MUi UiMsg.UiMsg | MMidi MidiMsg.MidiMsg | MOsc OscMsg.OscMsg
    deriving (Show)

handle_msgs msg_chan = do
    ui <- UiMsg.take_ui_msgs
    midi <- MidiMsg.take_midi_msgs
    osc <- OscMsg.take_osc_msgs
    mapM_ (write msg_chan) (map MUi ui ++ map MMidi midi ++ map MOsc osc)

write tchan = STM.atomically . TChan.writeTChan tchan
