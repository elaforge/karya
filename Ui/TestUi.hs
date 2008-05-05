import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Control.Concurrent.STM as STM
import qualified System.IO as IO

import qualified Util.Log as Log
import qualified Util.Thread as Thread

import qualified Ui.UiMsg as UiMsg
import qualified Ui.Initialize as Initialize

import qualified Ui.Block as Block

import qualified Ui.State as State
import qualified Ui.Diff as Diff
import qualified Ui.Sync as Sync

import qualified Ui.TestSetup as TestSetup


main = Initialize.initialize $ \msg_chan -> do
    _msg_th <- Thread.start_thread "print msgs" (msg_thread msg_chan)
    test_sync

msg_thread msg_chan = Monad.forever $ Exception.handle log_exc $ do
    msg <- STM.atomically $ STM.readTChan msg_chan
    putStrLn $ "msg: " ++ UiMsg.pretty_ui_msg msg

log_exc exc = Log.error ("msg thread died from  exception: " ++ show exc)

test_sync = do
    res <- State.run State.empty $ do
        ruler <- State.create_ruler "r1" (TestSetup.mkruler 20 10)
        t1 <- State.create_track "b1.t1" TestSetup.event_track_1
        b1 <- State.create_block "b1" (Block.block "hi b1"
            TestSetup.default_block_config
            (Block.RId ruler) [(Block.TId t1 ruler, 30)])
        v1 <- State.create_view "v1"
            (Block.view b1 TestSetup.default_rect TestSetup.default_view_config)
        return ()
    let (_val, state, updates) = right res
        diff_updates = right $ Diff.diff State.empty state
    print (updates, diff_updates)
    st <- Sync.sync state (diff_updates ++ updates)
    case res of
        Left err -> putStrLn $ "err: " ++ show err
        Right _ -> putStrLn $ "synced"
    pause

right (Left err) = error $ "error: " ++ show err
right (Right x) = x

pause = putStr "? " >> IO.hFlush IO.stdout >> getLine >> return ()
