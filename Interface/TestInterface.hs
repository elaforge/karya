{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fglasgow-exts #-}
module Interface.TestInterface where

import qualified Control.Monad as Monad
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM.TChan as TChan
import qualified Control.Concurrent.STM as STM
import Foreign
import qualified System.IO as IO

import Interface.Types
import qualified Interface.Util as Util
import qualified Interface.Color as Color
import qualified Interface.Ui as Ui
import qualified Interface.Block as Block
import qualified Interface.Ruler as Ruler
import qualified Interface.Track as Track

main = test_view

test_view = do
    block <- Block.create block_config
    ruler <- Ruler.create ruler_bg [marklist]

    msg_chan <- Ui.initialize
    msg_th <- Util.start_thread "print msgs" (msg_thread msg_chan)
    view <- Ui.send_action $
        Block.create_view (10, 10) (200, 200) block ruler view_config
    putStrLn $ "got response " ++ show view
    putStr "? " >> IO.hFlush IO.stdout >> getLine
    putStrLn "killing"
    Concurrent.killThread msg_th
    Ui.kill_ui_thread

msg_thread msg_chan = Monad.forever $ do
    msg <- STM.atomically $ TChan.readTChan msg_chan
    putStrLn $ "msg: " ++ show msg

test_block = do
    block <- Block.create block_config
    Block.set_title block "oh no"
    t <- Block.get_title block
    putStrLn t

test_ruler = do
    ruler1 <- Ruler.create ruler_bg [marklist]
    track1 <- Track.create

    block1 <- Block.create block_config
    block2 <- Block.create block_config

    Block.insert_track block1 0 (Block.R ruler1) 20
    Block.insert_track block1 1 (Block.T (track1, ruler1)) 60
    Block.insert_track block1 2 (Block.D Color.blue) 5
    print "done"

major = Ruler.Mark 1 3 (Color.Color 0.45 0.27 0 0) "" 0 0
minor = Ruler.Mark 2 2 (Color.Color 1 0.39 0.2 0) "" 0 0
marklist = Ruler.Marklist $ take 10 $ zip (map TrackPos [10, 20 ..])
    (cycle [major, minor, minor, minor])

block_config = Block.BlockModelConfig [Color.black] Color.white
    Color.blue Color.blue
view_config = Block.BlockViewConfig 1
ruler_bg = Color.Color 1 0.85 0.5 0

test_color = do
    alloca $ \colorp -> do
        poke colorp Color.red
        c_print_color colorp

foreign import ccall "print_color" c_print_color :: Ptr Color.Color -> IO ()
