{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fglasgow-exts #-}
module Interface.TestInterface where

import qualified Control.Monad as Monad
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM.TChan as TChan
import qualified Control.Concurrent.STM as STM
import Foreign
import qualified System.IO as IO

import qualified Util.Thread as Thread
import Interface.Types
import qualified Interface.Util as Util
import qualified Interface.Color as Color
import qualified Interface.UiMsg as UiMsg
import qualified Interface.Ui as Ui
import qualified Interface.Block as Block
import qualified Interface.Ruler as Ruler
import qualified Interface.Track as Track

main = test_view

test_view = Ui.initialize $ \msg_chan -> do
    block <- Block.create block_config
    track_ruler <- Ruler.create ruler_config
    overlay_ruler <- Ruler.create (overlay_config ruler_config)

    t1 <- Track.create Color.white

    msg_th <- Thread.start_thread "print msgs" (msg_thread msg_chan)

    view <- Block.create_view (0, 0) (100, 200) block track_ruler view_config
    Block.insert_track block 0 (Block.R track_ruler) 10
    Block.insert_track block 1 (Block.T t1 overlay_ruler) 70
    Block.insert_track block 2 (Block.T t1 overlay_ruler) 50

    Block.set_track_scroll view 150
    Block.get_track_scroll view >>= print
    Block.set_track_scroll view 0

    -- Util.show_children view >>= putStrLn
    putStr "? " >> IO.hFlush IO.stdout >> getLine
    putStrLn "killing"
    Concurrent.killThread msg_th

msg_thread msg_chan = Monad.forever $ do
    msg <- STM.atomically $ TChan.readTChan msg_chan
    return ()
    -- let s = case msg of
    --         Ui.MUi m -> UiMsg.short_show m
    --         _ -> show msg
    -- putStrLn $ "msg: " ++ s

test_block = do
    block <- Block.create block_config
    Block.set_title block "oh no"
    t <- Block.get_title block
    putStrLn t

test_ruler = do
    ruler1 <- Ruler.create ruler_config
    track1 <- Track.create track_bg

    block1 <- Block.create block_config
    block2 <- Block.create block_config

    Block.insert_track block1 0 (Block.R ruler1) 20
    Block.insert_track block1 1 (Block.T track1 ruler1) 60
    Block.insert_track block1 2 (Block.D Color.blue) 5
    print "done"

major = Ruler.Mark 1 3 (Color.rgba 0.45 0.27 0 0.35) "" 0 0
minor = Ruler.Mark 2 2 (Color.rgba 1 0.39 0.2 0.35) "" 0 0
marklist = Ruler.Marklist $ take 64 $ zip (map TrackPos [0, 10 ..])
    (cycle [major, minor, minor, minor])

ruler_bg = Color.rgb 1 0.85 0.5
ruler_config = Ruler.Config [marklist] ruler_bg True False False
-- Convert a ruler config for an overlay ruler.
overlay_config config = config
    { Ruler.config_show_names = False
    , Ruler.config_use_alpha = True
    , Ruler.config_full_width = True
    }

block_config = Block.BlockModelConfig
    { Block.config_select_colors = [Color.black]
    , Block.config_bg_color = Color.gray8
    , Block.config_track_box_color = Color.rgb 0.25 1 1
    , Block.config_sb_box_color = Color.rgb 0.25 1 1
    }

view_config = Block.BlockViewConfig
    { Block.vconfig_zoom_speed = 1
    , Block.vconfig_block_title_height = 20
    , Block.vconfig_track_title_height = 20
    , Block.vconfig_sb_size = 12
    , Block.vconfig_ruler_size = 18
    , Block.vconfig_status_size = 16
    }

track_bg = Color.white

test_color = do
    alloca $ \colorp -> do
        poke colorp Color.red
        c_print_color colorp

foreign import ccall "print_color" c_print_color :: Ptr Color.Color -> IO ()
