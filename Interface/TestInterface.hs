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

main = Ui.initialize $ \msg_chan -> do
    msg_th <- Thread.start_thread "print msgs" (msg_thread msg_chan)
    test_view


test_view = do
    block <- Block.create block_config
    track_ruler <- Ruler.create (ruler_config 64)
    overlay_ruler <- Ruler.create (overlay_config (ruler_config 64))

    t1 <- Track.create Color.white

    -- Insert some tracks before creating the view, some after.
    Block.insert_track block 0 (Block.R track_ruler) 15
    view <- Block.create_view (0, 0) (100, 200) block track_ruler view_config
    Block.insert_track block 1 (Block.T t1 overlay_ruler) 70
    Block.insert_track block 2 (Block.T t1 overlay_ruler) 50
    Block.insert_track block 2 (Block.D Color.blue) 5

    -- test selections
    Block.set_selection view 0
        (Block.Selection (0, TrackPos 0) (2, TrackPos 0))

    -- Util.show_children view >>= putStrLn
    putStr "? " >> IO.hFlush IO.stdout >> getLine


msg_thread msg_chan = Monad.forever $ do
    msg <- STM.atomically $ TChan.readTChan msg_chan
    return ()
    -- let s = case msg of
    --         Ui.MUi m -> UiMsg.short_show m
    --         _ -> show msg
    -- putStrLn $ "msg: " ++ s


test_scroll_zoom view = do
    Block.set_track_scroll view 150
    Block.get_track_scroll view >>= print
    Block.set_track_scroll view 0
    -- test zoom and time scroll


-- * Cheap tests.  TODO: use HUnit?

-- ** view tests

test_view_track_width view = do
    -- test set sizes
    track_ruler <- Ruler.create (ruler_config 3)
    Block.insert_track (Block.view_block view) 0 (Block.R track_ruler) 15
    io_equal (Block.get_track_width view 0) 15
    Block.set_track_width view 0 10
    io_equal (Block.get_track_width view 0) 10
    Block.set_track_width view 0 5
    -- minimum size is 10
    io_equal (Block.get_track_width view 0) 10

-- ** block tests

test_block = do
    block <- Block.create block_config

    io_equal (Block.get_config block) block_config
    let config2 = block_config { Block.config_bg_color = Color.black }
    Block.set_config block config2
    io_equal (Block.get_config block) config2

    Block.set_title block "oh no"
    io_equal (Block.get_title block) "oh no"

    io_equal (Block.get_attrs block) []
    let attrs = [("hi", "there")]
    Block.set_attrs block attrs
    io_equal (Block.get_attrs block) attrs

test_block_tracks = do
    block <- Block.create block_config
    track_ruler <- Ruler.create (ruler_config 2)
    overlay_ruler <- Ruler.create (overlay_config (ruler_config 2))
    t1 <- Track.create Color.white

    io_equal (Block.tracks block) 0

    Block.insert_track block 0 (Block.R track_ruler) 10
    Block.insert_track block 1 (Block.T t1 overlay_ruler) 70
    Block.insert_track block 1 (Block.D Color.blue) 10

    io_equal (Block.tracks block) 3

    io_equal (Block.track_at block 0) (Block.R track_ruler)
    io_equal (Block.track_at block 1) (Block.D Color.blue)
    io_equal (Block.track_at block 2) (Block.T t1 overlay_ruler)

io_equal io_val expected = do
    val <- io_val
    if val == expected
        then putStrLn $ "++-> " ++ show val
        else error $ "expected: " ++ show expected ++ ", got: " ++ show val


-- * setup

major = Ruler.Mark 1 3 (Color.rgba 0.45 0.27 0 0.35) "" 0 0
minor = Ruler.Mark 2 2 (Color.rgba 1 0.39 0.2 0.35) "" 0 0
marklist n = Ruler.Marklist $ take n $ zip (map TrackPos [0, 10 ..])
    (cycle [major, minor, minor, minor])

ruler_bg = Color.rgb 1 0.85 0.5
ruler_config marks = Ruler.Config [marklist marks] ruler_bg True False False
-- Convert a ruler config for an overlay ruler.
overlay_config config = config
    { Ruler.config_show_names = False
    , Ruler.config_use_alpha = True
    , Ruler.config_full_width = True
    }

block_config = Block.Config
    { Block.config_select_colors =
        let sel = Color.alpha 0.3 . Color.lighten 0.8 in
            [sel Color.blue, sel Color.green, sel Color.red]
    , Block.config_bg_color = Color.gray8
    , Block.config_track_box_color = Color.rgb 0.25 1 1
    , Block.config_sb_box_color = Color.rgb 0.25 1 1
    }

view_config = Block.ViewConfig
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
