{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fglasgow-exts #-}
module Ui.TestUi where

import qualified Control.Monad as Monad
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM.TChan as TChan
import qualified Control.Concurrent.STM as STM
import Foreign
import qualified System.IO as IO
import qualified System.IO.Unsafe as Unsafe

import qualified Util.Test as Test
import qualified Util.Thread as Thread
import Ui.Types
import qualified Ui.Util as Util
import qualified Ui.Color as Color
import qualified Ui.UiMsg as UiMsg
import qualified Ui.Ui as Ui
import qualified Ui.Block as Block
import qualified Ui.Ruler as Ruler
import qualified Ui.Track as Track
import qualified Ui.Event as Event

main = Ui.initialize $ \msg_chan -> do
    msg_th <- Thread.start_thread "print msgs" (msg_thread msg_chan)
    test_view


test_view = do
    block <- Block.create block_config
    track_ruler <- Ruler.create (ruler_config 64)
    overlay_ruler <- Ruler.create (overlay_config (ruler_config 64))

    t1 <- Track.create Color.white

    -- Insert some tracks before creating the view, some after.
    Block.insert_track block 0 (Block.T t1 overlay_ruler) 70

    view <- Block.create_view (0, 0) (100, 200) block track_ruler view_config

    Block.insert_track block 1 (Block.D Color.blue) 5
    Block.insert_track block 2 (Block.R track_ruler) 15
    Block.insert_track block 3 (Block.T t1 overlay_ruler) 50

    Block.set_selection view 0
        (Block.Selection (0, TrackPos 0) (2, TrackPos 0))

    Track.insert_event t1 (TrackPos 96) (event "tiny" 0)

    -- Util.show_children view >>= putStrLn
    putStr "? " >> IO.hFlush IO.stdout >> getLine
    return ()


msg_thread msg_chan = Monad.forever $ do
    msg <- STM.atomically $ TChan.readTChan msg_chan
    putStrLn $ "msg: " ++ UiMsg.pretty_ui_msg msg


test_scroll_zoom view = do
    Block.set_track_scroll view 150
    Block.get_track_scroll view >>= print
    Block.set_track_scroll view 0
    -- test zoom and time scroll


-- * Cheap tests.  TODO: use HUnit?
empty_ruler = Ruler.create (ruler_config 0)
io_equal :: (Eq a, Show a) => IO a -> a -> IO ()
io_equal = Test.io_check_equal

-- ** view tests

test_set_config = do
    block <- setup_event_track
    ruler <- empty_ruler
    view <- Block.create_view (0, 0) (200, 200) block ruler view_config

    -- block config
    Test.io_human "track box turns red" $
        Block.set_config block
            (block_config { Block.config_track_box_color = Color.rgb 1 0 0 })
    config <- Block.get_config block
    Test.io_human "sb box also red" $
        Block.set_config block
            (config { Block.config_sb_box_color = Color.rgb 1 0 0 })
    config <- Block.get_config block
    Test.io_human "block bg turns red" $
        Block.set_config block
            (config { Block.config_bg_color = Color.rgb 1 0 0 })
    config <- Block.get_config block

    Block.set_selection view 0
        (Block.Selection (0, TrackPos 10) (1, TrackPos 20))
    Test.io_human "selection turns red" $
        Block.set_config block
            (config {Block.config_select_colors
                = Color.rgb 1 0 0 : tail (Block.config_select_colors config)})

    -- view config

-- ** track tests

-- Give me a block with one event track.
setup_event_track = do
    block <- Block.create block_config
    ruler <- empty_ruler
    track <- Track.create Color.white
    Block.insert_track block 0 (Block.T track ruler) 50
    return block

test_insert_events = do
    block <- setup_event_track
    Block.T track _ <- Block.track_at block 0
    Track.insert_event track (TrackPos 20) (event "brick" 10)
    Track.insert_event track (TrackPos 40) (event "so" 10)
    Track.insert_event track (TrackPos 60) (event "bleck" 10)

    let repl pos = Track.insert_event track (TrackPos pos) (event "replace" 10)
    -- overlap previous, as middle or last event
    io_equal (repl 25) False
    io_equal (repl 65) False
    -- overlap next, as first or middle event
    io_equal (repl 15) False
    io_equal (repl 35) False
    -- exact match replaces
    io_equal (repl 20) True
    -- insert as first, middle, or last
    io_equal (repl 0) True
    io_equal (repl 30) True
    io_equal (repl 70) True

    ruler <- empty_ruler
    view <- Block.create_view (0, 0) (100, 200) block ruler view_config
    Test.io_human "alternating 'replace' and 'krazy' events, no brick"
        (return ())

test_view_selections view = do
    -- TODO incomplete
    Test.io_human "insertion point at beginning over 2 tracks " $
        Block.set_selection view 0
            (Block.Selection (0, TrackPos 0) (2, TrackPos 0))
    Test.io_human "insertion point moves, only 1 track" $
        Block.set_selection view 0
            (Block.Selection (0, TrackPos 16) (1, TrackPos 0))

test_view_track_width view = do
    -- test set sizes
    -- TODO incomplete
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


-- * setup

major n = Ruler.Mark 1 3 (Color.rgba 0.45 0.27 0 0.35) (show n) 0 0
minor = Ruler.Mark 2 2 (Color.rgba 1 0.39 0.2 0.35) "" 0 0
marklist n = Unsafe.unsafePerformIO $
    Ruler.create_marklist (take n $ zip (map TrackPos [0, 10 ..]) m44)


m44 = concatMap (\n -> [major n, minor, minor, minor]) [0..]

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

event s dur = Event.Event s (TrackPos dur) Color.gray7 text_style False
text_style = TextStyle Helvetica [] 12 Color.black
