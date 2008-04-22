import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Control.Concurrent.STM as STM
import qualified System.IO as IO
-- import qualified System.IO.Unsafe as Unsafe

import qualified Util.Log as Log
import qualified Util.Test as Test
import qualified Util.Thread as Thread

import Ui.Types
-- import qualified Ui.Util as Util
import qualified Ui.Color as Color
import qualified Ui.UiMsg as UiMsg
import qualified Ui.Initialize as Initialize

import qualified Ui.Block as Block
import qualified Ui.BlockC as BlockC
import qualified Ui.Ruler as Ruler
import qualified Ui.Track as Track
import qualified Ui.Event as Event

import qualified Ui.State as State
import qualified Ui.Diff as Diff
import qualified Ui.Sync as Sync


main = Initialize.initialize $ \msg_chan -> do
    msg_th <- Thread.start_thread "print msgs" (msg_thread msg_chan)
    test_sync

msg_thread msg_chan = Monad.forever $ Exception.handle log_exc $ do
    msg <- STM.atomically $ STM.readTChan msg_chan
    putStrLn $ "msg: " ++ UiMsg.pretty_ui_msg msg

log_exc exc = Log.error ("msg thread died from  exception: " ++ show exc)

send = Initialize.send_action

event pos name dur = (TrackPos pos, Event.event name (TrackPos dur))

test_sync = do
    res <- State.run State.empty $ do
        ruler <- State.create_ruler "r1" (mkruler 20 10)
        t1 <- State.create_track "b1.t1" event_track_1
        b1 <- State.create_block "b1" (Block.Block "hi b1" default_block_config
            (Block.R ruler) [(Block.T t1 ruler, 30)])
        v1 <- State.create_view "v1"
            (Block.view b1 default_rect default_view_config)
        return ()
    let (_val, state, updates) = right res
        diff_updates = right $ Diff.diff State.empty state
    print (updates, diff_updates)
    st <- Sync.sync state (diff_updates ++ updates)
    case res of
        Left err -> putStrLn $ "err: " ++ show err
        Right s -> putStrLn $ "synced"
    pause

right (Left err) = error $ "error: " ++ show err
right (Right x) = x

test = do
    let ruler = mkruler 20 10
    view <- create_empty_view
    send $ BlockC.insert_track view
        0 (BlockC.T empty_track (overlay_ruler ruler)) 30
    send $ BlockC.insert_track view
        1 (BlockC.D (Block.Divider Color.blue)) 5
    send $ BlockC.insert_track view
        2 (BlockC.T event_track_1 (overlay_ruler ruler)) 30
    putStrLn $ "created view " ++ show view
    pause
    return ()

pause = putStr "? " >> IO.hFlush IO.stdout >> getLine >> return ()

create_empty_view = do
    let view_id = Block.ViewId "default"
    send $ BlockC.create_view view_id default_rect
        default_view_config default_block_config default_ruler_track
    return view_id

create_default_view = do
    let ruler = default_ruler
    view <- create_empty_view
    send $ BlockC.insert_track view
        0 (BlockC.T event_track_1 (overlay_ruler ruler)) 30
    send $ BlockC.insert_track view
        1 (BlockC.D (Block.Divider Color.blue)) 5
    send $ BlockC.insert_track view
        2 (BlockC.T event_track_2 (overlay_ruler ruler)) 30
    return view

{-


-- * Cheap tests.  TODO: use HUnit?
empty_ruler = Ruler.create (mkruler 0)
io_equal :: (Eq a, Show a) => IO a -> a -> IO ()
io_equal = Test.io_check_equal

-- ** view tests

setup_view = do
    block <- Block.create default_block_config
    ruler <- empty_ruler
    Block.create_view block default_rect ruler default_view_config

test_set_config = do
    block <- setup_event_track
    ruler <- empty_ruler
    view <- Block.create_view block (Block.Rect (0, 0) (200, 200)) ruler
        default_view_config

    -- block config
    Test.io_human "track box turns red" $
        Block.set_config block
            (default_block_config { Block.config_track_box_color = Color.rgb 1 0 0 })
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
        (Block.Selection 0 (TrackPos 10) 1 (TrackPos 20))
    Test.io_human "selection turns red" $
        Block.set_config block
            (config {Block.config_select_colors
                = Color.rgb 1 0 0 : tail (Block.config_select_colors config)})

    -- view config

-- ** track tests

-- Give me a block with one event track.
setup_event_track = do
    block <- Block.create default_block_config
    ruler <- empty_ruler
    track <- Track.create Color.white
    Block.create_track block 0 (Block.T track ruler) 50
    return block

test_insert_events = do
    block <- setup_event_track
    (Block.T track  _ruler, _width) <- Block.track_at block 0
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
    view <- Block.create_view block default_rect ruler default_view_config
    Test.io_human "alternating 'replace' and 'krazy' events, no brick"
        (return ())

test_view_selections view = do
    -- TODO incomplete
    Test.io_human "insertion point at beginning over 2 tracks " $
        Block.set_selection view 0
            (Block.Selection 0 (TrackPos 0) 2 (TrackPos 0))
    Test.io_human "insertion point moves, only 1 track" $
        Block.set_selection view 0
            (Block.Selection 0 (TrackPos 16) 1 (TrackPos 0))

test_view_track_width view = do
    -- test set sizes
    -- TODO incomplete
    track_ruler <- Ruler.create (ruler 3)
    Block.create_track (Block.view_block view) 0 (Block.R track_ruler) 15
    io_equal (Block.get_track_width view 0) 15
    Block.set_track_width view 0 10
    io_equal (Block.get_track_width view 0) 10
    Block.set_track_width view 0 5
    -- minimum size is 10
    io_equal (Block.get_track_width view 0) 10

-- ** block tests

test_block = do
    block <- Block.create default_block_config

    io_equal (Block.get_config block) default_block_config
    let config2 = default_block_config { Block.config_bg_color = Color.black }
    Block.set_config block config2
    io_equal (Block.get_config block) config2

    Block.set_title block "oh no"
    io_equal (Block.get_title block) "oh no"

    io_equal (Block.get_attrs block) []
    let attrs = [("hi", "there")]
    Block.set_attrs block attrs
    io_equal (Block.get_attrs block) attrs

test_block_tracks = do
    block <- Block.create default_block_config
    track_ruler <- Ruler.create (ruler 2)
    overlay_ruler <- Ruler.create (overlay_ruler (ruler 2))
    t1 <- Track.create Color.white

    io_equal (Block.tracks block) 0

    Block.create_track block 0 (Block.R track_ruler) 10
    Block.create_track block 1 (Block.T t1 overlay_ruler) 70
    Block.create_track block 1 (Block.D Color.blue) 10

    io_equal (Block.tracks block) 3

    io_equal (Block.track_at block 0) ((Block.R track_ruler), 10)
    io_equal (Block.track_at block 1) ((Block.D Color.blue), 10)
    io_equal (Block.track_at block 2) ((Block.T t1 overlay_ruler), 70)
-}


-- * setup

-- No tracks
empty_block = Block.Block "title" default_block_config
    (Block.R (Ruler.RulerId "")) []
empty_track = Track.track "track1" [] Color.white


default_ruler = mkruler 20 10
default_ruler_track = BlockC.R default_ruler

event_track_1 = Track.modify_events empty_track (Track.insert_events
    [event 0 "hi" 16, event 30 "there" 32])
event_track_2 = Track.modify_events empty_track (Track.insert_events
    [event 16 "ho" 10, event 30 "eyo" 32])

-- (10, 50) seems to be the smallest x,y os x will accept.  Apparently
-- fltk's sizes don't take the menu bar into account, which is about 44 pixels
-- high, so a y of 44 is the minimum.
default_rect = Block.Rect (10, 50) (100, 200)

major n = Ruler.Mark 1 3 (Color.rgba 0.45 0.27 0 0.35) (show n) 0 0
minor = Ruler.Mark 2 2 (Color.rgba 1 0.39 0.2 0.35) "" 0 0
marklist n dist = Ruler.marklist (take n $ zip (map TrackPos [0, dist ..]) m44)


m44 = concatMap (\n -> [major n, minor, minor, minor]) [0..]

ruler_bg = Color.rgb 1 0.85 0.5
mkruler marks dist = Ruler.Ruler [marklist marks dist] ruler_bg True False False
-- Convert a ruler config for an overlay ruler.
overlay_ruler ruler = ruler
    { Ruler.ruler_show_names = False
    , Ruler.ruler_use_alpha = True
    , Ruler.ruler_full_width = True
    }

default_block_config = Block.Config
    { Block.config_select_colors =
        let sel = Color.alpha 0.3 . Color.lighten 0.8 in
            [sel Color.blue, sel Color.green, sel Color.red]
    , Block.config_bg_color = Color.gray8
    , Block.config_track_box_color = Color.rgb 0.25 1 1
    , Block.config_sb_box_color = Color.rgb 0.25 1 1
    }

default_view_config = Block.ViewConfig
    { Block.vconfig_zoom_speed = 1
    , Block.vconfig_block_title_height = 20
    , Block.vconfig_track_title_height = 20
    , Block.vconfig_sb_size = 12
    , Block.vconfig_ruler_size = 18
    , Block.vconfig_status_size = 16
    }

track_bg = Color.white

text_style = TextStyle Helvetica [] 12 Color.black
