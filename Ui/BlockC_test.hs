-- | Directly exercise the BlockC functions.
module Ui.BlockC_test where
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as Exception

import Util.Test

import qualified Ui.Ui as Ui
import Ui.Types
import qualified Ui.Color as Color

import qualified Ui.Block as Block
import qualified Ui.BlockC as BlockC
import qualified Ui.Track as Track

import Ui.TestSetup


initialize f = do
    quit_request <- Concurrent.newMVar ()
    msg_chan <- STM.newTChanIO
    Concurrent.forkIO (f `Exception.finally` Ui.quit_ui_thread quit_request)
    Ui.event_loop quit_request msg_chan
send = Ui.send_action

-- tests

-- TODO
-- test_errors = do
--     view <- create_empty_view
--     send $ BlockC.insert_track view 0 (Block.R ruler) 30
--     -- fltk errors
--     io_throws (BlockC.FltkError "tracknum out of bounds") $
--         send $ BlockC.set_track_width view 1 50
--     io_throws (BlockC.FltkError "can't set title of non-event track")
--         send $ BlockC.set_track_title 0 "hi ruler"
--     -- haskell errors
--     io_throws (BlockC.FltkError "view_id has no referent") $
--         send $ BlockC.set_track_width (Block.ViewId "bogus!") 50

test_create_set_size = do
    view <- create_empty_view
    io_equal (BlockC.get_size view) default_rect
    io_human "move and change size" $
        BlockC.set_size view (Block.Rect (200, 200) (200, 200))
    io_equal (BlockC.get_size view) (Block.Rect (200, 200) (200, 200))
    io_human "view is destroyed" $ do
        send $ BlockC.destroy_view view

-- TODO
-- test_set_view_config

test_scroll_zoom = do
    view <- create_empty_view
    send $ BlockC.insert_track view 0 (Block.T long_event_track no_ruler) 200
    io_human "scroll a little to the right" $
        send $ BlockC.set_track_scroll view 10
    io_human "all the way to the right" $
        send $ BlockC.set_track_scroll view 100
    io_human "all the way back" $
        send $ BlockC.set_track_scroll view 0

    io_human "scroll down a little" $
        send $ BlockC.set_zoom view (Block.Zoom (TrackPos 20) 1)
    io_human "scroll down all the way" $
        send $ BlockC.set_zoom view (Block.Zoom (TrackPos 99999) 1)
    io_human "scroll back" $
        send $ BlockC.set_zoom view (Block.Zoom (TrackPos (-10)) 1)
    -- TODO test zoom when it's implemented

test_set_selection = do
    view <- create_empty_view
    let ruler = mkruler 20 10
    send $ BlockC.insert_track view 0
        (Block.T event_track_1 (overlay_ruler ruler)) 30
    let c = Color.lighten 0.5 Color.blue
    io_human "point selection appears" $
        send $ BlockC.set_selection view 0
            (cselection c 0 (TrackPos 0) 1 (TrackPos 0))
    io_human "replaced by long selection" $
        send $ BlockC.set_selection view 0
            (cselection c 0 (TrackPos 10) 1 (TrackPos 20))
    io_human "goes away" $
        send $ BlockC.set_selection view 0 Nothing

cselection color track start tracks dur =
    Just (BlockC.CSelection color (Block.Selection track start tracks dur))

test_set_track_width = do
    view <- create_empty_view
    send $ BlockC.insert_track view 0 (Block.T event_track_1 no_ruler) 20
    send $ BlockC.insert_track view 1 (Block.T event_track_2 no_ruler) 30
    io_human "track 0 gets bigger" $
        send $ BlockC.set_track_width view 0 60
    io_human "track 0 goes to minimum size" $
        send $ BlockC.set_track_width view 0 1

test_set_model_config = do
    view <- create_empty_view
    let config = default_block_config

    -- block config
    io_human "track box turns red" $
        send $ BlockC.set_model_config view
            (config { Block.config_track_box_color = Color.red })
    io_human "sb box red" $
        send $ BlockC.set_model_config view
            (config { Block.config_sb_box_color = Color.red })
    io_human "block bg turns red" $
        send $ BlockC.set_model_config view
            (config { Block.config_bg_color = Color.red })

test_set_title = do
    view <- create_empty_view
    send $ BlockC.insert_track view 0 (Block.T event_track_1 no_ruler) 20

    io_human "block gets hi title" $
        send $ BlockC.set_title view "hi"
    io_human "block title cleared" $
        send $ BlockC.set_title view ""

    io_human "track title set" $
        send $ BlockC.set_track_title view 0 "ho"
    io_human "track title cleared" $
        send $ BlockC.set_track_title view 0 ""

test_create_remove_update_track = do
    view <- create_empty_view
    let ruler = mkruler 20 10
    send $ BlockC.insert_track view 0 (Block.R ruler) 30
    send $ BlockC.insert_track view 1
        (Block.T event_track_1 (overlay_ruler ruler)) 30
    send $ BlockC.insert_track view 0 (Block.D default_divider) 10

    io_human "divider is removed" $
        send $ BlockC.remove_track view 0
    io_human "ruler gets wider, both events change" $ do
        send $ BlockC.update_track view 0
            (Block.R (mkruler 20 16))
            (TrackPos 0) (TrackPos 60)
        send $ BlockC.update_track view 1
            (Block.T event_track_2 (overlay_ruler ruler))
            (TrackPos 0) (TrackPos 60)

-- TODO
-- test_print_children

-- setup

long_event_track = Track.modify_events (empty_track "long")
    (Track.insert_events [eventpos 0 "hi" 16, eventpos 400 "there" 32])

create_empty_view = do
    let view_id = Block.ViewId "default"
    send $ BlockC.create_view view_id "some title" default_rect
        default_view_config default_block_config (Block.R default_ruler)
    return view_id
