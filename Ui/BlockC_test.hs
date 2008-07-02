-- | Directly exercise the BlockC functions.
module Ui.BlockC_test where
import qualified Control.Arrow as Arrow
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

import qualified Ui.TestSetup as TestSetup


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
    io_human "move and change size" $
        BlockC.set_size view (Block.Rect (200, 200) (200, 200))
    io_human "view is destroyed" $
        send $ BlockC.destroy_view view

-- TODO
-- test_set_view_config

test_scroll_zoom = do
    view <- create_empty_view
    send $ BlockC.insert_track view 1 (event_track long_event_track)
        no_samples 200
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
        send $ BlockC.set_zoom view (Block.Zoom (TrackPos (-20)) 1)

    io_human "zoom in to 2" $
        send $ BlockC.set_zoom view (Block.Zoom (TrackPos 0) 2)
    io_human "zoom out back to 1" $
        send $ BlockC.set_zoom view (Block.Zoom (TrackPos 0) 1)

test_set_selection = do
    view <- create_empty_view
    let ruler = TestSetup.mkruler 20 10
    send $ BlockC.insert_track view 1
        (Block.T event_track_1 (TestSetup.overlay_ruler ruler)) no_samples 30
    let c = Color.lighten 0.5 Color.blue
    io_human "point selection appears" $
        send $ BlockC.set_selection view 0
            (cselection c 1 (TrackPos 0) 1 (TrackPos 0))
    io_human "replaced by long selection" $
        send $ BlockC.set_selection view 0
            (cselection c 1 (TrackPos 10) 1 (TrackPos 20))
    io_human "goes away" $
        send $ BlockC.set_selection view 0 Nothing

cselection color track start tracks dur =
    Just (BlockC.CSelection color (Block.Selection track start tracks dur))

test_set_track_width = do
    view <- create_empty_view
    send $ BlockC.insert_track view 1 (event_track event_track_1) no_samples 20
    send $ BlockC.insert_track view 2 (event_track event_track_2) no_samples 30
    io_human "track 1 gets bigger" $
        send $ BlockC.set_track_width view 1 60
    io_human "track 1 goes to minimum size" $
        send $ BlockC.set_track_width view 1 1

test_set_model_config = do
    view <- create_empty_view
    let config = TestSetup.default_block_config

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
    send $ BlockC.insert_track view 1 (event_track event_track_1) no_samples 20

    io_human "block gets hi title" $
        send $ BlockC.set_title view "hi"
    io_human "block title cleared" $
        send $ BlockC.set_title view ""

    io_human "track title set" $
        send $ BlockC.set_track_title view 1 "ho"
    io_human "track title cleared" $
        send $ BlockC.set_track_title view 1 ""

test_update_track = do
    view <- create_empty_view
    let ruler = TestSetup.mkruler 20 10
    send $ BlockC.insert_track view 0 (Block.D TestSetup.default_divider)
        no_samples 5
    send $ BlockC.insert_track view 1 (Block.R ruler) no_samples 30
    send $ BlockC.insert_track view 2
        (Block.T event_track_1 (TestSetup.overlay_ruler ruler)) no_samples 30
    send $ BlockC.insert_track view 1 (Block.D TestSetup.default_divider)
        no_samples 10

    io_human "ruler gets wider, both events change" $ do
        send $ BlockC.update_entire_track view 1
            (Block.R (TestSetup.mkruler 20 16)) no_samples
        send $ BlockC.update_track view 2
            (Block.T event_track_2 (TestSetup.overlay_ruler ruler))
            no_samples (TrackPos 0) (TrackPos 60)

test_insert_remove_track = do
    view <- create_empty_view
    let ruler = TestSetup.mkruler 20 10
    io_human "new event track" $
        send $ BlockC.insert_track view 1
            (Block.T event_track_1 ruler) no_samples 30
    send $ BlockC.set_zoom view (Block.Zoom (TrackPos 0) 2)
    io_human "another new event track also zoomed" $
        send $ BlockC.insert_track view 2
            (Block.T event_track_2 ruler) no_samples 30

test_samples = do
    view <- create_empty_view
    let track = Track.set_render_style Track.Line event_track_1
    io_human "track with samples" $
        send $ BlockC.insert_track view 1 (event_track track) samples 40

    let track2 = Track.set_render_style Track.Filled event_track_1
    io_human "samples are filled in" $
        send $ BlockC.update_entire_track view 1 (event_track track2) samples

samples = Track.samples $ map (Arrow.first TrackPos)
    [(0, 1), (32, 0.5), (32, 1), (64, 0), (500, 0), (510, 1), (520, 0)]

-- TODO
-- test_print_children

-- setup

event_track events = Block.T events TestSetup.no_ruler

long_event_track = TestSetup.mktrack
    ("long", [(0, 16, "hi"), (400, 32, "there")])
event_track_1 = TestSetup.mktrack ("1", [(0, 16, "hi"), (30, 32, "there")])
event_track_2 = TestSetup.mktrack ("2", [(16, 10, "ho"), (30, 32, "eyo")])

create_empty_view = do
    let view_id = Block.ViewId "default"
    send $ BlockC.create_view view_id "some title" TestSetup.default_rect
        TestSetup.default_view_config TestSetup.default_block_config
    return view_id

no_samples = Track.samples []
