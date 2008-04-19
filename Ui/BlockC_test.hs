-- | Directly exercise the BlockC functions.
module Ui.BlockC_test where

import Util.Test

import qualified Ui.Initialize as Initialize
import Ui.Types
import qualified Ui.Color as Color

import qualified Ui.Block as Block
import qualified Ui.BlockC as BlockC

import Ui.TestSetup


initialize f = Initialize.initialize $ \_msg_chan -> f
send = Initialize.send_action

-- tests

test_set_size = do
    view <- create_empty_view
    io_equal (BlockC.get_size view) default_rect
    io_human "move and change size" $
        BlockC.set_size view (Block.Rect (200, 200) (200, 200))
    io_equal (BlockC.get_size view) (Block.Rect (200, 200) (200, 200))

-- test_set_view_config

test_scroll_zoom = do
    view <- create_empty_view
    send $ BlockC.insert_track view 0 (BlockC.R default_ruler) 100
    io_human "scroll a little to the right" $
        send $ BlockC.set_track_scroll view 10
    io_human "all the way to the right" $
        send $ BlockC.set_track_scroll view 100
    io_human "all the way back" $
        send $ BlockC.set_track_scroll view 0
    -- test zoom and time scroll

test_set_selection = do
    view <- create_empty_view
    let ruler = mkruler 20 10
    send $ BlockC.insert_track view 0
        (BlockC.T event_track_1 (overlay_ruler ruler)) 30
    let c = Color.lighten 0.5 Color.blue
    io_human "point selection appears" $
        send $ BlockC.set_selection view 0
            (Just (Block.Selection c 0 (TrackPos 0) 1 (TrackPos 0)))
    io_human "replaced by long selection" $
        send $ BlockC.set_selection view 0
            (Just (Block.Selection c 0 (TrackPos 10) 1 (TrackPos 20)))
    io_human "goes away" $
        send $ BlockC.set_selection view 0 Nothing

-- test_set_track_width

-- test_set_model_config
-- test_set_title

test_create_remove_update_track = do
    view <- create_empty_view
    let ruler = mkruler 20 10
    send $ BlockC.insert_track view 0 (BlockC.R ruler) 30
    send $ BlockC.insert_track view 1
        (BlockC.T event_track_1 (overlay_ruler ruler)) 30
    -- TODO test remove_track
    io_human "ruler gets wider, both events change" $ do
        send $ BlockC.update_track view 0
            (BlockC.R (mkruler 20 16))
            (TrackPos 0) (TrackPos 60)
        send $ BlockC.update_track view 1
            (BlockC.T event_track_2 (overlay_ruler ruler))
            (TrackPos 0) (TrackPos 60)
    send $ BlockC.destroy_view view

-- setup

create_empty_view = do
    let view_id = Block.ViewId "default"
    send $ BlockC.create_view view_id default_rect
        default_view_config default_block_config (BlockC.R default_ruler)
    return view_id
