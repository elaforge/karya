-- | Directly exercise the BlockC functions.
module Ui.BlockC_test where
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as Exception

import qualified Util.Rect as Rect
import Util.Test

import qualified Ui.Color as Color
import qualified Ui.Track as Track
import qualified Ui.Types as Types
import qualified Ui.Ui as Ui

import qualified Ui.Block as Block
import qualified Ui.BlockC as BlockC
import qualified Ui.Symbol as Symbol
import qualified Ui.SymbolC as SymbolC

import qualified Ui.UiTest as UiTest

import qualified Perform.Signal as Signal


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
--     send $ BlockC.insert_track view 0 (Block.R ruler) [] 30
--     -- fltk errors
--     io_throws (BlockC.FltkError "tracknum out of bounds") $
--         send $ BlockC.set_track_width view 1 50
--     io_throws (BlockC.FltkError "can't set title of non-event track")
--         send $ BlockC.set_track_title 0 "hi ruler"
--     -- haskell errors
--     io_throws (BlockC.FltkError "view_id has no referent") $
--         send $ BlockC.set_track_width (Types.ViewId "bogus!") 50

test_create_set_size = do
    view <- create_empty_view
    io_human "move and change size" $
        BlockC.set_size view (Rect.xywh 200 200 200 200)
    io_human "view is destroyed" $
        send $ BlockC.destroy_view view

-- TODO
-- test_set_view_config

test_scroll_zoom = do
    view <- create_empty_view
    send $ BlockC.insert_track view 1 (event_track long_event_track) [] 200
    io_human "scroll a little to the right" $
        send $ BlockC.set_track_scroll view 10
    io_human "all the way to the right" $
        send $ BlockC.set_track_scroll view 100
    io_human "all the way back" $
        send $ BlockC.set_track_scroll view 0

    io_human "scroll down a little" $
        send $ BlockC.set_zoom view (Types.Zoom 20 1)
    io_human "scroll down all the way" $
        send $ BlockC.set_zoom view (Types.Zoom 99999 1)
    io_human "scroll back" $
        send $ BlockC.set_zoom view (Types.Zoom (-20) 1)

    io_human "zoom in to 2" $
        send $ BlockC.set_zoom view (Types.Zoom 0 2)
    io_human "zoom out back to 1" $
        send $ BlockC.set_zoom view (Types.Zoom 0 1)

test_set_selection = do
    view <- create_empty_view
    let ruler = UiTest.mkruler 20 10
    send $ BlockC.insert_track view 1
        (Block.T event_track_1 (UiTest.overlay_ruler ruler)) [] 30
    let c = Color.brightness 1.5 Color.blue
    io_human "point selection appears" $
        send $ BlockC.set_selection True view 0 (cselection c 1 0 1 0)
    io_human "replaced by long selection" $
        send $ BlockC.set_selection True view 0 (cselection c 1 10 1 20)
    io_human "goes away" $
        send $ BlockC.set_selection True view 0 Nothing

cselection color track start tracks dur =
    Just (BlockC.CSelection color (Types.Selection track start tracks dur))

test_set_track_width = do
    view <- create_empty_view
    send $ BlockC.insert_track view 1 (event_track event_track_1) [] 20
    send $ BlockC.insert_track view 2 (event_track event_track_2) [] 30
    io_human "track 1 gets bigger" $
        send $ BlockC.set_track_width view 1 60
    io_human "track 1 goes to minimum size" $
        send $ BlockC.set_track_width view 1 1

test_set_model_config = do
    view <- create_empty_view
    let config = Block.default_config

    -- block config
    io_human "track box turns red" $
        send $ BlockC.set_model_config view
            (config { Block.config_track_box = (Color.red, ' ') })
    io_human "sb box red, gets X" $
        send $ BlockC.set_model_config view
            (config { Block.config_sb_box = (Color.red, 'X') })
    io_human "block bg turns red" $
        send $ BlockC.set_model_config view
            (config { Block.config_bg_color = Color.red })

test_set_title = do
    view <- create_empty_view
    send $ BlockC.insert_track view 1 (event_track event_track_1) [] 20

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
    let ruler = UiTest.mkruler 20 10
    send $ BlockC.insert_track view 0 (Block.D UiTest.default_divider) [] 5
    send $ BlockC.insert_track view 1 (Block.R ruler) [] 30
    send $ BlockC.insert_track view 2
        (Block.T event_track_1 (UiTest.overlay_ruler ruler)) [] 30
    send $ BlockC.insert_track view 1 (Block.D UiTest.default_divider) [] 10

    io_human "ruler gets wider, both events change" $ do
        send $ BlockC.update_entire_track view 1
            (Block.R (UiTest.mkruler 20 16)) []
        send $ BlockC.update_track view 2
            (Block.T event_track_2 (UiTest.overlay_ruler ruler)) [] 0 60

test_insert_remove_track = do
    view <- create_empty_view
    let ruler = UiTest.mkruler 20 10
    io_human "new event track" $
        send $ BlockC.insert_track view 1
            (Block.T event_track_1 ruler) [] 30
    send $ BlockC.set_zoom view (Types.Zoom 0 2)
    io_human "another new event track also zoomed" $
        send $ BlockC.insert_track view 2 (Block.T event_track_2 ruler) [] 30
    io_human "remove ruler track, others move over" $
        send $ BlockC.remove_track view 0

test_track_signal = do
    view <- create_empty_view
    let track = Track.set_render_style Track.Line event_track_1
    io_human "track without signal" $
        send $ BlockC.insert_track view 1 (event_track track) [] 40

    let csig = Signal.signal [(0, 1), (32, 0.5), (64, 0), (500, 0), (510, 1)]
    let tsig = Track.TrackSignal (Track.Control csig) 0 1
    io_human "track gets signal" $
        send $ BlockC.set_track_signal view 1 tsig
    io_human "signal offset" $
        send $ BlockC.set_track_signal view 1 (tsig { Track.ts_shift = 4 })
    io_human "signal warp" $
        send $ BlockC.set_track_signal view 1 (tsig { Track.ts_stretch = 2 })

    -- have to put in a DEBUG print to see if the memory was freed
    io_human "track gone and signal memory freed" $
        send $ BlockC.remove_track view 1

-- This should really be in Ui.SymbolC_test, but I'm lazy.
test_symbols = do
    let sym = Symbol.Symbol "1^" True
            [ Symbol.glyph "1"
            , (Symbol.glyph "•") { Symbol.glyph_align = (0.2, -0.6) }
            ]

    let bad_font = Symbol.symbol "x"
            [(Symbol.glyph "y") { Symbol.glyph_font = Just "no such font" }]
    io_equal (SymbolC.insert_symbol bad_font) ["no such font"]
    io_equal (SymbolC.insert_symbol sym) []
    -- do it twice and make sure the memory from the first one is freed
    io_equal (SymbolC.insert_symbol sym) []
    view <- create_empty_view
    io_human "track with symbol" $
        send $ BlockC.insert_track view 1
            (event_track (UiTest.mktrack ("syms", [(0, 16, "`1^`")]))) [] 40

-- TODO
-- test_print_children

-- setup

event_track events = Block.T events UiTest.no_ruler

long_event_track = UiTest.mktrack
    ("long", [(0, 16, "hi"), (400, 32, "there")])
event_track_1 = UiTest.mktrack ("1", [(0, 16, "hi"), (30, 32, "there")])
event_track_2 = UiTest.mktrack ("2", [(16, 10, "ho"), (30, 32, "eyo")])

create_empty_view = do
    let view_id = Types.ViewId (UiTest.mkid "default")
    send $ BlockC.create_view view_id "some title" UiTest.default_rect
        Block.default_view_config Block.default_config
    return view_id
