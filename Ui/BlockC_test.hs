-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Directly exercise the BlockC functions.
module Ui.BlockC_test where
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as Exception

import qualified System.IO.Unsafe as Unsafe

import qualified Util.Rect as Rect
import Util.Test
import qualified Util.Test.Testing as Testing

import qualified Ui.Block as Block
import qualified Ui.BlockC as BlockC
import qualified Ui.Color as Color
import qualified Ui.Event as Event
import qualified Ui.Fltk as Fltk
import qualified Ui.Id as Id
import qualified Ui.Ruler as Ruler
import qualified Ui.Symbol as Symbol
import qualified Ui.SymbolC as SymbolC
import qualified Ui.Track as Track
import qualified Ui.Types as Types
import qualified Ui.UiTest as UiTest
import qualified Ui.Zoom as Zoom

import qualified Perform.Signal as Signal
import qualified App.Config as Config
import qualified App.LoadConfig as LoadConfig
import Types


meta = Testing.moduleMeta
    { Testing.initialize = \test -> do
        quit_request <- Concurrent.newMVar ()
        msg_chan <- STM.newTChanIO
        LoadConfig.styles Config.styles
        Concurrent.forkIO $
            test `Exception.finally` Fltk.quit_ui_thread quit_request
        Fltk.event_loop global_ui_channel quit_request msg_chan
    , Testing.tags = [Testing.Interactive]
    }

global_ui_channel :: Fltk.Channel
{-# NOINLINE global_ui_channel #-}
global_ui_channel = Unsafe.unsafePerformIO (MVar.newMVar [])

send :: Fltk.Fltk () -> IO ()
send = Fltk.send_action global_ui_channel "test"

-- tests

test_create_set_size = do
    view <- create_empty_view
    io_human "move and change size" $
        send $ BlockC.set_size view (Rect.xywh 200 200 200 200)
    io_human "view is destroyed" $
        send $ BlockC.destroy_view view

test_scroll_zoom = do
    view <- create_empty_view
    insert_track view 1 (event_track long_event_track) 200
    io_human "scroll a little to the right" $
        send $ BlockC.set_track_scroll view 10
    io_human "all the way to the right" $
        send $ BlockC.set_track_scroll view 100
    io_human "all the way back" $
        send $ BlockC.set_track_scroll view 0

    io_human "scroll down a little" $
        send $ BlockC.set_zoom view (Zoom.Zoom 20 1)
    io_human "scroll down all the way" $
        send $ BlockC.set_zoom view (Zoom.Zoom 99999 1)
    io_human "scroll back" $
        send $ BlockC.set_zoom view (Zoom.Zoom (-20) 1)

    io_human "zoom in to 2" $
        send $ BlockC.set_zoom view (Zoom.Zoom 0 2)
    io_human "zoom out back to 1" $
        send $ BlockC.set_zoom view (Zoom.Zoom 0 1)

test_set_selection = do
    view <- create_empty_view
    let ruler = mkruler 20 10
    insert_track view 1 (Block.T event_track_1 ruler) 30
    let c = Color.brightness 1.5 Color.blue
        set = BlockC.set_selection view 0 [1]
    io_human "point selection appears" $
        send $ set [BlockC.Selection c 1 1 BlockC.Positive]
    io_human "replaced by long selection" $
        send $ set [BlockC.Selection c 10 20 BlockC.Positive]
    io_human "lots of pretty colors" $ send $ set
        [ BlockC.Selection Color.blue 1 10 BlockC.Positive
        , BlockC.Selection Color.red 20 40 BlockC.Negative
        , BlockC.Selection Color.green 45 45 BlockC.Positive
        ]
    io_human "goes away" $ send $ set []

test_set_config = do
    view <- create_empty_view
    let config = Block.default_config

    -- block config
    io_human "track box turns red" $
        send $ BlockC.set_config view
            (config { Block.config_track_box = Block.Box Color.red ' ' })
    io_human "sb box red, gets 中" $
        send $ BlockC.set_config view
            (config { Block.config_sb_box = Block.Box Color.red '中' })

test_set_title = do
    view <- create_empty_view
    insert_track view 1 (event_track event_track_1) 20

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
    let ruler = mkruler 20 10
    insert_track view 0 (Block.D UiTest.default_divider) 5
    insert_track view 1 (Block.R ruler) 30
    insert_track view 2 (Block.T event_track_1 ruler) 30

    io_human "ruler gets wider, both events change" $ do
        send $ BlockC.update_entire_track True view 1
            (Block.R (mkruler 20 16)) [] set_style
        send $ BlockC.update_track True view 2
            (Block.T event_track_2 ruler) [] set_style 0 60

test_insert_remove_track = do
    view <- create_empty_view
    let ruler = mkruler 20 10
    io_human "new event track" $
        insert_track view 1 (Block.T event_track_1 ruler) 30
    send $ BlockC.set_zoom view (Zoom.Zoom 0 2)
    io_human "another new event track also zoomed" $
        insert_track view 2 (Block.T event_track_2 ruler) 30
    io_human "remove ruler track, others move over" $
        send $ BlockC.remove_track view 0

test_track_signal = do
    view <- create_empty_view
    let track = Track.set_render_style (Track.Line Nothing) event_track_1
    io_human "track without signal" $
        insert_track view 1 (event_track track) 40

    let csig = Signal.from_pairs
            [(0, 1), (32, 0.5), (64, 0), (500, 0), (510, 1)]
    let tsig = Track.TrackSignal csig 0 1
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
    io_equal (SymbolC.insert bad_font) ["no such font"]
    io_equal (SymbolC.insert sym) []
    -- do it twice and make sure the memory from the first one is freed
    io_equal (SymbolC.insert sym) []
    view <- create_empty_view
    io_human "track with symbol" $
        insert_track view 1
            (event_track (UiTest.make_track ("syms", [(0, 16, "`1^`")]))) 40

test_set_ruler_width = do
    view_id <- create_empty_view
    io_human "insert wide then narrow rulers" $ do
        -- 50 width ruler bumps over the 10 width ruler.
        insert_track view_id 0 (Block.R (mkruler 10 10)) 10
        insert_track view_id 0 (Block.R (mkruler 10 5)) 50
    return ()

-- TODO
-- test_print_children

-- * setup

set_style :: Track.SetStyle
set_style = (Track.track_bg, \_ event -> Event.style event)

mkruler :: Int -> ScoreTime -> Ruler.Ruler
mkruler = UiTest.mkruler_44

insert_track :: ViewId -> TrackNum -> Block.Tracklike -> Types.Width -> IO ()
insert_track view tracknum tracklike width = send $
    BlockC.insert_track view tracknum tracklike [] set_style width

event_track :: Track.Track -> Block.Tracklike
event_track events = Block.T events Ruler.empty_ruler

long_event_track, event_track_1, event_track_2 :: Track.Track
long_event_track = UiTest.make_track
    ("long", [(0, 16, "hi"), (400, 32, "there")])
event_track_1 = UiTest.make_track ("1", [(0, 16, "hi"), (30, 32, "there")])
event_track_2 = UiTest.make_track ("2", [(16, 10, "ho"), (30, 32, "eyo")])

create_empty_view :: IO ViewId
create_empty_view = do
    let view_id = Id.ViewId (UiTest.mkid "default")
    send $ BlockC.create_view view_id "some title" UiTest.default_rect
        Block.default_config
    return view_id
