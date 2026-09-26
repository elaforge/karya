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
        quitRequest <- Concurrent.newMVar ()
        msgChan <- STM.newTChanIO
        LoadConfig.styles Config.styles
        Concurrent.forkIO $
            test `Exception.finally` Fltk.quitUiThread quitRequest
        Fltk.eventLoop globalUiChannel quitRequest msgChan
    , Testing.tags = [Testing.Interactive]
    }

globalUiChannel :: Fltk.Channel
{-# NOINLINE globalUiChannel #-}
globalUiChannel = Unsafe.unsafePerformIO (MVar.newMVar [])

send :: Fltk.Fltk () -> IO ()
send = Fltk.sendAction globalUiChannel "test"

-- tests

test_create_set_size :: Test
test_create_set_size = do
    view <- createEmptyView
    ioHuman "move and change size" $
        send $ BlockC.setSize view (Rect.xywh 200 200 200 200)
    ioHuman "view is destroyed" $
        send $ BlockC.destroyView view

test_scroll_zoom :: Test
test_scroll_zoom = do
    view <- createEmptyView
    insertTrack view 1 (eventTrack longEventTrack) 200
    ioHuman "scroll a little to the right" $
        send $ BlockC.setTrackScroll view 10
    ioHuman "all the way to the right" $
        send $ BlockC.setTrackScroll view 100
    ioHuman "all the way back" $
        send $ BlockC.setTrackScroll view 0

    ioHuman "scroll down a little" $
        send $ BlockC.setZoom view (Zoom.Zoom 20 1)
    ioHuman "scroll down all the way" $
        send $ BlockC.setZoom view (Zoom.Zoom 99999 1)
    ioHuman "scroll back" $
        send $ BlockC.setZoom view (Zoom.Zoom (-20) 1)

    ioHuman "zoom in to 2" $
        send $ BlockC.setZoom view (Zoom.Zoom 0 2)
    ioHuman "zoom out back to 1" $
        send $ BlockC.setZoom view (Zoom.Zoom 0 1)

test_set_selection :: Test
test_set_selection = do
    view <- createEmptyView
    let ruler = mkruler 20 10
    insertTrack view 1 (Block.T eventTrack1 ruler) 30
    let c = Color.brightness 1.5 Color.blue
        set = BlockC.setSelection view 0 [1]
    ioHuman "point selection appears" $
        send $ set [BlockC.Selection c 1 1 BlockC.Positive]
    ioHuman "replaced by long selection" $
        send $ set [BlockC.Selection c 10 20 BlockC.Positive]
    ioHuman "lots of pretty colors" $ send $ set
        [ BlockC.Selection Color.blue 1 10 BlockC.Positive
        , BlockC.Selection Color.red 20 40 BlockC.Negative
        , BlockC.Selection Color.green 45 45 BlockC.Positive
        ]
    ioHuman "goes away" $ send $ set []

test_set_config :: Test
test_set_config = do
    view <- createEmptyView
    let config = Block.defaultConfig

    -- block config
    ioHuman "track box turns red" $
        send $ BlockC.setConfig view
            (config { Block.configTrackBox = Block.Box Color.red ' ' })
    ioHuman "sb box red, gets 中" $
        send $ BlockC.setConfig view
            (config { Block.configSbBox = Block.Box Color.red '中' })

test_set_title :: Test
test_set_title = do
    view <- createEmptyView
    insertTrack view 1 (eventTrack eventTrack1) 20

    ioHuman "block gets hi title" $
        send $ BlockC.setTitle view "hi"
    ioHuman "block title cleared" $
        send $ BlockC.setTitle view ""

    ioHuman "track title set" $
        send $ BlockC.setTrackTitle view 1 "ho"
    ioHuman "track title cleared" $
        send $ BlockC.setTrackTitle view 1 ""

test_update_track :: Test
test_update_track = do
    view <- createEmptyView
    let ruler = mkruler 20 10
    insertTrack view 0 (Block.D UiTest.defaultDivider) 5
    insertTrack view 1 (Block.R ruler) 30
    insertTrack view 2 (Block.T eventTrack1 ruler) 30

    ioHuman "ruler gets wider, both events change" $ do
        send $ BlockC.updateEntireTrack True view 1
            (Block.R (mkruler 20 16)) [] setStyle
        send $ BlockC.updateTrack True view 2
            (Block.T eventTrack2 ruler) [] setStyle 0 60

test_insert_remove_track :: Test
test_insert_remove_track = do
    view <- createEmptyView
    let ruler = mkruler 20 10
    ioHuman "new event track" $
        insertTrack view 1 (Block.T eventTrack1 ruler) 30
    send $ BlockC.setZoom view (Zoom.Zoom 0 2)
    ioHuman "another new event track also zoomed" $
        insertTrack view 2 (Block.T eventTrack2 ruler) 30
    ioHuman "remove ruler track, others move over" $
        send $ BlockC.removeTrack view 0

test_track_signal :: Test
test_track_signal = do
    view <- createEmptyView
    let track = Track.setRenderStyle (Track.Line Nothing) eventTrack1
    ioHuman "track without signal" $
        insertTrack view 1 (eventTrack track) 40

    let csig = Signal.fromPairs
            [(0, 1), (32, 0.5), (64, 0), (500, 0), (510, 1)]
    let tsig = Track.TrackSignal csig 0 1
    ioHuman "track gets signal" $
        send $ BlockC.setTrackSignal view 1 tsig
    ioHuman "signal offset" $
        send $ BlockC.setTrackSignal view 1 (tsig { Track.tsShift = 4 })
    ioHuman "signal warp" $
        send $ BlockC.setTrackSignal view 1 (tsig { Track.tsStretch = 2 })

    -- have to put in a DEBUG print to see if the memory was freed
    ioHuman "track gone and signal memory freed" $
        send $ BlockC.removeTrack view 1

-- This should really be in Ui.SymbolC_test, but I'm lazy.
test_symbols :: Test
test_symbols = do
    let sym = Symbol.Symbol "1^" True
            [ Symbol.glyph "1"
            , (Symbol.glyph "•") { Symbol.glyphAlign = (0.2, -0.6) }
            ]

    let badFont = Symbol.symbol "x"
            [(Symbol.glyph "y") { Symbol.glyphFont = Just "no such font" }]
    ioEqual (SymbolC.insert badFont) ["no such font"]
    ioEqual (SymbolC.insert sym) []
    -- do it twice and make sure the memory from the first one is freed
    ioEqual (SymbolC.insert sym) []
    view <- createEmptyView
    ioHuman "track with symbol" $
        insertTrack view 1
            (eventTrack (UiTest.makeTrack ("syms", [(0, 16, "`1^`")]))) 40

test_set_ruler_width :: Test
test_set_ruler_width = do
    viewId <- createEmptyView
    ioHuman "insert wide then narrow rulers" $ do
        -- 50 width ruler bumps over the 10 width ruler.
        insertTrack viewId 0 (Block.R (mkruler 10 10)) 10
        insertTrack viewId 0 (Block.R (mkruler 10 5)) 50
    pure ()

-- TODO
-- test_print_children

-- * setup

setStyle :: Track.SetStyle
setStyle = (Track.trackBg, \_ event -> Event.style event)

mkruler :: Int -> ScoreTime -> Ruler.Ruler
mkruler = UiTest.mkruler44

insertTrack :: ViewId -> TrackNum -> Block.Tracklike -> Types.Width -> IO ()
insertTrack view tracknum tracklike width = send $
    BlockC.insertTrack view tracknum tracklike [] setStyle width

eventTrack :: Track.Track -> Block.Tracklike
eventTrack events = Block.T events Ruler.empty

longEventTrack, eventTrack1, eventTrack2 :: Track.Track
longEventTrack = UiTest.makeTrack
    ("long", [(0, 16, "hi"), (400, 32, "there")])
eventTrack1 = UiTest.makeTrack ("1", [(0, 16, "hi"), (30, 32, "there")])
eventTrack2 = UiTest.makeTrack ("2", [(16, 10, "ho"), (30, 32, "eyo")])

createEmptyView :: IO ViewId
createEmptyView = do
    let viewId = Id.ViewId (UiTest.mkid "default")
    send $ BlockC.createView viewId "some title" UiTest.defaultRect
        Block.defaultConfig
    pure viewId
