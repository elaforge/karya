-- | In addition to Sync, this tests the whole State -> Diff -> Sync -> BlockC
-- sequence.
--
-- Sync is hard to test functionally because its whole point is to run IO
-- actions.
--
-- It's also hard to test Diff functionally.  I could test that the expected
-- Updates are emitted, but that doesn't mean much; what I really care about is
-- that the UI is updated in the expected manner.  I could build a model of the
-- UI and apply the Updates to that, but it would be complicated and have bugs
-- itself.
--
-- Unless I can think of a better way to do these tests, they will
-- unfortunately remain io_human tests in here.
module Ui.Sync_test where
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as Exception

import Util.Test

import Ui.Types
import qualified Ui.Ui as Ui
import qualified Ui.Block as Block
import qualified Ui.Track as Track
import qualified Ui.Ruler as Ruler
import qualified Ui.Event as Event

import qualified Ui.Color as Color
import qualified Ui.State as State
import qualified Ui.Diff as Diff
import qualified Ui.Sync as Sync

import Ui.TestSetup

-- TODO
-- test_error
-- set_block_title
-- set_block_config
-- set_block_ruler

initialize f = do
    quit_request <- Concurrent.newMVar ()
    msg_chan <- STM.newTChanIO
    Concurrent.forkIO (f `Exception.finally` Ui.quit_ui_thread quit_request)
    Ui.event_loop quit_request msg_chan

test_create_resize_destroy_view = do
    state <- io_human "view with selection and titles" $ run State.empty $ do
        v1 <- setup_state
        State.set_selection v1 0 (Block.point_selection 0 (TrackPos 20))
        view <- State.get_view v1
        State.set_block_title (Block.view_block view) "new block!"
        State.set_track_title t_track1_id "new track"
    state <- io_human "view moves over, gets bigger" $ run state $ do
        State.set_view_rect t_view_id (Block.Rect (400, 400) (400, 400))
    io_human "view is destroyed" $ run state $ do
        State.destroy_view t_view_id
    return ()

test_create_two_views = do
    state <- run_setup
    state <- io_human "another view created" $ run state $ do
        -- A new view is created, and a track that is in both is modified.
        b2 <- State.create_block "b2" $ Block.block ""
            default_block_config
            [(Block.RId t_ruler_id, 20), (Block.TId t_track1_id t_ruler_id, 30)]
            t_schema_id
        v2 <- State.create_view "v2" $
            Block.view b2 default_rect default_view_config
        State.set_track_title t_track1_id "hi there"
    return ()

test_zoom_scroll = do
    state <- run State.empty $ do
        v1 <- setup_state
        State.insert_events t_track1_id
            [ (TrackPos 0, event "one" 10)
            , (TrackPos 10, event "scrunch" 6)
            , (TrackPos 20, event "two" 32)
            , (TrackPos 100, event "last" 64)
            ]
    state <- io_human "scrolls to bottom" $ run state $ do
        State.set_zoom t_view_id (Block.Zoom (TrackPos 128) 1)
    state <- io_human "scrolls back up" $ run state $ do
        State.set_zoom t_view_id (Block.Zoom (TrackPos 0) 1)
    state <- io_human "zoom in to 2" $ run state $ do
        State.set_zoom t_view_id (Block.Zoom (TrackPos 0) 2)
    state <- io_human "zoom out to .5" $ run state $ do
        State.set_zoom t_view_id (Block.Zoom (TrackPos 0) 0.5)
    state <- io_human "zoom out to 0, should clamp at a low number" $
        run state $ State.set_zoom t_view_id (Block.Zoom (TrackPos 0) 0)
    return ()

test_set_status = do
    state <- run_setup
    state <- io_human "status set" $ run state $ do
        State.set_view_status t_view_id "lol" (Just "o hai")
        State.set_view_status t_view_id "brick" (Just "krazy")
    state <- io_human "'lol' status cleared" $ run state $ do
        State.set_view_status t_view_id "lol" Nothing
    return ()

test_insert_remove_track = do
    state <- run_setup
    state <- io_human "new wide track at the end" $ run state $ do
        State.insert_track t_block_id 2 (Block.TId t_track1_id t_ruler_id) 80
    state <- io_human "first track replaced by divider" $ run state $ do
        State.remove_track t_block_id 1
        State.insert_track t_block_id 1 (Block.DId default_divider) 5
    return ()

test_update_track = do
    -- sync to initial state
    state <- io_human "create view with one track with two events" run_setup

    -- add a track, change the old track's width
    state <- io_human "add events, get wider, turn green" $ run state $ do
        State.insert_events t_track1_id [(TrackPos 70, event "last1" 10),
            (TrackPos 90, event "last2" 15)]
        State.set_track_width t_view_id 0 50
        State.set_track_bg t_track1_id Color.green
    return ()

test_update_track2 = do
    state <- run State.empty $ do
        v1 <- setup_state
        t2 <- State.create_track "b1.t2" event_track_2
        State.insert_track t_block_id 1 (Block.TId t2 t_ruler_id) 30
        return ()
    io_human "1st track deleted, 2nd track gets wider" $ run state $ do
        State.remove_track t_block_id 0
        State.set_track_width t_view_id 0 100
    return ()

test_create_track = do
    state <- run_setup
    let msg = "new track with selectio and new title, all bgs green"
    state <- io_human msg $ run state $ do
        State.insert_track t_block_id 1 (Block.TId t_track1_id t_ruler_id) 50
        State.set_selection t_view_id 0
            (Block.selection 1 (TrackPos 10) 1 (TrackPos 60))
        State.set_track_title t_track1_id "new track"
        State.set_track_bg t_track1_id Color.green
    return ()

test_selection = do
    state <- run_setup
    state <- io_human "selection is set" $ run state $ do
        State.set_selection t_view_id 0
            (Block.selection 0 (TrackPos 10) 1 (TrackPos 20))
    state <- io_human "selection is cleared" $ run state $ do
        State.set_selection t_view_id 0
            (Block.selection 0 (TrackPos 10) 0 (TrackPos 20))
    return ()


cues_marklist = Ruler.marklist "cues"
    [ (TrackPos 0, mark "start")
    , (TrackPos 90, mark "head explodes")
    ]

test_modify_ruler = do
    state <- run State.empty $ do
        setup_state
        State.insert_track t_block_id 2 (Block.RId t_ruler_id) 30
    state <- io_human "add head-explodes to all rulers" $ run state $ do
        State.insert_marklist t_ruler_id 1 cues_marklist
    state <- io_human "meter goes away" $ run state $ do
        State.remove_marklist t_ruler_id 0
    return ()

-- | Selection is correct even when tracks are added or deleted.
test_selection_change_tracks = do
    state <- run_setup
    state <- run state $ do
        State.set_selection t_view_id 0
            (Block.selection 0 (TrackPos 10) 1 (TrackPos 20))
    state <- io_human "sel moves when new track is added" $ run state $ do
        State.insert_track t_block_id 0 (Block.TId t_track1_id t_ruler_id) 40
    state <- io_human "sel moves back" $ run state $ do
        State.remove_track t_block_id 0
    return ()

test_insert_into_selection = do
    state <- run State.empty $ do
        v1 <- setup_state
        t2 <- State.create_track "b1.t2" event_track_2
        State.insert_track t_block_id 1 (Block.TId t2 t_ruler_id) 60
        t3 <- State.create_track "b1.t3" event_track_2
        State.insert_track t_block_id 2 (Block.TId t2 t_ruler_id) 60
        State.set_selection v1 0
            (Block.selection 0 (TrackPos 10) 2 (TrackPos 60))
    state <- io_human "insert into sel, gets bigger" $ run state $ do
        State.insert_track t_block_id 1 (Block.TId t_track1_id t_ruler_id) 20
    state <- io_human "remove from sel, gets smaller" $ run state $ do
        State.remove_track t_block_id 1
    return ()

-- * util

t_ruler_id = Ruler.RulerId "r1"
t_block_id = Block.BlockId "b1"
t_track1_id = Track.TrackId "b1.t1"
t_view_id = Block.ViewId "v1"
t_schema_id = Block.SchemaId "no schema"

run_setup = run State.empty setup_state
setup_state = do
    ruler <- State.create_ruler "r1" (mkruler 20 10)
    t1 <- State.create_track "b1.t1" (empty_track "t1")
    b1 <- State.create_block "b1" $
        Block.block "hi b1" default_block_config
            [(Block.RId ruler, 20), (Block.TId t1 ruler, 30)]
            t_schema_id
    State.create_view "v1" (Block.view b1 default_rect default_view_config)

run st1 m = do
    res <- State.run st1 m
    let (_val, st2, updates) = right res
    sync st1 st2 updates
    return st2

sync st1 st2 hint_updates = do
    let updates = right $ Diff.diff st1 st2
    putStrLn "updates:"
    plist (updates ++ hint_updates)
    result <- Sync.sync st2 (updates ++ hint_updates)
    case result of
        Just err -> putStrLn $ "err: " ++ show err
        Nothing -> putStrLn "synced"

right (Left err) = error $ "error: " ++ show err
right (Right x) = x
