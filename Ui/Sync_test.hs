{- | In addition to Sync, this tests the whole State -> Diff -> Sync -> BlockC
    sequence.

    Sync is hard to test functionally because its whole point is to run IO
    actions.

    It's also hard to test Diff functionally.  I could test that the expected
    Updates are emitted, but that doesn't mean much; what I really care about
    is that the UI is updated in the expected manner.  I could build a model of
    the UI and apply the Updates to that, but it would be complicated and have
    bugs itself.

    Unless I can think of a better way to do these tests, they will
    unfortunately remain io_human tests in here.
-}
module Ui.Sync_test where
import qualified Control.Arrow as Arrow
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as Exception

import Util.Test
import qualified Util.Seq as Seq

import Ui.Types
import qualified Ui.Block as Block
import qualified Ui.Color as Color
import qualified Ui.Diff as Diff
import qualified Ui.Event as Event
import qualified Ui.Ruler as Ruler
import qualified Ui.State as State
import qualified Ui.Sync as Sync
import qualified Ui.Track as Track
import qualified Ui.Ui as Ui

import qualified Ui.UiTest as UiTest
import Ui.UiTest (mkid)

-- TODO
-- test_error
-- set_block_title
-- set_track_scroll

initialize f = do
    quit_request <- Concurrent.newMVar ()
    msg_chan <- STM.newTChanIO
    Concurrent.forkIO (f `Exception.finally` Ui.quit_ui_thread quit_request)
    Ui.event_loop quit_request msg_chan

test_create_resize_destroy_view = do
    state <- io_human "view with selection and titles" $ run State.empty $ do
        v1 <- setup_state
        State.set_selection v1 0 (Block.point_selection 1 (TrackPos 20))
        view <- State.get_view v1
        State.set_block_title (Block.view_block view) "new block!"
        State.set_track_title t_track1_id "new track"
    state <- io_human "view moves over, gets bigger" $ run state $ do
        State.set_view_rect t_view_id (Block.Rect 400 400 400 400)
    io_human "view is destroyed" $ run state $ do
        State.destroy_view t_view_id
    return ()

test_create_two_views = do
    state <- run_setup
    state <- io_human "view created, track title changes" $ run state $ do
        b2 <- create_block "b2" $ UiTest.mkblock ""
            UiTest.default_block_config
            [(Block.RId t_ruler_id, 20), (Block.TId t_track1_id t_ruler_id, 30)]
        v2 <- create_view "v2" $
            Block.view b2
                (UiTest.default_rect
                    { Block.rect_x = 300, Block.rect_y = 20 })
                UiTest.default_zoom UiTest.default_view_config
        State.set_track_title t_track1_id "hi there"
    return ()

test_set_view_config = do
    state <- run_setup
    state <- io_human "block and track titles get tall" $ run state $ do
        view <- State.get_view t_view_id
        let vconfig = Block.view_config view
        State.set_view_config t_view_id $ vconfig
            { Block.vconfig_block_title_height = 30
            , Block.vconfig_track_title_height = 30
            }
    io_human "sbs and status get big too" $ run state $ do
        view <- State.get_view t_view_id
        let vconfig = Block.view_config view
        State.set_view_config t_view_id $ vconfig
            { Block.vconfig_sb_size = 30
            , Block.vconfig_status_size = 30
            }
    return ()

test_set_block_config = do
    state <- run State.empty $ do
        setup_state
        State.set_selection t_view_id 0
            (Block.selection 1 (TrackPos 10) 2 (TrackPos 60))
    io_human "selections, bg, and boxes go red" $ run state $ do
        block <- State.get_block t_block_id
        let config = Block.block_config block
        State.set_block_config t_block_id $ config
            { Block.config_selection_colors = [Color.red]
            , Block.config_bg_color = Color.red
            , Block.config_track_box = (Color.red, ' ')
            , Block.config_sb_box = (Color.red, ' ')
            }
    return ()

test_set_skeleton = do
    let (tids, state) =
            UiTest.run_mkview [("t1", []), ("t2", []), ("t3", [])]
    sync_states State.empty state
    state <- io_human "skel set" $ run state $ do
        State.set_skeleton t_block_id $
            Just (mkskel 1 [mkskel 2 [], mkskel 3 []])
    state <- io_human "skel set to something else" $ run state $ do
        State.set_skeleton t_block_id $
            Just (mkskel 1 [mkskel 2 [mkskel 3 []]])
    _state <- io_human "skel cleared" $ run state $ do
        State.set_skeleton t_block_id Nothing
    return ()

mkskel tracknum subs =
    Block.Skeleton tracknum Block.TrackControl subs

test_zoom_scroll = do
    state <- run State.empty $ do
        v1 <- setup_state
        State.insert_events t_track1_id
            [ (TrackPos 0, Event.event "one" 10)
            , (TrackPos 10, Event.event "scrunch" 6)
            , (TrackPos 20, Event.event "two" 32)
            , (TrackPos 100, Event.event "last" 64)
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
        insert_track t_block_id 2 (Block.TId t_track1_id t_ruler_id) 80
    state <- io_human "first track replaced by divider" $ run state $ do
        State.remove_track t_block_id 1
        insert_track t_block_id 1 (Block.DId UiTest.default_divider) 5
    return ()

-- Make sure removing and inserting the ruler track makes the others move over.
test_insert_remove_ruler_track = do
    state <- run_setup
    io_human "both tracks are replaced" $ run state $ do
        t2 <- create_track "b1.t2" (UiTest.empty_track "t2")
        State.remove_track t_block_id 0
        State.remove_track t_block_id 0
        insert_track t_block_id 0 (Block.TId t2 t_ruler_id) 20
        insert_track t_block_id 1 (Block.DId UiTest.default_divider) 5
    return ()

test_update_ruler_track = do
    state <- run_setup
    io_human "try to remove ruler, nothing happens" $ run state $ do
        State.remove_track t_block_id 0
    io_human "ruler is replaced by track, gets wider" $ run state $ do
        insert_track t_block_id 0 (Block.TId t_track1_id t_ruler_id) 70
    io_human "ruler gets smaller" $ run state $ do
        State.set_track_width t_view_id 0 10
    return ()

test_update_track = do
    -- sync to initial state
    state <- io_human "create view with one track" run_setup

    state <- io_human "add events, get wider, turn green" $ run state $ do
        State.insert_events t_track1_id [(TrackPos 70, Event.event "last1" 10),
            (TrackPos 90, Event.event "last2" 15)]
        State.set_track_width t_view_id 1 50
        State.set_track_bg t_track1_id Color.green
    return ()

-- multiple simultaneous updates
test_update_two_tracks = do
    state <- run State.empty $ do
        v1 <- setup_state
        t2 <- create_track "b1.t2" UiTest.event_track_2
        insert_track t_block_id 1 (Block.TId t2 t_ruler_id) 30
        return ()
    io_human "1st track deleted, 2nd track gets wider" $ run state $ do
        State.remove_track t_block_id 1
        State.set_track_width t_view_id 1 100
    return ()

test_create_track = do
    state <- run_setup
    let msg = "new track with selectio and new title, all bgs green"
    state <- io_human msg $ run state $ do
        insert_track t_block_id 1 (Block.TId t_track1_id t_ruler_id) 50
        State.set_selection t_view_id 0
            (Block.selection 1 (TrackPos 10) 1 (TrackPos 60))
        State.set_track_title t_track1_id "new track"
        State.set_track_bg t_track1_id Color.green
    return ()

test_alter_track = do
    state <- run_setup
    state <- io_human "track should get wider" $ run state $ do
        State.set_track_width t_view_id 1 100
    state <- io_human "track should get a new ruler" $ run state $ do
        rid <- create_ruler "r2" (UiTest.mkruler 0 0)
        let set_ruler track = Block.modify_id track (Block.set_rid rid)
        State.modify_block t_block_id $ \block -> block
            { Block.block_tracks =
                Seq.modify_at (Block.block_tracks block) 1 set_ruler }
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
    [ (TrackPos 0, UiTest.mark "start")
    , (TrackPos 90, UiTest.mark "head explodes")
    ]

test_modify_ruler = do
    state <- run State.empty $ do
        setup_state
        insert_track t_block_id 2 (Block.RId t_ruler_id) 30
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
            (Block.selection 1 (TrackPos 10) 1 (TrackPos 20))
    state <- io_human "sel moves when new track is added" $ run state $ do
        insert_track t_block_id 1 (Block.TId t_track1_id t_ruler_id) 40
    state <- io_human "sel moves back" $ run state $ do
        State.remove_track t_block_id 1
    return ()

test_insert_into_selection = do
    state <- run State.empty $ do
        v1 <- setup_state
        t2 <- create_track "b1.t2" UiTest.event_track_2
        insert_track t_block_id 1 (Block.TId t2 t_ruler_id) 60
        t3 <- create_track "b1.t3" UiTest.event_track_2
        insert_track t_block_id 2 (Block.TId t2 t_ruler_id) 60
        State.set_selection v1 0
            (Block.selection 0 (TrackPos 10) 2 (TrackPos 60))
    state <- io_human "insert into sel, gets bigger" $ run state $ do
        insert_track t_block_id 1 (Block.TId t_track1_id t_ruler_id) 20
    state <- io_human "remove from sel, gets smaller" $ run state $ do
        State.remove_track t_block_id 1
    return ()

insert_track bid tracknum tracklike_id width =
    State.insert_track bid tracknum (Block.block_track tracklike_id width)

test_render_samples = do
    state <- run_setup
    let t_track2_id = Track.TrackId (mkid "b1.t2")
        samples1 = Track.samples sample_pairs
        bsamples = [(t_block_id, [(t_track2_id, samples1)])]
    state <- io_human "new track with samples" $ run_samples bsamples state $ do
        let track = Track.set_render_style Track.Line
                (UiTest.empty_track "t2")
        create_track "b1.t2" track
        insert_track t_block_id 2 (Block.TId t_track2_id t_ruler_id) 40
    state <- io_human "track samples filled" $ run_samples bsamples state $ do
        State.modify_track_render t_track2_id $ \config ->
            config { Track.render_style = Track.Filled }

    let samples2 = Track.samples ((TrackPos 0, 0) : tail sample_pairs)
        bsamples2 = [(t_block_id, [(t_track2_id, samples2)])]
    state <- io_human "first samples changed" $ run_samples bsamples2 state $ do
        State.insert_events t_track2_id $ map UiTest.mkevent
            [(0, 0, "0"), (32, 0, "0.5")]
    return ()

sample_pairs = map (Arrow.first TrackPos)
    [(0, 1), (32, 0.5), (32, 1), (64, 0), (500, 0), (510, 1), (520, 0)]

-- * util

t_ruler_id = Ruler.RulerId (mkid "r1")
t_block_id = Block.BlockId (mkid "b1")
t_track1_id = Track.TrackId (mkid "b1.t1")
t_view_id = Block.ViewId (mkid "v1")

run_setup = run State.empty setup_state
setup_state = do
    ruler <- create_ruler "r1" (UiTest.mkruler 20 10)
    t1 <- create_track "b1.t1" (UiTest.empty_track "t1")
    b1 <- create_block "b1" $
        UiTest.mkblock "hi b1" UiTest.default_block_config
            [(Block.RId ruler, 20), (Block.TId t1 ruler, 30)]
    create_view "v1"
        (Block.view b1 UiTest.default_rect UiTest.default_zoom
            UiTest.default_view_config)

create_view a b = State.create_view (mkid a) b
create_block a b = State.create_block (mkid a) b
create_track a b = State.create_track (mkid a) b
create_ruler a b = State.create_ruler (mkid a) b

run st1 m = do
    res <- State.run st1 m
    let (_val, st2, updates) = right res
    sync st1 st2 updates []
    return st2

run_samples block_samples st1 m = do
    res <- State.run st1 m
    let (_val, st2, updates) = right res
    sync st1 st2 updates block_samples
    return st2


sync_states st1 st2 = sync st1 st2 [] []

sync st1 st2 hint_updates block_samples = do
    let updates = right $ Diff.diff st1 st2
    putStrLn "updates:"
    plist (updates ++ hint_updates)
    result <- Sync.sync st2 (updates ++ hint_updates) block_samples
    case result of
        Just err -> putStrLn $ "err: " ++ show err
        Nothing -> putStrLn "synced"

right (Left err) = error $ "error: " ++ show err
right (Right x) = x
