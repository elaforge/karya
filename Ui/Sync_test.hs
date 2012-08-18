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
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as Exception

import qualified Data.List as List
import qualified Data.Map as Map

import Util.Control
import qualified Util.Rect as Rect
import qualified Util.Seq as Seq
import Util.Test

import qualified Ui.Block as Block
import qualified Ui.BlockC as BlockC
import qualified Ui.Color as Color
import qualified Ui.Diff as Diff
import qualified Ui.Dump as Dump
import qualified Ui.Event as Event
import qualified Ui.Id as Id
import qualified Ui.Ruler as Ruler
import qualified Ui.Skeleton as Skeleton
import qualified Ui.State as State
import qualified Ui.Sync as Sync
import qualified Ui.Transform as Transform
import qualified Ui.Types as Types
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest
import Ui.UiTest (mkid)
import qualified Ui.Update as Update

import qualified Cmd.Internal as Internal
import qualified App.Config as Config
import qualified App.LoadConfig as LoadConfig
import Types


-- TODO
-- test_error
-- set_block_title
-- set_track_scroll

initialize f = do
    quit_request <- Concurrent.newMVar ()
    msg_chan <- STM.newTChanIO
    LoadConfig.styles Config.styles
    Concurrent.forkIO (f `Exception.finally` Ui.quit_ui_thread quit_request)
    Ui.event_loop quit_request msg_chan

test_create_resize_destroy_view = thread (return State.empty) $
    ("view with selection and titles", do
        v1 <- setup_state
        State.set_view_rect v1 (Rect.xywh 200 200 200 200)
        set_selection v1 (Types.point_selection 1 20)
        view <- State.get_view v1
        State.set_block_title (Block.view_block view) "block title"
        State.set_track_title t_track1_id "new track"
    , [[("x", "200"), ("y", "200"), ("w", "200"), ("h", "200"),
        ("title", "block title"), ("track1.title", "new track")]])
    : ("view moves over, gets bigger", do
        State.set_view_rect t_view_id (Rect.xywh 400 400 400 400)
    , [[("x", "400"), ("y", "400"), ("w", "400"),
        ("h", "400")]])
    : ("view is destroyed", do
        State.destroy_view t_view_id
    , [])
    : []

test_rename_block = thread run_setup $
    ("block changes BlockId, window recreated with new block", do
        Transform.map_block_ids $ \id ->
            if Types.BlockId id == t_block_id then Id.read_id "test/newb"
                else id
    , [[("window-title", "(bid \"test/newb\") -- (vid \"test/v1\")")]])
    : []

test_create_two_views = thread run_setup $
    ("view created, has big track, track title changes", do
        b2 <- create_block "b2" "" [(Block.RId t_ruler_id, 20),
            (Block.TId t_track1_id t_ruler_id, 30)]
        v2 <- create_view "v2" $
            Block.view b2 (Rect.move 300 20 UiTest.default_rect)
                UiTest.default_zoom
        State.set_track_title t_track1_id "title changed!"
        State.set_track_width b2 1 300
    , [ [("track1.title", "title changed!"), ("track1.width", "30")]
      , [("track1.title", "title changed!"), ("track1.width", "300")]
      ])
    : []

test_set_block_config = do
    state <- run State.empty $ do
        setup_state
        set_selection t_view_id (Types.selection 1 10 2 60)
    io_human "boxes go red" $ run state $ do
        block <- State.get_block t_block_id
        let config = Block.block_config block
        State.set_block_config t_block_id $ config
            { Block.config_skel_box = Block.Box Color.red ' '
            , Block.config_track_box = Block.Box Color.red ' '
            , Block.config_sb_box = Block.Box Color.red ' '
            }

test_set_skeleton = do
    let (tids, state) =
            UiTest.run_mkview [("t1", []), ("t2", []), ("t3", [])]
    sync_states State.empty state
    state <- io_human "skel set" $ run state $ do
        State.set_skeleton t_block_id (Skeleton.make [(1, 2), (1, 3)])
    state <- io_human "skel set to something else" $ run state $ do
        State.set_skeleton t_block_id (Skeleton.make [(1, 2), (2, 3)])
    _state <- io_human "skel cleared" $ run state $ do
        State.set_skeleton t_block_id (Skeleton.make [])
    return ()

test_zoom_scroll = do
    state <- run State.empty $ do
        v1 <- setup_state
        State.insert_events t_track1_id
            [ Event.event 0 10 "one"
            , Event.event 10 6 "scrunch"
            , Event.event 20 32 "two"
            , Event.event 100 64 "last"
            ]
    state <- io_human "scrolls to bottom" $ run state $ do
        State.set_zoom t_view_id (Types.Zoom 128 1)
    state <- io_human "scrolls back up" $ run state $ do
        State.set_zoom t_view_id (Types.Zoom 0 1)
    state <- io_human "zoom in to 2" $ run state $ do
        State.set_zoom t_view_id (Types.Zoom 0 2)
    state <- io_human "zoom out to .5" $ run state $ do
        State.set_zoom t_view_id (Types.Zoom 0 0.5)
    state <- io_human "zoom out to 0, should clamp at a low number" $
        run state $ State.set_zoom t_view_id (Types.Zoom 0 0)
    return ()

test_set_status = do
    state <- run_setup
    state <- io_human "status set" $ run state $ do
        State.set_view_status t_view_id "lol" (Just "o hai")
        State.set_view_status t_view_id "brick" (Just "krazy")
    state <- io_human "'lol' status cleared" $ run state $ do
        State.set_view_status t_view_id "lol" Nothing
    return ()

test_set_track_flags = do
    state <- run State.empty $ do
        setup_state
        t2 <- create_track "b1.t2" UiTest.event_track_2
        insert_track t_block_id 1 (Block.TId t2 t_ruler_id) 30
    state <- io_human "track1 is muted" $ run state $ do
        State.toggle_track_flag t_block_id 1 Block.Mute
    state <- io_human "track1 is soloed" $ run state $ do
        State.toggle_track_flag t_block_id 1 Block.Solo
    state <- io_human "collapse" $ run state $ do
        State.toggle_track_flag t_block_id 1 Block.Collapse
    state <- io_human "expand" $ run state $ do
        State.toggle_track_flag t_block_id 1 Block.Collapse
    state <- io_human "unsolo" $ run state $ do
        State.toggle_track_flag t_block_id 1 Block.Solo
    state <- io_human "unmute" $ run state $ do
        State.toggle_track_flag t_block_id 1 Block.Mute
    return ()

test_set_track_width = do
    state <- io_human "two views" $ run State.empty $ do
        UiTest.mkblock (t_block, [(">", [])])
        create_view "v1" $ Block.view UiTest.default_block_id
            UiTest.default_rect UiTest.default_zoom
        create_view "v2" $ Block.view UiTest.default_block_id
            (Rect.move 300 0 UiTest.default_rect) UiTest.default_zoom
    state <- io_human "both tracks change size" $ run state $ do
        State.set_track_width t_block_id 1 100
    return ()

test_adjacent_collapsed_tracks = do
    state <- run State.empty $
        UiTest.mkblock_view (t_block, [("1", []), ("2", []), ("3", [])])
    state <- io_human "collapse track 1" $ run state $ do
        State.toggle_track_flag t_block_id 1 Block.Collapse
    state <- io_human "collapse track 2" $ run state $ do
        State.toggle_track_flag t_block_id 2 Block.Collapse
    state <- io_human "collapse track 3" $ run state $ do
        State.toggle_track_flag t_block_id 3 Block.Collapse
    state <- io_human "expand track 2" $ run state $ do
        State.toggle_track_flag t_block_id 2 Block.Collapse
    return ()

test_created_merged = do
    state <- io_human "already has merged track" $ run State.empty $ do
        [t1, t2] <- UiTest.mkblock_view (t_block,
            [ ("t1", [(0, 1, "n1"), (2, 1, "")])
            , ("t2", [(0, 0, "p1"), (0.5, 0, "p2"), (2, 0, "p3")])
            ])
        State.merge_track t_block_id 1 2
    state <- io_human "unmerge" $ run state $ do
        State.unmerge_track t_block_id 1
    state <- io_human "merge" $ run state $ do
        State.merge_track t_block_id 1 2
    return ()

test_set_track_merge = do
    let ([t1, t2], state) = UiTest.run_mkview
            [ ("t1", [(0, 1, "n1"), (2, 1, "")])
            , ("t2", [(0, 0, "p1"), (0.5, 0, "p2"), (2, 0, "p3")])
            ]
    state <- io_human "has two tracks" $ run State.empty $ State.put state
    state <- io_human "t2 merged with t1" $ run state $ do
        State.set_merged_tracks UiTest.default_block_id 1 [t2]
    state <- io_human "t2 modifications visible in t1" $ run state $ do
        State.insert_event t2 (Event.event 0.5 0 "xxx")
    return ()

test_merge_unmerge_track = do
    let ([t1, t2], state) = UiTest.run_mkview
            [ ("t1", [(0, 1, "n1"), (2, 1, "")])
            , ("t2", [(0, 0, "p1"), (0.5, 0, "p2"), (2, 0, "p3")])
            ]
    state <- io_human "has two tracks" $ run State.empty $ do
        State.put state
        State.set_track_width UiTest.default_block_id 2 100
    state <- io_human "t2 merged with t1" $ run state $ do
        State.merge_track UiTest.default_block_id 1 2
    state <- io_human "t2's xxx visible in t1" $ run state $ do
        State.insert_event t2 (Event.event 0.5 0 "xxx")
    state <- io_human "t1 unmerged, t1 reappears, still wide" $ run state $ do
        State.unmerge_track UiTest.default_block_id 1
    return ()

test_update_merged = do
    let ((t1, t2), state) = UiTest.run State.empty $ do
        [t1, t2] <- UiTest.mkblock_view (UiTest.default_block_name,
            [ ("t1", [(0, 1, "n1"), (2, 1, "")])
            , ("t2", [(0, 0, "p1"), (0.5, 0, "p2"), (2, 0, "p3")])
            ])
        State.set_merged_tracks UiTest.default_block_id 1 [t2]
        return (t1, t2)
    state <- io_human "block with merged track" $ run State.empty $
        State.put state
    state <- io_human "t2 modifications visible in t1" $ run state $ do
        State.insert_event t2 (Event.event 0.5 0 "xxx")
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
        State.set_track_width t_block_id 0 10
    return ()

test_update_track = do
    -- sync to initial state
    state <- io_human "create view with one track" run_setup

    state <- io_human "add events, get wider, turn green" $ run state $ do
        State.insert_events t_track1_id
            [Event.event 70 10 "last1", Event.event 90 15 "last2"]
        State.set_track_width t_block_id 1 50
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
        State.set_track_width t_block_id 1 100
    return ()

test_create_track = do
    state <- run_setup
    let msg = "new track with selection and new title, all bgs green"
    state <- io_human msg $ run state $ do
        insert_track t_block_id 1 (Block.TId t_track1_id t_ruler_id) 50
        set_selection t_view_id (Types.selection 1 10 1 60)
        State.set_track_title t_track1_id "new track"
        State.set_track_bg t_track1_id Color.green
    return ()

test_alter_track = do
    state <- run_setup
    state <- io_human "track should get wider" $ run state $ do
        State.set_track_width t_block_id 1 100
    state <- io_human "lose ruler and stay wide" $ run state $ do
        rid <- create_ruler "r2" (UiTest.mkruler 0 0)
        State.set_track_ruler t_block_id 1 rid
    return ()

test_selection = do
    state <- run_setup
    state <- io_human "selection is set" $ run state $ do
        set_selection t_view_id (Types.selection 0 10 1 20)
    state <- io_human "selection is cleared" $ run state $ do
        set_selection t_view_id (Types.selection 0 10 0 20)
    return ()

cue_marklist = Ruler.marklist
    [ (0, UiTest.mark "start")
    , (90, UiTest.mark "head explodes")
    ]

test_modify_ruler = do
    state <- run State.empty $ do
        setup_state
        insert_track t_block_id 2 (Block.RId t_ruler_id) 30
    state <- io_human "add head-explodes to all rulers" $ run state $
        State.modify_ruler t_ruler_id $ Ruler.set_marklist "cue" cue_marklist
    state <- io_human "meter goes away" $ run state $
        State.modify_ruler t_ruler_id $ Ruler.remove_marklist "cue"
    return ()

-- | Selection is correct even when tracks are added or deleted.
test_selection_change_tracks = do
    state <- run_setup
    state <- run state $
        set_selection t_view_id (Types.selection 1 10 1 20)
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
        set_selection v1 (Types.selection 0 10 2 60)
    state <- io_human "insert into sel, gets bigger" $ run state $ do
        insert_track t_block_id 1 (Block.TId t_track1_id t_ruler_id) 20
    state <- io_human "remove from sel, gets smaller" $ run state $ do
        State.remove_track t_block_id 1
    return ()

insert_track bid tracknum tracklike_id width =
    State.insert_track bid tracknum (Block.track tracklike_id width)


-- * thread

-- TODO perhaps useful in BlockC tests?

-- | (msg describing what is going to happen, action, patterns to match
-- against each view dump)
type Test a = (String, State.StateT IO a, [[(String, String)]])

thread :: IO State.State -> [Test a] -> IO State.State
thread setup tests = do
    state <- setup
    (\f -> foldM f state tests) $ \state (desc, action, expected) -> do
        putStrLn $ "====> " ++ desc
        state <- run state action
        -- Sort by view to ensure a consistent order.
        unparsed <- map snd . List.sort <$> BlockC.dump
        putStr "***dump: " >> pslist unparsed
        let dumps = map parse_dump unparsed
        passed <- match_dumps desc dumps expected
        unless passed pause
        return state

match_dumps :: String -> [Dump.Dump] -> [[(String, String)]] -> IO Bool
match_dumps desc dumps attrs = allM match (Seq.padded_zip dumps attrs)
    where
    match (Seq.Second attrs) =
        fail $ "view missing for attrs: " ++ show attrs
    match (Seq.First dump) =
        fail $ "unexpected view: " ++ show dump
    match (Seq.Both dump attrs)
        | null missing = pass $ "found " ++ show attrs
        | otherwise =
            fail $ "attrs " ++ show missing ++ " not in dump: " ++ pshow dump
        where missing = filter (`notElem` dump) attrs
    allM f xs = List.foldl' (&&) True <$> mapM f xs
    fail = failure . ((desc ++ ": ") ++)
    pass = success . ((desc ++ ": ") ++)

parse_dump :: String -> Dump.Dump
parse_dump = either (error . ("failed to parse dump: "++)) id . Dump.parse

-- * util

set_selection view_id sel = State.set_selection view_id 0 (Just sel)

t_block = "b1"
t_block_id = Types.BlockId (mkid t_block)
t_ruler_id = Types.RulerId (mkid "r1")
t_track1_id = Types.TrackId (mkid "b1.t1")
t_view_id = Types.ViewId (mkid "v1")

run_setup :: IO State.State
run_setup = run State.empty setup_state

setup_state :: (State.M m) => m ViewId
setup_state = do
    ruler <- create_ruler "r1" (UiTest.mkruler 20 10)
    t1 <- create_track "b1.t1" (UiTest.empty_track "t1")
    b1 <- create_block t_block "hi b1"
        [(Block.RId ruler, 20), (Block.TId t1 ruler, 30)]
    create_view "v1" (Block.view b1 UiTest.default_rect UiTest.default_zoom)

create_view a b = State.create_view (mkid a) b
create_block block_name title tracks =
    UiTest.create_block block_name title tracks
create_track a b = State.create_track (mkid a) b
create_ruler a b = State.create_ruler (mkid a) b

run :: State.State -> State.StateT IO a -> IO State.State
run st1 m = do
    res <- State.run st1 m
    let (_val, st2, cmd_updates) = right "run: state" res
    sync st1 st2 cmd_updates
    return st2

sync_states :: State.State -> State.State -> IO ()
sync_states st1 st2 = sync st1 st2 []

sync :: State.State -> State.State -> [Update.CmdUpdate] -> IO ()
sync st1 st2 cmd_updates = do
    let (_cupdates, dupdates) = Diff.diff cmd_updates st1 st2
    pmlist "cmd updates" cmd_updates
    pmlist "updates" dupdates
    result <- Sync.sync Map.empty Internal.set_style st2 dupdates
    case result of
        Just err -> putStrLn $ "err: " ++ show err
        Nothing -> putStrLn "synced"

right :: (Show err) => String -> Either err a -> a
right msg (Left err) = error $ msg ++ " error: " ++ show err
right msg (Right x) = x
