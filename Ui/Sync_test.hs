-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

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
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as Exception

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified System.IO.Unsafe as Unsafe

import qualified Util.PPrint as PPrint
import qualified Util.Rect as Rect
import qualified Util.Seq as Seq
import Util.Test
import qualified Util.Test.Testing as Testing

import qualified Ui.Block as Block
import qualified Ui.BlockC as BlockC
import qualified Ui.Color as Color
import qualified Ui.Diff as Diff
import qualified Ui.Dump as Dump
import qualified Ui.Event as Event
import qualified Ui.Fltk as Fltk
import qualified Ui.Id as Id
import qualified Ui.Ruler as Ruler
import qualified Ui.Sel as Sel
import qualified Ui.Skeleton as Skeleton
import qualified Ui.Sync as Sync
import qualified Ui.Track as Track
import qualified Ui.Transform as Transform
import qualified Ui.Types as Types
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest
import Ui.UiTest (mkid)
import qualified Ui.Update as Update
import qualified Ui.Zoom as Zoom

import qualified Cmd.Internal as Internal
import Derive.TestInstances ()
import qualified App.Config as Config
import qualified App.LoadConfig as LoadConfig
import Global
import Types


-- TODO
-- test_error
-- set_block_title
-- set_track_scroll

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

test_create_resize_destroy_view = thread (return Ui.empty) $
    ("view with selection and titles", do
        v1 <- setup_state
        Ui.set_view_rect v1 (Rect.xywh 200 200 200 200)
        set_selection v1 (Sel.point 1 20 Sel.Positive)
        view <- Ui.get_view v1
        Ui.set_block_title (Block.view_block view) "block title"
        Ui.set_track_title t_track1_id "new track"
    , [[("x", "200"), ("y", "200"), ("w", "200"), ("h", "200"),
        ("title", "block title"), ("track1.title", "new track")]])
    : ("view moves over, gets bigger", do
        Ui.set_view_rect t_view_id (Rect.xywh 400 400 400 400)
    , [[("x", "400"), ("y", "400"), ("w", "400"),
        ("h", "400")]])
    : ("view is destroyed", do
        Ui.destroy_view t_view_id
    , [])
    : []

test_rename_block = thread run_setup $
    ("block changes BlockId, window recreated with new block", do
        Transform.map_block_ids $ \id ->
            if Id.BlockId id == t_block_id then Id.read_id "test/newb"
                else id
    , [[("window-title", "(bid \"test/newb\") -- (vid \"test/v1\")")]])
    : []

test_create_two_views = thread run_setup $
    ("view created, has big track, track title changes", do
        b2 <- create_block "b2" ""
            [Block.RId t_ruler_id, Block.TId t_track1_id t_ruler_id]
        create_view "v2" =<<
            make_view b2 (Rect.place 300 20 UiTest.default_rect)
                UiTest.default_zoom
        Ui.set_track_title t_track1_id "title changed!"
        Ui.set_track_width b2 1 300
    , [ [("track1.title", "title changed!"), ("track1.width", "30")]
      , [("track1.title", "title changed!"), ("track1.width", "300")]
      ])
    : []

test_set_block_config = do
    state <- run Ui.empty $ do
        setup_state
        set_selection t_view_id (selection 1 10 2 60)
    io_human "boxes go red" $ run state $ do
        block <- Ui.get_block t_block_id
        let config = Block.block_config block
        Ui.set_block_config t_block_id $ config
            { Block.config_skel_box = Block.Box Color.red ' '
            , Block.config_track_box = Block.Box Color.red ' '
            , Block.config_sb_box = Block.Box Color.red ' '
            }

test_set_skeleton = do
    let (_, state) = UiTest.run_mkview [("t1", []), ("t2", []), ("t3", [])]
    sync_states Ui.empty state
    state <- io_human "skel set" $ run state $ do
        Ui.set_skeleton t_block_id (Skeleton.make [(1, 2), (1, 3)])
    state <- io_human "skel set to something else" $ run state $ do
        Ui.set_skeleton t_block_id (Skeleton.make [(1, 2), (2, 3)])
    _ <- io_human "skel cleared" $ run state $ do
        Ui.set_skeleton t_block_id (Skeleton.make [])
    return ()

test_zoom_scroll = do
    state <- run Ui.empty $ do
        setup_state
        Ui.insert_events t_track1_id
            [ Event.event 0 10 "one"
            , Event.event 10 6 "scrunch"
            , Event.event 20 32 "two"
            , Event.event 100 64 "last"
            ]
    state <- io_human "scrolls to bottom" $ run state $ do
        Ui.modify_zoom t_view_id $ const (Zoom.Zoom 128 1)
    state <- io_human "scrolls back up" $ run state $ do
        Ui.modify_zoom t_view_id $ const (Zoom.Zoom 0 1)
    state <- io_human "zoom in to 2" $ run state $ do
        Ui.modify_zoom t_view_id $ const (Zoom.Zoom 0 2)
    state <- io_human "zoom out to .5" $ run state $ do
        Ui.modify_zoom t_view_id $ const (Zoom.Zoom 0 0.5)
    _ <- io_human "zoom out to 0, should clamp at a low number" $
        run state $ Ui.modify_zoom t_view_id $ const (Zoom.Zoom 0 0)
    return ()

test_set_status = do
    state <- run_setup
    state <- io_human "status set" $ run state $ do
        Ui.set_view_status t_view_id (0, "lol") (Just "o hai")
        Ui.set_view_status t_view_id (1, "brick") (Just "krazy")
    _ <- io_human "'lol' status cleared" $ run state $ do
        Ui.set_view_status t_view_id (0, "lol") Nothing
    return ()

test_set_track_flags = do
    state <- run Ui.empty $ do
        setup_state
        t2 <- create_track "b1.t2" track_with_events
        insert_track t_block_id 1 (Block.TId t2 t_ruler_id) 30
    state <- io_human "track1 is muted" $ run state $
        Ui.toggle_track_flag t_block_id 1 Block.Mute
    state <- io_human "track1 is soloed" $ run state $
        Ui.toggle_track_flag t_block_id 1 Block.Solo
    state <- io_human "collapse" $ run state $
        Ui.toggle_track_flag t_block_id 1 Block.Collapse
    state <- io_human "expand" $ run state $
        Ui.toggle_track_flag t_block_id 1 Block.Collapse
    state <- io_human "unsolo" $ run state $
        Ui.toggle_track_flag t_block_id 1 Block.Solo
    _ <- io_human "unmute" $ run state $
        Ui.toggle_track_flag t_block_id 1 Block.Mute
    return ()

test_set_track_width = do
    state <- io_human "two views" $ run Ui.empty $ do
        UiTest.mkblock (t_block, [(">", [])])
        create_view "v1" =<< make_view UiTest.default_block_id
            UiTest.default_rect UiTest.default_zoom
        create_view "v2" =<< make_view UiTest.default_block_id
            (Rect.place 300 0 UiTest.default_rect) UiTest.default_zoom
    _ <- io_human "both tracks change size" $ run state $ do
        Ui.set_track_width t_block_id 1 100
    return ()

test_adjacent_collapsed_tracks = do
    state <- run Ui.empty $
        UiTest.mkblock_view (t_block, [("1", []), ("2", []), ("3", [])])
    state <- io_human "collapse track 1" $ run state $ do
        Ui.toggle_track_flag t_block_id 1 Block.Collapse
    state <- io_human "collapse track 2" $ run state $ do
        Ui.toggle_track_flag t_block_id 2 Block.Collapse
    state <- io_human "collapse track 3" $ run state $ do
        Ui.toggle_track_flag t_block_id 3 Block.Collapse
    _ <- io_human "expand track 2" $ run state $ do
        Ui.toggle_track_flag t_block_id 2 Block.Collapse
    return ()

test_created_merged = do
    state <- io_human "already has merged track" $ run Ui.empty $ do
        UiTest.mkblock_view (t_block,
            [ ("t1", [(0, 1, "n1"), (2, 1, "")])
            , ("t2", [(0, 0, "p1"), (0.5, 0, "p2"), (2, 0, "p3")])
            ])
        Ui.merge_track t_block_id 1 2
    state <- io_human "unmerge" $ run state $ do
        Ui.unmerge_track t_block_id 1
    _ <- io_human "merge" $ run state $ do
        Ui.merge_track t_block_id 1 2
    return ()

test_set_track_merge = do
    let ([_, t2], state) = UiTest.run_mkview
            [ ("t1", [(0, 1, "n1"), (2, 1, "")])
            , ("t2", [(0, 0, "p1"), (0.5, 0, "p2"), (2, 0, "p3")])
            ]
    state <- io_human "has two tracks" $ run Ui.empty $ Ui.put state
    state <- io_human "t2 merged with t1" $ run state $ do
        Ui.set_merged_tracks UiTest.default_block_id 1 (Set.singleton t2)
    _ <- io_human "t2 modifications visible in t1" $ run state $ do
        Ui.insert_event t2 (Event.event 0.5 0 "xxx")
    return ()

test_merge_unmerge_track = do
    let ([_, t2], state) = UiTest.run_mkview
            [ ("t1", [(0, 1, "n1"), (2, 1, "")])
            , ("t2", [(0, 0, "p1"), (0.5, 0, "p2"), (2, 0, "p3")])
            ]
    state <- io_human "has two tracks" $ run Ui.empty $ do
        Ui.put state
        Ui.set_track_width UiTest.default_block_id 2 100
    state <- io_human "t2 merged with t1" $ run state $ do
        Ui.merge_track UiTest.default_block_id 1 2
    state <- io_human "t2's xxx visible in t1" $ run state $ do
        Ui.insert_event t2 (Event.event 0.5 0 "xxx")
    _ <- io_human "t1 unmerged, t1 reappears, still wide" $ run state $ do
        Ui.unmerge_track UiTest.default_block_id 1
    return ()

test_update_merged = do
    let (t2, state) = UiTest.run Ui.empty $ do
        [_t1, t2] <- UiTest.mkblock_view (UiTest.default_block_name,
            [ ("t1", [(0, 1, "n1"), (2, 1, "")])
            , ("t2", [(0, 0, "p1"), (0.5, 0, "p2"), (2, 0, "p3")])
            ])
        Ui.set_merged_tracks UiTest.default_block_id 1 (Set.singleton t2)
        return t2
    state <- io_human "block with merged track" $ run Ui.empty $
        Ui.put state
    _ <- io_human "t2 modifications visible in t1" $ run state $ do
        Ui.insert_event t2 (Event.event 0.5 0 "xxx")
    return ()

test_insert_remove_track = do
    state <- run_setup
    state <- io_human "new wide track at the end" $ run state $ do
        insert_track t_block_id 2 (Block.TId t_track1_id t_ruler_id) 80
    _ <- io_human "first track replaced by divider" $ run state $ do
        Ui.remove_track t_block_id 1
        insert_track t_block_id 1 (Block.DId UiTest.default_divider) 5
    return ()

-- Make sure removing and inserting the ruler track makes the others move over.
test_insert_remove_ruler_track = do
    state <- run_setup
    io_human "both tracks are replaced" $ run state $ do
        t2 <- create_track "b1.t2" (UiTest.empty_track "t2")
        Ui.remove_track t_block_id 0
        Ui.remove_track t_block_id 0
        insert_track t_block_id 0 (Block.TId t2 t_ruler_id) 20
        insert_track t_block_id 1 (Block.DId UiTest.default_divider) 5
    return ()

test_update_ruler_track = do
    state <- run_setup
    io_human "try to remove ruler, nothing happens" $ run state $ do
        Ui.remove_track t_block_id 0
    io_human "ruler is replaced by track, gets wider" $ run state $ do
        insert_track t_block_id 0 (Block.TId t_track1_id t_ruler_id) 70
    io_human "ruler gets smaller" $ run state $ do
        Ui.set_track_width t_block_id 0 10
    return ()

test_update_track = do
    -- sync to initial state
    state <- io_human "create view with one track" run_setup
    _ <- io_human "add events, get wider, turn green" $ run state $ do
        Ui.insert_events t_track1_id
            [Event.event 70 10 "last1", Event.event 90 15 "last2"]
        Ui.set_track_width t_block_id 1 50
        Ui.set_track_bg t_track1_id Color.green
    return ()

-- multiple simultaneous updates
test_update_two_tracks = do
    state <- run Ui.empty $ do
        setup_state
        t2 <- create_track "b1.t2" track_with_events
        insert_track t_block_id 1 (Block.TId t2 t_ruler_id) 30
        return ()
    io_human "1st track deleted, 2nd track gets wider" $ run state $ do
        Ui.remove_track t_block_id 1
        Ui.set_track_width t_block_id 1 100
    return ()

test_create_track = do
    state <- run_setup
    let msg = "new track with selection and new title, all bgs green"
    _ <- io_human msg $ run state $ do
        insert_track t_block_id 1 (Block.TId t_track1_id t_ruler_id) 50
        set_selection t_view_id (selection 1 10 1 60)
        Ui.set_track_title t_track1_id "new track"
        Ui.set_track_bg t_track1_id Color.green
    return ()

test_alter_track = do
    state <- run_setup
    state <- io_human "track should get wider" $ run state $ do
        Ui.set_track_width t_block_id 1 100
    _ <- io_human "lose ruler and stay wide" $ run state $ do
        rid <- create_ruler "r2" (UiTest.mkruler_44 0 0)
        Ui.set_track_ruler t_block_id 1 rid
    return ()

test_selection = do
    state <- run_setup
    state <- io_human "selection is set" $ run state $ do
        set_selection t_view_id (selection 0 10 1 20)
    _ <- io_human "selection is cleared" $ run state $ do
        set_selection t_view_id (selection 0 10 0 20)
    return ()

cue_marklist :: Ruler.Marklist
cue_marklist = Ruler.marklist
    [ (0, mark "start")
    , (2, mark "head explodes")
    ]

mark :: Text -> Ruler.Mark
mark name = Ruler.Mark 0 3 (Color.rgba 0.4 0 0.4 0.4) name 0 0

test_modify_ruler = do
    state <- run Ui.empty $ do
        setup_state
        insert_track t_block_id 2 (Block.RId t_ruler_id) 30
    state <- io_human "add head-explodes to all rulers" $ run state $
        Ui.modify_ruler t_ruler_id $
            Right . Ruler.set_marklist "cue" cue_marklist
    state <- io_human "meter goes away" $ run state $
        Ui.modify_ruler t_ruler_id $ Right . Ruler.remove_marklist "cue"
    _ <- io_human "doesn't crash when a track is collapsed" $ run state $ do
        Ui.add_track_flag t_block_id 1 Block.Collapse
        Ui.modify_ruler t_ruler_id $ Right . Ruler.modify_marklists
            (fmap $ second $ const (Ruler.marklist [(0, mark "new")]))
    return ()

-- | Selection is correct even when tracks are added or deleted.
test_selection_change_tracks = do
    state <- run_setup
    state <- run state $
        set_selection t_view_id (selection 1 10 1 20)
    state <- io_human "sel moves when new track is added" $ run state $ do
        insert_track t_block_id 1 (Block.TId t_track1_id t_ruler_id) 40
    _ <- io_human "sel moves back" $ run state $ do
        Ui.remove_track t_block_id 1
    return ()

test_insert_into_selection = do
    state <- run Ui.empty $ do
        v1 <- setup_state
        t2 <- create_track "b1.t2" track_with_events
        insert_track t_block_id 1 (Block.TId t2 t_ruler_id) 60
        create_track "b1.t3" track_with_events
        insert_track t_block_id 2 (Block.TId t2 t_ruler_id) 60
        set_selection v1 (selection 0 10 2 60)
    state <- io_human "insert into sel, gets bigger" $ run state $ do
        insert_track t_block_id 1 (Block.TId t_track1_id t_ruler_id) 20
    _ <- io_human "remove from sel, gets smaller" $ run state $ do
        Ui.remove_track t_block_id 1
    return ()

insert_track :: Ui.M m => BlockId -> TrackNum -> Block.TracklikeId
    -> Types.Width -> m ()
insert_track bid tracknum tracklike_id width =
    Ui.insert_track bid tracknum (Block.track tracklike_id width)


-- * thread

-- TODO perhaps useful in BlockC tests?

-- | (msg describing what is going to happen, action, patterns to match
-- against each view dump)
type Test a = (String, Ui.StateT IO a, [[(String, String)]])

thread :: IO Ui.State -> [Test a] -> IO Ui.State
thread setup tests = do
    state <- setup
    (\f -> foldM f state tests) $ \state (desc, action, expected) -> do
        putStrLn $ "====> " ++ desc
        state <- run state action
        -- Sort by view to ensure a consistent order.
        unparsed <- map snd . List.sort <$> BlockC.dump
        putStr "***dump: " >> pprint unparsed
        let dumps = map parse_dump unparsed
        passed <- match_dumps (txt desc) dumps expected
        unless passed (pause "")
        return state

match_dumps :: Text -> [Dump.Dump] -> [[(String, String)]] -> IO Bool
match_dumps desc dumps attrs = allM matches (Seq.zip_padded dumps attrs)
    where
    matches (Seq.Second attrs) =
        fail $ "view missing for attrs: " <> showt attrs
    matches (Seq.First dump) =
        fail $ "unexpected view: " <> showt dump
    matches (Seq.Both dump attrs)
        | null missing = pass $ "found " <> showt attrs
        | otherwise =
            fail $ "attrs " <> showt missing <> " not in dump: "
                <> txt (PPrint.pshow dump)
        where missing = filter (`notElem` dump) attrs
    allM f xs = foldl' (&&) True <$> mapM f xs
    fail = failure . ((desc <> ": ") <>)
    pass = success . ((desc <> ": ") <>)

parse_dump :: String -> Dump.Dump
parse_dump = either (error . ("failed to parse dump: "++) . untxt) id
    . Dump.parse

-- * util

track_with_events :: Track.Track
track_with_events = UiTest.make_track ("2", [(16, 10, "ho"), (30, 32, "eyo")])

set_selection :: Ui.M m => ViewId -> Sel.Selection -> m ()
set_selection view_id sel = Ui.set_selection view_id 0 (Just sel)

t_block = "b1"
t_block_id = Id.BlockId (mkid t_block)
t_ruler_id = Id.RulerId (mkid "r1")
t_track1_id = Id.TrackId (mkid "b1.t1")
t_view_id = Id.ViewId (mkid "v1")

run_setup :: IO Ui.State
run_setup = run Ui.empty setup_state

setup_state :: Ui.M m => m ViewId
setup_state = do
    ruler <- create_ruler "r1" (UiTest.mkruler_44 20 1)
    t1 <- create_track "b1.t1" (UiTest.empty_track "t1")
    b1 <- create_block t_block "hi b1" [Block.RId ruler, Block.TId t1 ruler]
    create_view "v1" =<< make_view b1 UiTest.default_rect UiTest.default_zoom

make_view :: Ui.M m => BlockId -> Rect.Rect -> Zoom.Zoom -> m Block.View
make_view block_id rect zoom = do
    block <- Ui.get_block block_id
    return $ Block.view block block_id rect zoom

create_view :: Ui.M m => Text -> Block.View -> m ViewId
create_view a b = Ui.create_view (mkid a) b

create_block :: Ui.M m => Text -> Text -> [Block.TracklikeId]
    -> m BlockId
create_block block_name = UiTest.create_block (UiTest.mkid block_name)

create_track :: Ui.M m => Text -> Track.Track -> m TrackId
create_track a b = Ui.create_track (mkid a) b

create_ruler :: Ui.M m => Text -> Ruler.Ruler -> m RulerId
create_ruler a b = Ui.create_ruler (mkid a) b

run :: Ui.State -> Ui.StateT IO a -> IO Ui.State
run st1 m = do
    res <- Ui.run st1 m
    let (_val, st2, cmd_updates) = expect_right res
    sync st1 st2 cmd_updates
    return st2

sync_states :: Ui.State -> Ui.State -> IO ()
sync_states st1 st2 = sync st1 st2 []

sync :: Ui.State -> Ui.State -> [Update.CmdUpdate] -> IO ()
sync st1 st2 cmd_updates = do
    let (_cupdates, dupdates) = Diff.diff cmd_updates st1 st2
    putStr "cmd updates: "
    pprint cmd_updates
    putStr "updates: "
    pprint dupdates
    result <- Sync.sync global_ui_channel Map.empty Internal.set_style st2
        dupdates
    case result of
        Just err -> putStrLn $ "err: " ++ show err
        Nothing -> putStrLn "synced"

selection :: TrackNum -> TrackTime -> TrackNum -> TrackTime -> Sel.Selection
selection a b c d = Sel.Selection
    { start_track = a, start_pos = b, cur_track = c, cur_pos = d
    , orientation = Sel.Positive
    }
