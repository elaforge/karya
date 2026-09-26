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
import qualified Util.Lists as Lists
import qualified Util.Test.Testing as Testing

import qualified App.Config as Config
import qualified App.LoadConfig as LoadConfig
import qualified Cmd.Internal as Internal
import           Cmd.TestInstances ()
import qualified Ui.Block as Block
import qualified Ui.BlockC as BlockC
import qualified Ui.Color as Color
import qualified Ui.Diff as Diff
import qualified Ui.Dump as Dump
import qualified Ui.Event as Event
import qualified Ui.Fltk as Fltk
import qualified Ui.Id as Id
import qualified Ui.Meter.Mark as Mark
import qualified Ui.Ruler as Ruler
import qualified Ui.Sel as Sel
import qualified Ui.Skeleton as Skeleton
import qualified Ui.Sync as Sync
import qualified Ui.Track as Track
import qualified Ui.Transform as Transform
import qualified Ui.Types as Types
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest
import           Ui.UiTest (mkid)
import qualified Ui.Update as Update
import qualified Ui.Zoom as Zoom

import           Global
import           Types
import           Util.Test


-- TODO
-- test_error
-- set_block_title
-- set_track_scroll

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

test_create_resize_destroy_view :: Test
test_create_resize_destroy_view = thread (pure Ui.empty) $
    ("view with selection and titles", do
        v1 <- setupState
        Ui.setViewRect v1 (Rect.xywh 200 200 200 200)
        setSelection v1 (Sel.point 1 20 Sel.Positive)
        view <- Ui.getView v1
        Ui.setBlockTitle (Block.viewBlock view) "block title"
        Ui.setTrackTitle tTrack1Id "new track"
    , [[("x", "200"), ("y", "200"), ("w", "200"), ("h", "200"),
        ("title", "block title"), ("track1.title", "new track")]])
    : ("view moves over, gets bigger", do
        Ui.setViewRect tViewId (Rect.xywh 400 400 400 400)
    , [[("x", "400"), ("y", "400"), ("w", "400"),
        ("h", "400")]])
    : ("view is destroyed", do
        Ui.destroyView tViewId
    , [])
    : []

test_rename_block :: Test
test_rename_block = thread runSetup $
    ("block changes BlockId, window recreated with new block", do
        Transform.mapBlockIds $ \id ->
            if Id.BlockId id == tBlockId then Id.readId "test/newb"
                else id
    , [[("window-title", "(bid \"test/newb\") -- (vid \"test/v1\")")]])
    : []

test_create_two_views :: Test
test_create_two_views = thread runSetup $
    ("view created, has big track, track title changes", do
        b2 <- createBlock "b2" ""
            [Block.RId tRulerId, Block.TId tTrack1Id tRulerId]
        createView "v2" =<<
            makeView b2 (Rect.place 300 20 UiTest.defaultRect)
                UiTest.defaultZoom
        Ui.setTrackTitle tTrack1Id "title changed!"
        Ui.setTrackWidth b2 1 300
    , [ [("track1.title", "title changed!"), ("track1.width", "30")]
      , [("track1.title", "title changed!"), ("track1.width", "300")]
      ])
    : []

test_set_block_config :: Test
test_set_block_config = do
    state <- run Ui.empty $ do
        setupState
        setSelection tViewId (selection 1 10 2 60)
    ioHuman "boxes go red" $ run state $ do
        let box = Block.Box Color.red ' '
        Ui.setEditBox tBlockId box box
        Ui.setPlayBox tBlockId Color.red
    pure ()

test_set_skeleton :: Test
test_set_skeleton = do
    let (_, state) = UiTest.runMkview [("t1", []), ("t2", []), ("t3", [])]
    syncStates Ui.empty state
    state <- ioHuman "skel set" $ run state $ do
        Ui.setSkeleton tBlockId (Skeleton.make [(1, 2), (1, 3)])
    state <- ioHuman "skel set to something else" $ run state $ do
        Ui.setSkeleton tBlockId (Skeleton.make [(1, 2), (2, 3)])
    _ <- ioHuman "skel cleared" $ run state $ do
        Ui.setSkeleton tBlockId (Skeleton.make [])
    pure ()

test_zoom_scroll :: Test
test_zoom_scroll = do
    state <- run Ui.empty $ do
        setupState
        Ui.insertEvents tTrack1Id
            [ Event.event 0 10 "one"
            , Event.event 10 6 "scrunch"
            , Event.event 20 32 "two"
            , Event.event 100 64 "last"
            ]
    state <- ioHuman "scrolls to bottom" $ run state $ do
        Ui.modifyZoom tViewId $ const (Zoom.Zoom 128 1)
    state <- ioHuman "scrolls back up" $ run state $ do
        Ui.modifyZoom tViewId $ const (Zoom.Zoom 0 1)
    state <- ioHuman "zoom in to 2" $ run state $ do
        Ui.modifyZoom tViewId $ const (Zoom.Zoom 0 2)
    state <- ioHuman "zoom out to .5" $ run state $ do
        Ui.modifyZoom tViewId $ const (Zoom.Zoom 0 0.5)
    _ <- ioHuman "zoom out to 0, should clamp at a low number" $
        run state $ Ui.modifyZoom tViewId $ const (Zoom.Zoom 0 0)
    pure ()

test_set_status :: Test
test_set_status = do
    state <- runSetup
    state <- ioHuman "status set" $ run state $ do
        Ui.setViewStatus tViewId (0, "lol") (Just "o hai")
        Ui.setViewStatus tViewId (1, "brick") (Just "krazy")
    _ <- ioHuman "'lol' status cleared" $ run state $ do
        Ui.setViewStatus tViewId (0, "lol") Nothing
    pure ()

test_set_track_flags :: Test
test_set_track_flags = do
    state <- run Ui.empty $ do
        setupState
        t2 <- createTrack "b1.t2" trackWithEvents
        insertTrack tBlockId 1 (Block.TId t2 tRulerId) 30
    state <- ioHuman "track1 is muted" $ run state $
        Ui.toggleTrackFlag tBlockId 1 Block.Mute
    state <- ioHuman "track1 is soloed" $ run state $
        Ui.toggleTrackFlag tBlockId 1 Block.Solo
    state <- ioHuman "collapse" $ run state $
        Ui.toggleTrackFlag tBlockId 1 Block.Collapse
    state <- ioHuman "expand" $ run state $
        Ui.toggleTrackFlag tBlockId 1 Block.Collapse
    state <- ioHuman "unsolo" $ run state $
        Ui.toggleTrackFlag tBlockId 1 Block.Solo
    _ <- ioHuman "unmute" $ run state $
        Ui.toggleTrackFlag tBlockId 1 Block.Mute
    pure ()

test_set_track_width :: Test
test_set_track_width = do
    state <- ioHuman "two views" $ run Ui.empty $ do
        UiTest.mkblocks [(tBlock, [(">", [])])]
        createView "v1" =<< makeView UiTest.defaultBlockId
            UiTest.defaultRect UiTest.defaultZoom
        createView "v2" =<< makeView UiTest.defaultBlockId
            (Rect.place 300 0 UiTest.defaultRect) UiTest.defaultZoom
    _ <- ioHuman "both tracks change size" $ run state $ do
        Ui.setTrackWidth tBlockId 1 100
    pure ()

test_adjacent_collapsed_tracks :: Test
test_adjacent_collapsed_tracks = do
    state <- run Ui.empty $
        UiTest.mkblockView (tBlock, [("1", []), ("2", []), ("3", [])])
    state <- ioHuman "collapse track 1" $ run state $ do
        Ui.toggleTrackFlag tBlockId 1 Block.Collapse
    state <- ioHuman "collapse track 2" $ run state $ do
        Ui.toggleTrackFlag tBlockId 2 Block.Collapse
    state <- ioHuman "collapse track 3" $ run state $ do
        Ui.toggleTrackFlag tBlockId 3 Block.Collapse
    _ <- ioHuman "expand track 2" $ run state $ do
        Ui.toggleTrackFlag tBlockId 2 Block.Collapse
    pure ()

test_created_merged :: Test
test_created_merged = do
    state <- ioHuman "already has merged track" $ run Ui.empty $ do
        UiTest.mkblockView (tBlock,
            [ ("t1", [(0, 1, "n1"), (2, 1, "")])
            , ("t2", [(0, 0, "p1"), (0.5, 0, "p2"), (2, 0, "p3")])
            ])
        Ui.mergeTrack tBlockId 1 2
    state <- ioHuman "unmerge" $ run state $ do
        Ui.unmergeTrack tBlockId 1
    _ <- ioHuman "merge" $ run state $ do
        Ui.mergeTrack tBlockId 1 2
    pure ()

test_set_track_merge :: Test
test_set_track_merge = do
    let ([_, t2], state) = UiTest.runMkview
            [ ("t1", [(0, 1, "n1"), (2, 1, "")])
            , ("t2", [(0, 0, "p1"), (0.5, 0, "p2"), (2, 0, "p3")])
            ]
    state <- ioHuman "has two tracks" $ run Ui.empty $ Ui.put state
    state <- ioHuman "t2 merged with t1" $ run state $ do
        Ui.setMergedTracks UiTest.defaultBlockId 1 (Set.singleton t2)
    _ <- ioHuman "t2 modifications visible in t1" $ run state $ do
        Ui.insertEvent t2 (Event.event 0.5 0 "xxx")
    pure ()

test_merge_unmerge_track :: Test
test_merge_unmerge_track = do
    let ([_, t2], state) = UiTest.runMkview
            [ ("t1", [(0, 1, "n1"), (2, 1, "")])
            , ("t2", [(0, 0, "p1"), (0.5, 0, "p2"), (2, 0, "p3")])
            ]
    state <- ioHuman "has two tracks" $ run Ui.empty $ do
        Ui.put state
        Ui.setTrackWidth UiTest.defaultBlockId 2 100
    state <- ioHuman "t2 merged with t1" $ run state $ do
        Ui.mergeTrack UiTest.defaultBlockId 1 2
    state <- ioHuman "t2's xxx visible in t1" $ run state $ do
        Ui.insertEvent t2 (Event.event 0.5 0 "xxx")
    _ <- ioHuman "t1 unmerged, t1 reappears, still wide" $ run state $ do
        Ui.unmergeTrack UiTest.defaultBlockId 1
    pure ()

test_update_merged :: Test
test_update_merged = do
    let (t2, state) = UiTest.run Ui.empty $ do
        [_t1, t2] <- UiTest.mkblockView (UiTest.defaultBlockName,
            [ ("t1", [(0, 1, "n1"), (2, 1, "")])
            , ("t2", [(0, 0, "p1"), (0.5, 0, "p2"), (2, 0, "p3")])
            ])
        Ui.setMergedTracks UiTest.defaultBlockId 1 (Set.singleton t2)
        pure t2
    state <- ioHuman "block with merged track" $ run Ui.empty $
        Ui.put state
    _ <- ioHuman "t2 modifications visible in t1" $ run state $ do
        Ui.insertEvent t2 (Event.event 0.5 0 "xxx")
    pure ()

test_insert_remove_track :: Test
test_insert_remove_track = do
    state <- runSetup
    state <- ioHuman "new wide track at the end" $ run state $ do
        insertTrack tBlockId 2 (Block.TId tTrack1Id tRulerId) 80
    _ <- ioHuman "first track replaced by divider" $ run state $ do
        Ui.removeTrack tBlockId 1
        insertTrack tBlockId 1 (Block.DId UiTest.defaultDivider) 5
    pure ()

-- Make sure removing and inserting the ruler track makes the others move over.
test_insert_remove_ruler_track :: Test
test_insert_remove_ruler_track = do
    state <- runSetup
    ioHuman "both tracks are replaced" $ run state $ do
        t2 <- createTrack "b1.t2" (UiTest.emptyTrack "t2")
        Ui.removeTrack tBlockId 0
        Ui.removeTrack tBlockId 0
        insertTrack tBlockId 0 (Block.TId t2 tRulerId) 20
        insertTrack tBlockId 1 (Block.DId UiTest.defaultDivider) 5
    pure ()

test_update_ruler_track :: Test
test_update_ruler_track = do
    state <- runSetup
    ioHuman "try to remove ruler, nothing happens" $ run state $ do
        Ui.removeTrack tBlockId 0
    ioHuman "ruler is replaced by track, gets wider" $ run state $ do
        insertTrack tBlockId 0 (Block.TId tTrack1Id tRulerId) 70
    ioHuman "ruler gets smaller" $ run state $ do
        Ui.setTrackWidth tBlockId 0 10
    pure ()

test_update_track :: Test
test_update_track = do
    -- sync to initial state
    state <- ioHuman "create view with one track" runSetup
    _ <- ioHuman "add events, get wider, turn green" $ run state $ do
        Ui.insertEvents tTrack1Id
            [Event.event 70 10 "last1", Event.event 90 15 "last2"]
        Ui.setTrackWidth tBlockId 1 50
        Ui.setTrackBg tTrack1Id Color.green
    pure ()

-- multiple simultaneous updates
test_update_two_tracks :: Test
test_update_two_tracks = do
    state <- run Ui.empty $ do
        setupState
        t2 <- createTrack "b1.t2" trackWithEvents
        insertTrack tBlockId 1 (Block.TId t2 tRulerId) 30
        pure ()
    ioHuman "1st track deleted, 2nd track gets wider" $ run state $ do
        Ui.removeTrack tBlockId 1
        Ui.setTrackWidth tBlockId 1 100
    pure ()

test_create_track :: Test
test_create_track = do
    state <- runSetup
    let msg = "new track with selection and new title, all bgs green"
    _ <- ioHuman msg $ run state $ do
        insertTrack tBlockId 1 (Block.TId tTrack1Id tRulerId) 50
        setSelection tViewId (selection 1 10 1 60)
        Ui.setTrackTitle tTrack1Id "new track"
        Ui.setTrackBg tTrack1Id Color.green
    pure ()

test_alter_track :: Test
test_alter_track = do
    state <- runSetup
    state <- ioHuman "track should get wider" $ run state $ do
        Ui.setTrackWidth tBlockId 1 100
    _ <- ioHuman "lose ruler and stay wide" $ run state $ do
        rid <- createRuler "r2" (UiTest.mkruler44 0 0)
        Ui.setTrackRuler tBlockId 1 rid
    pure ()

test_selection :: Test
test_selection = do
    state <- runSetup
    state <- ioHuman "selection is set" $ run state $ do
        setSelection tViewId (selection 0 10 1 20)
    _ <- ioHuman "selection is cleared" $ run state $ do
        setSelection tViewId (selection 0 10 0 20)
    pure ()

cueMarklist :: Mark.Marklist
cueMarklist = Mark.marklist
    [ (0, mark "start")
    , (2, mark "head explodes")
    ]

mark :: Text -> Mark.Mark
mark name = Mark.Mark minBound 3 (Color.rgba 0.4 0 0.4 0.4) name 0 0

test_modify_ruler :: Test
test_modify_ruler = do
    state <- run Ui.empty $ do
        setupState
        insertTrack tBlockId 2 (Block.RId tRulerId) 30
    state <- ioHuman "add head-explodes to all rulers" $ run state $
        Ui.modifyRuler tRulerId $
            Right . Ruler.setMarklist "cue" cueMarklist
    state <- ioHuman "meter goes away" $ run state $
        Ui.modifyRuler tRulerId $ Right . Ruler.removeMarklist "cue"
    _ <- ioHuman "doesn't crash when a track is collapsed" $ run state $ do
        Ui.addTrackFlag tBlockId 1 Block.Collapse
        Ui.modifyRuler tRulerId $ Right . Ruler.modifyMarklists
            (fmap $ second $ const (Mark.marklist [(0, mark "new")]))
    pure ()

-- | Selection is correct even when tracks are added or deleted.
test_selection_change_tracks :: Test
test_selection_change_tracks = do
    state <- runSetup
    state <- run state $
        setSelection tViewId (selection 1 10 1 20)
    state <- ioHuman "sel moves when new track is added" $ run state $ do
        insertTrack tBlockId 1 (Block.TId tTrack1Id tRulerId) 40
    _ <- ioHuman "sel moves back" $ run state $ do
        Ui.removeTrack tBlockId 1
    pure ()

test_insert_into_selection :: Test
test_insert_into_selection = do
    state <- run Ui.empty $ do
        v1 <- setupState
        t2 <- createTrack "b1.t2" trackWithEvents
        insertTrack tBlockId 1 (Block.TId t2 tRulerId) 60
        createTrack "b1.t3" trackWithEvents
        insertTrack tBlockId 2 (Block.TId t2 tRulerId) 60
        setSelection v1 (selection 0 10 2 60)
    state <- ioHuman "insert into sel, gets bigger" $ run state $ do
        insertTrack tBlockId 1 (Block.TId tTrack1Id tRulerId) 20
    _ <- ioHuman "remove from sel, gets smaller" $ run state $ do
        Ui.removeTrack tBlockId 1
    pure ()

insertTrack :: Ui.M m => BlockId -> TrackNum -> Block.TracklikeId
    -> Types.Width -> m ()
insertTrack bid tracknum tracklikeId width =
    Ui.insertTrack bid tracknum (Block.track tracklikeId width)


-- * thread

-- TODO perhaps useful in BlockC tests?

-- | (msg describing what is going to happen, action, patterns to match
-- against each view dump)
type STest a = (String, Ui.StateT IO a, [[(String, String)]])

thread :: IO Ui.State -> [STest a] -> IO ()
thread setup tests = do
    state <- setup
    (\f -> foldM f state tests) $ \state (desc, action, expected) -> do
        putStrLn $ "====> " ++ desc
        state <- run state action
        -- Sort by view to ensure a consistent order.
        unparsed <- map snd . List.sort <$> BlockC.dump
        putStr "***dump: " >> pprint unparsed
        let dumps = map parseDump unparsed
        passed <- matchDumps (txt desc) dumps expected
        unless passed (pause "")
        pure state
    pure ()

matchDumps :: Text -> [Dump.Dump] -> [[(String, String)]] -> IO Bool
matchDumps desc dumps attrs = allM matches (Lists.zipPadded dumps attrs)
    where
    matches (Lists.Second attrs) =
        fail $ "view missing for attrs: " <> showt attrs
    matches (Lists.First dump) =
        fail $ "unexpected view: " <> showt dump
    matches (Lists.Both dump attrs)
        | null missing = pass $ "found " <> showt attrs
        | otherwise =
            fail $ "attrs " <> showt missing <> " not in dump: "
                <> txt (PPrint.pshow dump)
        where missing = filter (`notElem` dump) attrs
    allM f xs = foldl' (&&) True <$> mapM f xs
    fail = (>> pure False) . failure . ((desc <> ": ") <>)
    pass = (>> pure True) . success . ((desc <> ": ") <>)

parseDump :: String -> Dump.Dump
parseDump = either (error . ("failed to parse dump: "++) . untxt) id
    . Dump.parse

-- * util

trackWithEvents :: Track.Track
trackWithEvents = UiTest.makeTrack ("2", [(16, 10, "ho"), (30, 32, "eyo")])

setSelection :: Ui.M m => ViewId -> Sel.Selection -> m ()
setSelection viewId sel = Ui.setSelection viewId 0 (Just sel)

tBlock = "b1"
tBlockId = Id.BlockId (mkid tBlock)
tRulerId = Id.RulerId (mkid "r1")
tTrack1Id = Id.TrackId (mkid "b1.t1")
tViewId = Id.ViewId (mkid "v1")

runSetup :: IO Ui.State
runSetup = run Ui.empty setupState

setupState :: Ui.M m => m ViewId
setupState = do
    ruler <- createRuler "r1" (UiTest.mkruler44 20 1)
    t1 <- createTrack "b1.t1" (UiTest.emptyTrack "t1")
    b1 <- createBlock tBlock "hi b1" [Block.RId ruler, Block.TId t1 ruler]
    createView "v1" =<< makeView b1 UiTest.defaultRect UiTest.defaultZoom

makeView :: Ui.M m => BlockId -> Rect.Rect -> Zoom.Zoom -> m Block.View
makeView blockId rect zoom = do
    block <- Ui.getBlock blockId
    pure $ Block.view block blockId rect zoom

createView :: Ui.M m => Text -> Block.View -> m ViewId
createView a b = Ui.createView (mkid a) b

createBlock :: Ui.M m => Text -> Text -> [Block.TracklikeId]
    -> m BlockId
createBlock blockName = UiTest.createBlock (UiTest.mkid blockName)

createTrack :: Ui.M m => Text -> Track.Track -> m TrackId
createTrack a b = Ui.createTrack (mkid a) b

createRuler :: Ui.M m => Text -> Ruler.Ruler -> m RulerId
createRuler a b = Ui.createRuler (mkid a) b

run :: Ui.State -> Ui.StateT IO a -> IO Ui.State
run st1 m = do
    res <- Ui.run st1 m
    let (_val, st2, damage) = expectRight res
    sync st1 st2 damage
    pure st2

syncStates :: Ui.State -> Ui.State -> IO ()
syncStates st1 st2 = sync st1 st2 mempty

sync :: Ui.State -> Ui.State -> Update.UiDamage -> IO ()
sync st1 st2 damage = do
    let (_cupdates, dupdates) = Diff.diff damage st1 st2
    putStr "ui damage: "
    pprint damage
    putStr "updates: "
    pprint dupdates
    result <- Sync.sync globalUiChannel Map.empty Internal.setStyle st2
        dupdates
    case result of
        Just err -> putStrLn $ "err: " ++ show err
        Nothing -> putStrLn "synced"

selection :: TrackNum -> TrackTime -> TrackNum -> TrackTime -> Sel.Selection
selection a b c d = Sel.Selection
    { startTrack = a, startPos = b, curTrack = c, curPos = d
    , orientation = Sel.Positive
    }
