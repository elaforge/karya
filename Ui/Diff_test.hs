-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Ui.Diff_test where
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Util.Ranges as Ranges
import qualified App.Config as Config
import qualified Derive.Derive as Derive
import qualified Ui.Block as Block
import qualified Ui.Diff as Diff
import qualified Ui.Skeleton as Skeleton
import qualified Ui.Track as Track
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig
import qualified Ui.UiTest as UiTest
import qualified Ui.Update as Update

import           Global
import           Types
import           Util.Test


test_display_track :: Test
test_display_track = do
    let ([tid1, tid2], st1) = UiTest.runMkblock [(">", []), ("*", [])]
        rid = UiTest.defaultRulerId
    let (uiUpdates, displayUpdates) = diff st1 $ Ui.mergeTrack bid 1 2
    equal uiUpdates
        [ Update.Block bid $ Update.BlockTrack 1 $
            Block.Track (Block.TId tid1 rid) 30 30 mempty (Set.singleton tid2)
        , Update.Block bid $ Update.BlockTrack 2 $
            Block.Track (Block.TId tid2 rid) 30 30
                (Set.singleton Block.Merge) mempty
        ]

    let div = Block.DId (Block.Divider Config.abbreviationColor)
    equal displayUpdates
        [ Update.Block bid $ Update.BlockTrack 1 $
            Block.DisplayTrack (Block.TId tid1 rid) 30 (Set.singleton tid2)
                Block.emptyStatus 1
        , Update.Block bid (Update.RemoveTrack 2)
        , Update.Block bid $ Update.InsertTrack 2 $
            Block.DisplayTrack div 3 mempty Block.emptyStatus 1
        ]
    -- TODO add more tests if I modify Diff

test_merge_updates :: Test
test_merge_updates = do
    let ([tid1, tid2], st) = UiTest.run Ui.empty $ do
            tids <- UiTest.mkblock [(">", []), ("*", [])]
            Ui.mergeTrack bid 1 2
            pure tids
    let update = mempty
            { Update._tracks = Map.singleton tid2 Ranges.everything }
    equal (Diff.diff update st st) $
        ( [Update.Track tid2 Update.TrackAllEvents]
        , [ Update.Track tid2 Update.TrackAllEvents
          , Update.Track tid1 Update.TrackAllEvents
          ]
        )

test_track_flags :: Test
test_track_flags = do
    let (_, st) = UiTest.runMkblock [(">", []), ("*", [])]
    -- Make sure adding Solo doesn't cause other damage.
    equal (fst $ diff st ((Ui.addTrackFlag bid 2 Block.Solo)))
        [ Update.Block UiTest.defaultBlockId $ Update.BlockTrack 2 $
            (Block.track (Block.TId (UiTest.mkTid 2) (UiTest.rid "r0")) 30)
                { Block.trackFlags = Set.fromList [Block.Solo] }
        ]

diff :: Ui.State -> Ui.StateId a -> ([Update.UiUpdate], [Update.DisplayUpdate])
diff state1 modify = Diff.diff update state1 state2
    where (_, state2, update) = expectRight $ Ui.runId state1 modify


-- * derive_diff

test_derive_diff :: Test
test_derive_diff = do
    let ([_, tid2], ustate) = UiTest.runMkblock
            [ ("tempo", [(0, 0, ".5")])
            , (">i", [(0, 1, ""), (1, 1, "")])
            ]
    let f = deriveDiff ustate
    -- Track damage.
    equal (f (Ui.setTrackTitle tid2 ">i2"))
        (mkdamage [(tid2, Ranges.everything)] [bid] [])
    equal (f (Ui.setRenderStyle (Track.Filled Nothing) tid2))
        (mkdamage [(tid2, Ranges.everything)] [bid] [])
    -- Block damage.
    equal (f (Ui.setBlockTitle bid "new"))
        (mkdamage [] [] [bid])
    equal (f (Ui.setSkeleton bid (Skeleton.make [(2, 1)])))
        (mkdamage [] [] [bid])
    equal (f (Ui.setSkeleton bid (Skeleton.make [(1, 2)])))
        (mkdamage [] [] [])
    -- Config damage.
    let tlike = Block.TId (UiTest.mkTid 4) Ui.noRuler
    equal (f (Ui.insertTrack bid 3 (Block.track tlike 10)))
        (mkdamage [] [] [bid])
    let modifyConfig = Ui.modifyDefault $ UiConfig.tempo #= 0.5
    equal (f modifyConfig) (mkdamage [] [] [bid])
    -- A config change damages blocks that were removed as well.
    equal (f (modifyConfig >> Ui.destroyBlock bid))
        (mkdamage [] [] [bid])
    -- Add and remove blocks.
    equal (f (UiTest.createBlock (UiTest.mkid "new") "" []))
        (mkdamage [] [] [UiTest.bid "new"])
    equal (f (Ui.destroyBlock bid)) (mkdamage [] [] [bid])

test_derive_diff_track_flags :: Test
test_derive_diff_track_flags = do
    let (_, ustate) = UiTest.runMkblock
            [ ("tempo", [(0, 0, "1")])
            , (">i", [(0, 1, ""), (1, 1, "")])
            ]
    let f = deriveDiff ustate
    equal (f (Ui.addTrackFlag bid 2 Block.Collapse)) (mkdamage [] [] [bid])
    equal (f (Ui.addTrackFlag bid 2 Block.Solo)) (mkdamage [] [] [])
    equal (f (Ui.addTrackFlag bid 2 Block.Disable)) (mkdamage [] [] [bid])

test_derive_diff_updates :: Test
test_derive_diff_updates = do
    let ([_, tid2], ustate) = UiTest.runMkblock
            [ ("tempo", [(0, 0, ".5")])
            , (">i", [(0, 1, ""), (1, 1, "")])
            ]
    let f = Diff.deriveDiff ustate ustate mempty
    equal (f [Update.Track tid2 (Update.TrackEvents 1 2)])
        (mkdamage [(tid2, Ranges.range 1 2)] [bid] [])

bid :: BlockId
bid = UiTest.defaultBlockId

mkdamage :: [(TrackId, Ranges.Ranges ScoreTime)] -> [BlockId] -> [BlockId]
    -> Derive.ScoreDamage
mkdamage tracks trackBlocks blocks = Derive.ScoreDamage
    { sdamageTracks = Map.fromList tracks
    , sdamageTrackBlocks = Set.fromList trackBlocks
    , sdamageBlocks = Set.fromList blocks
    }

deriveDiff :: Ui.State -> Ui.StateId a -> Derive.ScoreDamage
deriveDiff state1 modify = Diff.deriveDiff state1 state2 update []
    where Right (_, state2, update) = Ui.runId state1 modify
