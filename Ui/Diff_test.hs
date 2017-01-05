-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Ui.Diff_test where
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Util.Ranges as Ranges
import Util.Test
import qualified Ui.Block as Block
import qualified Ui.Diff as Diff
import qualified Ui.Skeleton as Skeleton
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig
import qualified Ui.Track as Track
import qualified Ui.UiTest as UiTest
import qualified Ui.Update as Update

import qualified Derive.Derive as Derive
import qualified App.Config as Config
import Global
import Types


test_display_track = do
    let ([tid1, tid2], st1) = UiTest.run_mkblock [(">", []), ("*", [])]
        rid = UiTest.default_ruler_id
        st2 = UiTest.exec st1 (Ui.merge_track bid 1 2)
    let (ui_updates, display_updates) = diff st1 st2
    equal ui_updates
        [ Update.Block bid $ Update.BlockTrack 1 $
            Block.Track (Block.TId tid1 rid) 30 mempty (Set.singleton tid2)
        , Update.Block bid $ Update.BlockTrack 2 $
            Block.Track (Block.TId tid2 rid) 30
                (Set.singleton Block.Collapse) mempty
        ]

    let div = Block.DId (Block.Divider Config.abbreviation_color)
    equal display_updates
        [ Update.Block bid $ Update.BlockTrack 1 $
            Block.DisplayTrack (Block.TId tid1 rid) 30 (Set.singleton tid2)
                Nothing 1
        , Update.Block bid (Update.RemoveTrack 2)
        , Update.Block bid $ Update.InsertTrack 2 $
            Block.DisplayTrack div 3 mempty Nothing 1
        ]
    -- TODO add more tests if I modify Diff

test_merge_updates = do
    let ((_, [tid1, tid2]), st) = UiTest.run Ui.empty $ do
            tids <- UiTest.mkblock (UiTest.default_block_name,
                [(">", []), ("*", [])])
            Ui.merge_track bid 1 2
            return tids
    equal (Diff.diff [Update.CmdTrackAllEvents tid2] st st) $
        ([Update.Track tid2 Update.TrackAllEvents],
        [ Update.Track tid2 Update.TrackAllEvents
        , Update.Track tid1 Update.TrackAllEvents
        ])

diff :: Ui.State -> Ui.State -> ([Update.UiUpdate], [Update.DisplayUpdate])
diff = Diff.diff []


-- * derive_diff

test_derive_diff = do
    let ([_, tid2], ustate) = UiTest.run_mkblock
            [ ("tempo", [(0, 0, ".5")])
            , (">i", [(0, 1, ""), (1, 1, "")])
            ]
    let f modify = Diff.derive_diff ustate (UiTest.exec ustate modify) []
    -- Track damage.
    equal (f (Ui.set_track_title tid2 ">i2"))
        (mkdamage [(tid2, Ranges.everything)] [bid] [])
    equal (f (Ui.set_render_style (Track.Filled Nothing) tid2))
        (mkdamage [(tid2, Ranges.everything)] [bid] [])
    -- Block damage.
    equal (f (Ui.set_block_title bid "new"))
        (mkdamage [] [] [bid])
    equal (f (Ui.set_skeleton bid (Skeleton.make [(2, 1)])))
        (mkdamage [] [] [bid])
    equal (f (Ui.set_skeleton bid (Skeleton.make [(1, 2)])))
        (mkdamage [] [] [])
    equal (f (Ui.add_track_flag bid 2 Block.Collapse))
        (mkdamage [] [] [])
    -- Config damage.
    let tlike = Block.TId (UiTest.mk_tid 4) Ui.no_ruler
    equal (f (Ui.insert_track bid 3 (Block.track tlike 10)))
        (mkdamage [] [] [bid])
    let modify_config = Ui.modify_default $ UiConfig.tempo #= 0.5
    equal (f modify_config) (mkdamage [] [] [bid])
    -- A config change damages blocks that were removed as well.
    equal (f (modify_config >> Ui.destroy_block bid))
        (mkdamage [] [] [bid])
    -- Add and remove blocks.
    equal (f (UiTest.create_block (UiTest.mkid "new") "" []))
        (mkdamage [] [] [UiTest.bid "new"])
    equal (f (Ui.destroy_block bid)) (mkdamage [] [] [bid])

test_derive_diff_updates = do
    let ([_, tid2], ustate) = UiTest.run_mkblock
            [ ("tempo", [(0, 0, ".5")])
            , (">i", [(0, 1, ""), (1, 1, "")])
            ]
    let f = Diff.derive_diff ustate ustate
    equal (f [Update.Track tid2 (Update.TrackEvents 1 2)])
        (mkdamage [(tid2, Ranges.range 1 2)] [bid] [])

bid :: BlockId
bid = UiTest.default_block_id

mkdamage :: [(TrackId, Ranges.Ranges ScoreTime)] -> [BlockId] -> [BlockId]
    -> Derive.ScoreDamage
mkdamage tracks track_blocks blocks =
    Derive.ScoreDamage (Map.fromList tracks)
        (Set.fromList track_blocks) (Set.fromList blocks)
