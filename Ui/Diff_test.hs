module Ui.Diff_test where
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Util.Ranges as Ranges
import Util.Test
import Ui
import qualified Ui.Block as Block
import qualified Ui.Diff as Diff
import qualified Ui.Events as Events
import qualified Ui.Skeleton as Skeleton
import qualified Ui.State as State
import qualified Ui.UiTest as UiTest
import qualified Ui.Update as Update

import qualified Derive.Derive as Derive
import qualified App.Config as Config


test_display_track = do
    let ([tid1, tid2], st1) = UiTest.run_mkstate [(">", []), ("*", [])]
        rid = UiTest.default_ruler_id
        st2 = UiTest.exec st1 (State.merge_track bid 1 2)
    let Right (cmd_updates, display_updates) = diff st1 st2
    equal cmd_updates
        [ Update.BlockUpdate bid (Update.BlockTrack 1
            (Block.Track (Block.TId tid1 rid) 40 [] [tid2]))
        , Update.BlockUpdate bid (Update.BlockTrack 2
            (Block.Track (Block.TId tid2 rid) 40 [Block.Collapse] []))
        ]

    let div = Block.DId (Block.Divider Config.abbreviation_color)
    equal display_updates
        [ Update.BlockUpdate bid $ Update.BlockTrack 1 $
            Block.DisplayTrack (Block.TId tid1 rid) 40 [tid2] Nothing 1
        , Update.BlockUpdate bid (Update.RemoveTrack 2)
        , Update.BlockUpdate bid $ Update.InsertTrack 2 $
            Block.DisplayTrack div 3 [] Nothing 1
        ]
    -- TODO add more tests if I modify Diff

test_merge_updates = do
    let ([tid1, tid2], st) = UiTest.run State.empty $ do
            tids <- UiTest.mkstate UiTest.default_block_name
                [(">", []), ("*", [])]
            State.merge_track bid 1 2
            return tids
    let all_events = Update.TrackAllEvents Events.empty
    equal (Diff.diff [Update.TrackUpdate tid2 all_events] st st) $
        Right
            ([Update.TrackUpdate tid2 all_events],
            [ Update.TrackUpdate tid2 all_events
            , Update.TrackUpdate tid1 all_events
            ])

diff :: State.State -> State.State
    -> Either String ([Update.CmdUpdate], [Update.DisplayUpdate])
diff = Diff.diff []


-- * derive_diff

test_derive_diff = do
    let ([_, tid2], ustate) = UiTest.run_mkstate
            [ ("tempo", [(0, 0, ".5")])
            , (">i", [(0, 1, ""), (1, 1, "")])
            ]
    let f modify = Diff.derive_diff ustate (UiTest.exec ustate modify) []
    equal (f (State.set_track_title tid2 ">i2"))
        (mkdamage [(tid2, Ranges.everything)] [bid] [])
    equal (f (State.set_block_title bid "new"))
        (mkdamage [] [] [bid])
    equal (f (State.set_skeleton bid (Skeleton.make [(1, 0)])))
        (mkdamage [] [] [bid])
    equal (f (State.set_skeleton bid (Skeleton.make [(1, 2)])))
        (mkdamage [] [] [])
    equal (f (State.add_track_flag bid 2 Block.Collapse))
        (mkdamage [] [] [])
    equal (f (State.add_track_flag bid 2 Block.Mute))
        (mkdamage [] [] [bid])

test_derive_diff_updates = do
    let ([_, tid2], ustate) = UiTest.run_mkstate
            [ ("tempo", [(0, 0, ".5")])
            , (">i", [(0, 1, ""), (1, 1, "")])
            ]
    let f = Diff.derive_diff ustate ustate
    equal (f [Update.TrackUpdate tid2 (Update.TrackEvents 1 2 Events.empty)])
        (mkdamage [(tid2, Ranges.range 1 2)] [bid] [])

bid = UiTest.default_block_id

mkdamage :: [(TrackId, Ranges.Ranges ScoreTime)] -> [BlockId] -> [BlockId]
    -> Derive.ScoreDamage
mkdamage tracks track_blocks blocks =
    Derive.ScoreDamage (Map.fromList tracks)
        (Set.fromList track_blocks) (Set.fromList blocks)
