-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.BlockConfig_test where
import Util.Test
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest
import qualified Cmd.BlockConfig as BlockConfig
import Global


test_move_tracks = do
    let f state sources dest = extract $
            UiTest.exec state $
            BlockConfig.move_tracks UiTest.default_block_id sources dest
        extract st = (mconcatMap fst $ UiTest.extract_tracks st,
            UiTest.extract_skeleton st)

    -- Testing skeleton seems extra, but I might not have other tests that
    -- skeleton works right with Ui.move_track.
    let skel_state skel = UiTest.exec Ui.empty $ UiTest.mkblocks_skel $ (:[]) $
                ( ( UiTest.default_block_name
                  , [(showt n, []) | n <- [1..4]]
                  )
                , skel
                )
    -- Move up.
    equal (f (skel_state [(2, 3)]) [1] 3) ("2314", [(1, 2)])
    equal (f (skel_state [(3, 4)]) [1, 2] 3) ("3124", [(1, 4)])
    -- Move down.
    equal (f (skel_state [(2, 3)]) [3] 1) ("3124", [(3, 1)])
    equal (f (skel_state [(2, 3)]) [3] 1) ("3124", [(3, 1)])

    -- The moved tracks are tweaked to avoid splitting up merged tracks.
    let merged = UiTest.exec Ui.empty $ do
            UiTest.mkblock [(showt n, []) | n <- [1..4]]
            Ui.merge_track UiTest.default_block_id 1 2
            Ui.merge_track UiTest.default_block_id 3 4
    equal (fst $ f merged [1, 2] 3) "3412"
    equal (fst $ f merged [1, 2] 4) "3412"
    equal (fst $ f merged [3, 4] 1) "3412"
    equal (fst $ f merged [3, 4] 2) "3412"
