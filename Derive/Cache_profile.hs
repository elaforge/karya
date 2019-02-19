-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Profile performance with and without the cache.
--
-- This module is about performance, correctness is tested in
-- "Derive.Cache_test".
module Derive.Cache_profile where
import qualified System.IO as IO
import qualified Text.Printf as Printf

import Util.Test
import qualified Util.Thread as Thread
import qualified Ui.Diff as Diff
import qualified Ui.Event as Event
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest
import qualified Ui.Update as Update

import qualified Derive.Cache_test as Cache_test
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Derive_profile as Derive_profile

import Global
import Types


-- TODO this has an endless loop somewhere, since it seems to take forever

profile_normal = do
    let ui_state = UiTest.exec Ui.empty
            (Derive_profile.make_nested_controls 10 3 128)
        modify block pos = modify_note ("b1.0." <> showt block <> ".t0")
            ("b1.0." <> showt block <> ".t1") pos
    rederive ui_state [modify 0 2, modify 1 0, modify 4 4]

profile_small = do
    let ui_state = UiTest.exec Ui.empty
            (Derive_profile.make_nested_controls 4 3 128)
    -- pprint (Map.keys (Ui.state_tracks ui_state))
    rederive ui_state [modify_pitch "b1.0.0.t1" 2]

modify_note :: Ui.M m => Text -> Text -> ScoreTime -> m ()
modify_note note_tid pitch_tid pos = do
    Ui.insert_event (UiTest.tid note_tid) (Event.event pos 1 "")
    Ui.insert_event (UiTest.tid pitch_tid) (Event.event pos 0 "1c")

modify_pitch :: Ui.M m => Text -> ScoreTime -> m ()
modify_pitch pitch_tid pos =
    Ui.insert_event (UiTest.tid pitch_tid) (Event.event pos 0 "1c")

-- | Run the state transform a few times and rederive each time.
rederive :: Ui.State -> [Ui.StateId ()] -> IO ()
rederive initial_state modifications = do
    go initial_state mempty (return () : modifications)
    where
    go _ _ [] = return ()
    go state1 cache (modify:rest) = do
        let (_, state2, cmd_updates) = Cache_test.run state1 modify
        cached <- time_section "cached" $
            eval_derivation cache state1 state2 cmd_updates
        uncached <- time_section "uncached" $ do
            let result = DeriveTest.derive_block_standard mempty
                    DeriveTest.default_cmd_state mempty mempty state2
                    (UiTest.bid "b1")
            Thread.force $ Derive.r_events result
            return result
        equal (Cache_test.diff_events cached uncached) []
        go state2 (Derive.r_cache cached) rest

time_section :: String -> IO a -> IO a
time_section title op = do
    putStr $ "--> " ++ title ++ ": "
    IO.hFlush IO.stdout
    (val, metric) <- Thread.timeAction op
    Printf.printf "%.2f\n" (realToFrac (Thread.metricCpu metric) :: Double)
    return val

eval_derivation :: Derive.Cache -> Ui.State -> Ui.State
    -> [Update.CmdUpdate] -> IO Derive.Result
eval_derivation cache state1 state2 cmd_updates = do
    Thread.force $ Derive.r_events result
    return result
    where
    (ui_updates, _) = Diff.diff cmd_updates state1 state2
    damage = Diff.derive_diff state1 state2 ui_updates
    result = DeriveTest.derive_block_standard mempty
        DeriveTest.default_cmd_state cache damage state2 (UiTest.bid "b1")
