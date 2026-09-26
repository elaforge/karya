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

profileNormal = do
    let uiState = UiTest.exec Ui.empty
            (Derive_profile.makeNestedControls 10 3 128)
        modify block pos = modifyNote ("b1.0." <> showt block <> ".t0")
            ("b1.0." <> showt block <> ".t1") pos
    rederive uiState [modify 0 2, modify 1 0, modify 4 4]

profileSmall = do
    let uiState = UiTest.exec Ui.empty
            (Derive_profile.makeNestedControls 4 3 128)
    -- pprint (Map.keys (Ui.state_tracks ui_state))
    rederive uiState [modifyPitch "b1.0.0.t1" 2]

modifyNote :: Ui.M m => Text -> Text -> ScoreTime -> m ()
modifyNote noteTid pitchTid pos = do
    Ui.insertEvent (UiTest.tid noteTid) (Event.event pos 1 "")
    Ui.insertEvent (UiTest.tid pitchTid) (Event.event pos 0 "1c")

modifyPitch :: Ui.M m => Text -> ScoreTime -> m ()
modifyPitch pitchTid pos =
    Ui.insertEvent (UiTest.tid pitchTid) (Event.event pos 0 "1c")

-- | Run the state transform a few times and rederive each time.
rederive :: Ui.State -> [Ui.StateId ()] -> IO ()
rederive initialState modifications = do
    go initialState mempty (pure () : modifications)
    where
    go _ _ [] = pure ()
    go state1 cache (modify:rest) = do
        let (_, state2, uiDamage) = Cache_test.run state1 modify
        cached <- timeSection "cached" $
            evalDerivation cache state1 state2 uiDamage
        uncached <- timeSection "uncached" $ do
            let result = DeriveTest.deriveBlockStandard mempty
                    DeriveTest.defaultCmdState mempty mempty state2
                    (UiTest.bid "b1")
            Thread.force $ Derive.rEvents result
            pure result
        equal (Cache_test.diffEvents cached uncached) []
        go state2 (Derive.rCache cached) rest

timeSection :: String -> IO a -> IO a
timeSection title op = do
    putStr $ "--> " ++ title ++ ": "
    IO.hFlush IO.stdout
    (val, metric) <- Thread.timeAction op
    Printf.printf "%.2f\n" (realToFrac (Thread.metricCpu metric) :: Double)
    pure val

evalDerivation :: Derive.Cache -> Ui.State -> Ui.State -> Update.UiDamage
    -> IO Derive.Result
evalDerivation cache state1 state2 uiDamage = do
    Thread.force $ Derive.rEvents result
    pure result
    where
    (uiUpdates, _) = Diff.diff uiDamage state1 state2
    damage = Diff.deriveDiff state1 state2 uiDamage uiUpdates
    result = DeriveTest.deriveBlockStandard mempty
        DeriveTest.defaultCmdState cache damage state2 (UiTest.bid "b1")
