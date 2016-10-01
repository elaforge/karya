-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Skeleton for one-off tests, presumably using the output of
-- LDebug.dump_block.
module Derive.ManualTest where
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Util.Log as Log
import qualified Util.Ranges as Ranges
import Util.Test

import qualified Ui.UiTest as UiTest
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Performance as Performance
import qualified Cmd.PlayUtil as PlayUtil
import qualified Cmd.Simple as Simple

import qualified Derive.Cache_test as Cache_test
import qualified Derive.Derive as Derive
import qualified Derive.DeriveSaved as DeriveSaved
import qualified Derive.DeriveTest as DeriveTest

import qualified Local.Instrument.Kontakt as Kontakt
import Global


run :: IO ()
run = do
    let synth = Kontakt.synth
    let res = DeriveTest.derive_dump [synth] dump (UiTest.bid "thani-s")
        (pevents, msgs, logs) = DeriveTest.perform_dump [synth] dump res
    prettyp $ DeriveTest.extract id res
    prettyp pevents
    prettyp msgs
    -- prettyp logs

dump :: Simple.State
dump = undefined


-- * cache

run_cached = do
    cmd_config <- DeriveSaved.load_cmd_config
    let cstate = Cmd.initial_state cmd_config
    (state, _lib) <- expect_right <$>
        DeriveSaved.load_score (Cmd.config_instrument_db cmd_config) "bug/cache"
    let root = UiTest.bid "kendang-telek/telek1"
        tempot = UiTest.tid "kendang-telek/telek1.t1"
    let (uncached, logs) = Performance.derive state cstate root
    mapM_ Log.write logs

    let cache = Cmd.perf_derive_cache uncached
        damage = Derive.ScoreDamage
            (Map.fromList [(tempot, Ranges.range 200 200)])
            (Set.fromList [root]) mempty
    let (cached, logs) = expect_right $
            DeriveSaved.run_cmd state cstate $
            PlayUtil.derive_block cache damage root
    mapM_ Log.write logs
    prettyp (Cache_test.r_block_logs cached)
