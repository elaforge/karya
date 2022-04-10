-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Env_test where
import Util.Test
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Typecheck as Typecheck
import qualified Derive.ValType as ValType
import Types


test_put_val :: Test
test_put_val = do
    let f key val = either Just (const Nothing) . Env.put_val key val
    equal (f EnvKey.block_end (0 :: ScoreTime) mempty) Nothing
    equal (f EnvKey.block_end (0 :: RealTime) mempty)
        (Just (ValType.TNum ValType.TScoreTime ValType.TAny))
    -- Don't infer that just because someone put in a positive value that
    -- the type must be positive.
    let env = Env.from_list [("k", Typecheck.to_val (1 :: Int))]
    equal (f "k" (-1 :: Int) env) Nothing
