-- Copyright 2022 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Deriver.DeriveM_criterion where
import qualified Criterion
import qualified Criterion.Main
import qualified System.Environment as Environment
import qualified Text.Read as Read

import qualified Derive.Deriver.DeriveMBench as DeriveMBench


-- Try to see if there is overhead in DeriveM.
-- Either I'm doing it wrong, or there is none.
main :: IO ()
main = Environment.getArgs >>= \case
    [benchs, ns] -> do
        let Just n = Read.readMaybe ns
        let Just bench = lookup benchs benchmarks
        print (bench (10^n))
    _ -> Criterion.Main.defaultMain
        [ Criterion.bgroup name $ map (mkBench bench) ns
        | (name, bench) <- benchmarks
        ]
    where
    mkBench bench n = Criterion.bench (show n) (Criterion.nf bench n)
    ns = [10^2, 10^3, 10^4, 10^5, 10^6]

benchmarks :: [(String, Int -> (Int, Int))]
benchmarks =
    [ ("reference", DeriveMBench.runRefCountdown)
    , ("countdown", DeriveMBench.run DeriveMBench.countdown)
    , ("throw", DeriveMBench.run DeriveMBench.countdownThrow)
    ]
