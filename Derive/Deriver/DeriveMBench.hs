-- Copyright 2022 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Put benchmark functions in a separate module, just in case cross-module
-- makes a difference.
module Derive.Deriver.DeriveMBench where
import           Control.Monad (when)

import qualified Util.Log as Log
import qualified Derive.Deriver.DeriveM as DeriveM


runRefCountdown :: Int -> (Int, Int)
runRefCountdown = program
    where
    program n
        | n <= 0 = (n, n)
        | otherwise = program (n - 1)

newtype State = State { _val :: Int }
    deriving (Eq, Show)
data Error = Error deriving (Show)

type Deriver = DeriveM.Deriver State Error

run :: Deriver Int -> Int -> (Int, Int)
run deriver n = case DeriveM.run (State n) deriver of
    (Right val, State n, []) -> (val, n)
    (Left Error, State 0, [_msg]) -> (0, 0)
    r -> error $ "unexpected result: " <> show r

countdown :: Deriver Int
countdown = do
    State n <- DeriveM.get
    if n <= 0 then pure n else do
        DeriveM.put $ State (n - 1)
        countdown

countdownThrow :: Deriver Int
countdownThrow = do
    State n <- DeriveM.get
    when (n <= 0) $ do
        DeriveM.write $ Log.msg Log.Debug Nothing "hi"
        DeriveM.throw Error
    if n <= 0 then pure n else do
        DeriveM.put $ State (n - 1)
        countdownThrow
