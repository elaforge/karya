-- | Put benchmark functions in a separate module, just in case cross-module
-- makes a difference.
module Derive.Deriver.DeriveMBench where
import qualified Util.Log as Log
import qualified Derive.Deriver.DeriveM as DeriveM

import           Global


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

runCountdown :: Int -> (Int, Int)
runCountdown n = case DeriveM.run (State n) countdown of
    (Right val, State n, []) -> (val, n)
    r -> error $ "unexpected result: " <> show r

countdown :: Deriver Int
countdown = do
    State n <- DeriveM.get
    when (n <= 0) $ do
        DeriveM.write $ Log.msg Log.Debug Nothing "hi"
        DeriveM.throw Error
    if n <= 0 then pure n else do
        DeriveM.put $ State (n - 1)
        countdown
