module Util.Logger_profile where
import qualified Control.Monad.State.Strict as State
import qualified Control.Monad.Identity as Identity

import qualified Util.Logger as Logger

depth = 10

-- type Logs a = [a]
-- to_logs = (:[])
-- from_logs = id

profile_no_anything = runprof (return ())
profile_logging_no_state = runprof (Logger.log 1)
profile_no_logging = runprof increment
profile_logging = runprof (increment >> Logger.log 1)

type Log a = State.StateT Int (Logger.LoggerT Int Identity.Identity) a

increment :: Log ()
increment = do
    s <- State.get
    State.put $! (s+1)
-- increment = State.modify $! (+1)

runprof m = do
    let ((val, state), logs) = run $ run_lots depth m
    putStrLn $ "logs: " ++ show (length logs)
    putStrLn $ "values: " ++ show (take 10 logs)
    putStrLn $ "(val, state): " ++ show (val, state)

run :: Log a -> ((a, Int), [Int])
run = Identity.runIdentity . Logger.run . flip State.runStateT 0

run_lots :: Int -> Log () -> Log Int
run_lots depth m
    | depth <= 0 = m >> (return $! 1)
    | otherwise = do
        x1 <- run_lots (depth-1) m
        x2 <- run_lots (depth-1) m
        x3 <- run_lots (depth-1) m
        x4 <- run_lots (depth-1) m
        return $! (1 + x1 + x2 + x3 + x4)
