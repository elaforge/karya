-- | A stub module that doesn't do a good job at interpreting haskell but does
-- a great job at linking quickly.
module Cmd.ReplStub (
    Session, make_session
    , interpreter, interpret
) where
import Control.Monad
import qualified Util.Thread as Thread
import qualified Cmd.Cmd as Cmd

type Session = ()

make_session :: IO Session
make_session = return ()

interpreter :: Session -> IO ()
interpreter () = forever $ Thread.delay 10

interpret :: Session -> [String] -> String -> IO (Cmd.CmdT IO String)
interpret () _ text = return (return $ "repl not linked in: " ++ show text)
