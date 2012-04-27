-- | A stub module that doesn't do a good job at interpreting haskell but does
-- a great job at linking quickly.
module Cmd.LangStub (
    Session, make_session
    , interpreter, interpret
) where
import Control.Monad
import qualified Util.Thread as Thread
import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd

type Session = ()

make_session :: IO Session
make_session = return ()

interpreter :: Session -> IO ()
interpreter () = forever $ Thread.delay 10

interpret :: Session -> [String] -> State.State
    -> Cmd.State -> String -> IO (Cmd.CmdT IO String)
interpret () _ _ _ text = return (return $ "repl not linked in: " ++ show text)
