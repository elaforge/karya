-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | A stub module that doesn't do a good job at interpreting haskell but does
-- a great job at linking quickly.
module Cmd.ReplStub (
    Session, make_session
    , interpreter, interpret
) where
import Control.Monad

import qualified Util.Thread as Thread
import qualified Cmd.Cmd as Cmd
import qualified App.ReplUtil as ReplUtil
import Global

type Session = ()

make_session :: IO Session
make_session = return ()

interpreter :: Session -> IO ()
interpreter () = forever $ Thread.delay 10

interpret :: Session -> [String] -> Text -> IO (Cmd.CmdT IO ReplUtil.Response)
interpret () _ text = return $ return
    (ReplUtil.Raw $ "repl not linked in: " <> showt text, [])
