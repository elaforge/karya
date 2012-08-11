-- | Lang cmds providing general UI state operations.
module Cmd.Lang.LState where
import qualified Control.Monad.Trans as Trans
import qualified System.IO as IO

import qualified Util.PPrint as PPrint
import qualified Ui.Id as Id
import qualified Ui.State as State
import qualified Ui.UiTest as UiTest

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import Types


-- | Save state in a format that can be copy-pasted into a test.
save_test :: FilePath -> Cmd.CmdL ()
save_test fname = do
    st <- State.get
    Trans.liftIO $ IO.writeFile fname $ PPrint.pshow (UiTest.to_spec st)

-- | Save the given block in a format that can be copy-pasted into a test.
save_test_block :: FilePath -> BlockId -> Cmd.CmdL ()
save_test_block fname block_id = do
    st <- State.get
    Trans.liftIO $ IO.writeFile fname $
        PPrint.pshow [UiTest.block_to_spec block_id st]

rename :: String -> Cmd.CmdL ()
rename ns = case Id.namespace ns of
    Nothing -> Cmd.throw $ "invalid namespace: " ++ show ns
    Just ns -> Create.rename_project ns
