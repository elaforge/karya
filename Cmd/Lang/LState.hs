-- | Lang cmds providing general UI state operations.
module Cmd.Lang.LState where
import qualified Control.Monad.Trans as Trans
import qualified Data.Map as Map
import qualified System.IO as IO

import qualified Util.PPrint as PPrint
import qualified Ui.Id as Id
import qualified Ui.State as State
import qualified Ui.UiTest as UiTest

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Perform.Lilypond.Lilypond as Lilypond
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

rename_project :: String -> Cmd.CmdL ()
rename_project ns = case Id.namespace ns of
    Nothing -> Cmd.throw $ "invalid namespace: " ++ show ns
    Just ns -> Create.rename_project ns


-- * lilypond

-- | Turn on lilypond derivation for this block.
enable_lilypond :: String -> String -> String -> BlockId -> Cmd.CmdL ()
enable_lilypond clef time_sig dur1 block_id = do
    State.modify_block_meta block_id $ Map.union $ Map.fromList
        [ (Lilypond.meta_ly, "true")
        , (Lilypond.meta_title, Id.ident_string block_id)
        , (Lilypond.meta_clef, clef)
        , (Lilypond.meta_time_signature, time_sig)
        , (Lilypond.meta_duration1, dur1)
        ]
