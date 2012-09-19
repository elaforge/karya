-- | Lang cmds providing general UI state operations.
module Cmd.Lang.LState where
import qualified Control.Monad.Trans as Trans
import qualified System.IO as IO

import Util.Control
import qualified Util.PPrint as PPrint
import qualified Ui.Id as Id
import qualified Ui.State as State
import qualified Ui.UiTest as UiTest

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Derive.Score as Score
import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import Types


-- * configure

set_default_tempo :: Signal.Y -> Cmd.CmdL ()
set_default_tempo t =
    State.modify_default $ \d -> d { State.default_tempo = t }

set_default_inst :: String -> Cmd.CmdL ()
set_default_inst inst = State.modify_default $ \d ->
    d { State.default_instrument = Just (Score.Instrument inst) }

set_default_scale :: String -> Cmd.CmdL ()
set_default_scale scale = State.modify_default $ \d ->
    d { State.default_scale = Pitch.ScaleId scale }

set_global_transform :: String -> Cmd.CmdL ()
set_global_transform = State.modify . (State.config#State.global_transform #=)

get_global_transform :: Cmd.CmdL String
get_global_transform = State.config#State.global_transform <#> State.get

-- * transform

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
