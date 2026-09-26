-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | A wrapper around 'Ui.StateT' that provides logging.
module Ui.UiLog where
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.Trans as Trans

import qualified Util.Log as Log
import qualified Ui.Ui as Ui
import qualified Ui.Update as Update
import Global


type UiLogT m = Ui.StateT (Log.LogT m)
type UiLog = UiLogT Identity.Identity

run :: Monad m => Ui.State -> UiLogT m a
    -> m (Either Ui.Error (a, Ui.State, Update.UiDamage), [Log.Msg])
run state = Log.run . Ui.run state

runId :: Ui.State -> UiLog a
    -> (Either Ui.Error (a, Ui.State, Update.UiDamage), [Log.Msg])
runId state = Identity.runIdentity . run state

execId :: Ui.State -> UiLog a
    -> (Either Ui.Error (Ui.State, Update.UiDamage), [Log.Msg])
execId state = first (fmap extract) . runId state
    where extract (_, state, updates) = (state, updates)

instance Monad m => Log.LogMonad (UiLogT m) where
    write = Trans.lift . Log.write
