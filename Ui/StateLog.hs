-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-- | A wrapper around 'State.StateT' that provides logging.
module Ui.StateLog where
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.Trans as Trans

import Util.Control
import qualified Util.Log as Log
import qualified Ui.State as State
import qualified Ui.Update as Update


type StateLogT m = State.StateT (Log.LogT m)
type StateLog = StateLogT Identity.Identity

run :: Monad m => State.State -> StateLogT m a
    -> m (Either State.Error (a, State.State, [Update.CmdUpdate]), [Log.Msg])
run state = Log.run . State.run state

run_id :: State.State -> StateLog a
    -> (Either State.Error (a, State.State, [Update.CmdUpdate]), [Log.Msg])
run_id state = Identity.runIdentity . run state

exec_id :: State.State -> StateLog a
    -> (Either State.Error (State.State, [Update.CmdUpdate]), [Log.Msg])
exec_id state = first (fmap extract) . run_id state
    where extract (_, state, updates) = (state, updates)

instance Monad m => Log.LogMonad (StateLogT m) where
    write = Trans.lift . Log.write
