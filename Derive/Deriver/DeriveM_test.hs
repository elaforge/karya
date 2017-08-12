-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Deriver.DeriveM_test where
import qualified Control.Monad.Except as Except

import qualified Util.Log as Log
import Util.Test
import qualified Derive.Deriver.DeriveM as DeriveM
import Global


type State = Int

run :: DeriveM.Deriver State Text Text -> (Either Text Text, State, [Text])
run d = (result, state, map Log.msg_text logs)
    where (result, state, logs) = DeriveM.run 0 d

test_monad_error = do
    equal (run $ return "a") (Right "a", 0, [])
    equal (run $ Except.throwError "x") (Left "x", 0, [])
    equal (run $ Except.throwError "x" `Except.catchError` return)
        (Right "x", 0, [])
    -- Logs and state are preserved.
    let notice = DeriveM.write (Log.msg Log.Notice Nothing "notice")
    equal (run $ DeriveM.modify (+1) >> notice >> Except.throwError "x")
        (Left "x", 1, ["notice"])
    equal (run $ DeriveM.modify (+1) >> notice
            >> Except.throwError "x" `Except.catchError` return)
        (Right "x", 1, ["notice"])
