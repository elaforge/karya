-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module App.ReplProtocol_test where
import qualified Util.Log as Log
import qualified Util.Serialize as Serialize
import Util.Test

import App.ReplProtocol (Query(..), Response(..), CmdResult(..), Result(..))

test_serialize = do
    let f a expected = (expected a, round_trip a)
    uncurry equal (f QSaveFile Right)
    uncurry equal (f (QCommand "hi") Right)
    uncurry equal (f (RSaveFile (Just "path")) Right)

    let cmd result logs = RCommand (CmdResult result logs)
    uncurry equal (f (cmd (Format "()") []) Right)
    uncurry equal (f (cmd (Raw "blah") [Log.msg Log.Notice Nothing "hi"]) Right)
    uncurry equal (f (cmd (Format "blah") []) Right)

round_trip :: Serialize.Serialize a => a -> Either String a
round_trip = Serialize.decode . Serialize.encode
