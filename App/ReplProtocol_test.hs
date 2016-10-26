-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module App.ReplProtocol_test where
import qualified Util.Log as Log
import qualified Util.Serialize as Serialize
import Util.Test

import qualified App.ReplProtocol as ReplProtocol


test_serialize = do
    let f a expected = (expected a, round_trip a)
    uncurry equal (f ReplProtocol.QSaveFile Right)
    uncurry equal (f (ReplProtocol.QCommand "hi") Right)
    uncurry equal (f (ReplProtocol.RSaveFile (Just "path")) Right)

    let cmd result logs =
            ReplProtocol.RCommand (ReplProtocol.CmdResult result logs)
    uncurry equal (f (cmd (ReplProtocol.Raw "blah")
            [Log.msg Log.Notice Nothing "hi"]) Right)
    uncurry equal (f (cmd (ReplProtocol.Format "blah") []) Right)

round_trip :: Serialize.Serialize a => a -> Either String a
round_trip = Serialize.decode . Serialize.encode
