-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.Import_test where
import Util.Test
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score


test_import = do
    let run title = DeriveTest.extract Score.event_start
            . DeriveTest.derive_tracks title
    strings_like (snd (run "" [(">", [(0, 1, "X")])]))
        ["note generator not found"]
    equal (run "import bali.reyong" [(">", [(0, 1, "X")])]) ([0, 0, 0, 0], [])
    strings_like
        (snd (run "import europe no-such-module" [(">", [(0, 1, "")])]))
        ["no calls in the imported module"]
