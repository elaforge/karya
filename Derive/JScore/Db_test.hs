-- Copyright 2026 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.JScore.Db_test where
import qualified Derive.JScore.Db as Db

import           Util.Test.Global


-- TODO mostly this just ensures Db can compile.
test_load :: Test
test_load = do
    entries <- expectRight <$> Db.load "Example/jscore/balungan32.jscore"
    notEqual (length entries) 0
    Db.printEntries entries
