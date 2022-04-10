-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Shake.CcDeps_test where
import qualified Data.Set as Set
import qualified Development.Shake as Shake

import Util.Test
import qualified Util.Test.Testing as Testing
import qualified Shake.CcDeps as CcDeps
import qualified Shake.Util as Util


test_includesOf :: Test
test_includesOf = Testing.in_tmp_dir "CcDeps" $ do
    let includesOf gen = CcDeps.includesOf gen []
        transitive gen = CcDeps.transitiveIncludesOf gen []

    writeFile "a.h" "#include \"b.h\"\n"
    writeFile "b.h"
        "#include \"c.h\"\n\
        \#include <sys.h>\n"

    io_equal (run $ includesOf mempty "a.h") (["b.h"], [])
    io_equal (run $ includesOf mempty "b.h") ([], ["c.h"])
    io_equal (run $ includesOf (Set.singleton "c.h") "b.h") (["c.h"], [])
    io_equal (run $ transitive mempty "a.h") (["a.h", "b.h"], ["c.h"])

run :: Shake.Action a -> IO a
run action = do
    dir <- Testing.tmp_dir "shake"
    Util.runIO dir action
