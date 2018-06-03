-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Shake.HsDeps_test where
import qualified Data.Set as Set
import qualified Development.Shake as Shake
import qualified System.Directory as Directory

import Util.Test
import qualified Util.Test.Testing as Testing
import qualified Shake.HsDeps as HsDeps
import qualified Shake.Util as Util


test_importsOf = Testing.in_tmp_dir "HsDeps" $ do
    let importsOf generated hs = run $
            HsDeps.importsOf (mkGen generated) Nothing hs
        transitive generated hs = run $
            HsDeps.transitiveImportsOf (mkGen generated) (const Nothing) hs
        mkGen g = HsDeps.Generated (Set.fromList g) [".hsc"]
    makeFiles
    io_equal (importsOf [] "A.hs") ["M/B.hs"]
    io_equal (transitive [] "A.hs") ["A.hs", "M/B.hs"]

    -- I assume unknown imports are from external packages.
    io_equal (importsOf [] "M/B.hs") []

makeFiles :: IO ()
makeFiles = do
    writeFile "A.hs"
        "module A where\n\
        \import M.B\n\
        \import System\n\
        \#include \"c.h\"\n\
        \#include <sys.h>\n"
    Directory.createDirectory "M"
    writeFile "M/B.hs" "import C\n"

run :: Shake.Action a -> IO a
run action = do
    dir <- Testing.tmp_dir "shake"
    Util.runIO dir action
