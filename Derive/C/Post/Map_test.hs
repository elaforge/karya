-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.C.Post.Map_test where
import Util.Test
import qualified Derive.DeriveTest as DeriveTest


test_mapc :: Test
test_mapc = do
    let run extract = DeriveTest.extract extract . DeriveTest.deriveTracks ""
    stringsLike
        (snd $ run (DeriveTest.eControl "c")
            [("> | mapc in_valid \"(smooth 2)", [(0, 8, "")])])
        ["expected Control * or PControl"]
    equal (run (DeriveTest.eControl "c")
        [ ("> | mapc c \"(smooth 2)", [(0, 8, "")])
        , ("c", [(0, 0, "0"), (4, 0, "1")])
        ])
        ([[(0, 0), (4, 0), (6, 1)]], [])
