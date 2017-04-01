-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.Prelude.Conditional_test where
import Util.Test
import qualified Derive.DeriveTest as DeriveTest

import Global


test_c_if_c = do
    let run control pitch = DeriveTest.extract DeriveTest.e_note $
            DeriveTest.derive_tracks ""
                [ ("c", [(0, 0, showt control)])
                , (">", [(0, 1, "")])
                , ("*", [(0, 0, pitch)])
                ]
    equal (run 0 "if-c< c 1 '4c' '4d'") ([(0, 1, "4c")], [])
    equal (run 1 "if-c< c 1 '4c' '4d'") ([(0, 1, "4d")], [])
    equal (run 0 "if-c> c 1 '4c' '4d'") ([(0, 1, "4d")], [])
    equal (run 2 "if-c> c 1 '4c' '4d'") ([(0, 1, "4c")], [])
