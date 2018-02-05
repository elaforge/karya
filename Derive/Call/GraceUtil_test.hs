-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.GraceUtil_test where
import qualified Data.Map as Map

import Util.Test
import qualified Derive.Attrs as Attrs
import qualified Derive.Call.CallTest as CallTest
import qualified Derive.Call.GraceUtil as GraceUtil
import qualified Derive.DeriveTest as DeriveTest
import Global


test_grace_attr = do
    let run note = DeriveTest.extract extract $
            DeriveTest.derive_tracks_setup setup_call "import europe"
                [ ("> | %legato-overlap = .5 | grace-dur = 1", [note])
                , ("*", [(0, 0, "4c")])
                ]
        extract e = (DeriveTest.e_start_dur e, DeriveTest.e_pitch e,
            DeriveTest.e_attributes e)
        setup_call =
            CallTest.with_note_generator "g" (GraceUtil.c_attr_grace graces)
    -- Attrs when it can.
    equal (run (0, 1, "g (3bb)"))
        ([((-1, 2), "4c", "+up+whole")], [])
    equal (run (0, 1, "g 1c"))
        ([((-1, 2), "4c", "+down+half")], [])
    -- Notes when it can't.
    equal (run (0, 1, "g (4a)"))
        ([((-1, 1.5), "4a", "+"), ((0, 1), "4c", "+")], [])
    where
    graces :: Map Int Attrs.Attributes
    graces = Map.fromList
        [ (-1, Attrs.attrs ["half", "down"])
        , (2, Attrs.attrs ["whole", "up"])
        ]

test_fit_grace = do
    let f place notes = GraceUtil.fit_grace place (Just 0) 2 4 notes 1
    equal (f 0 4) [0.5, 1, 1.5, 2]
    equal (f 1 4) [2, 2.5, 3, 3.5]
    equal (f 0.5 4) [1.25, 1.75, 2.25, 2.75]
