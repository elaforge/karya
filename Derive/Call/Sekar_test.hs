-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.Sekar_test where
import Util.Test
import qualified Derive.DeriveTest as DeriveTest


test_sekar = do
    let extract = DeriveTest.extract DeriveTest.e_note
        run sekar_event notes = extract $ DeriveTest.linear_derive_tracks id
            [ (">", [sekar_event])
            , (">", [(start, dur, "") | (start, dur, _) <- notes])
            , ("*twelve", [(start, 0, p) | (start, _, p) <- notes])
            ]

    equal (run (0, 4, "sekar 'a b'") [])
        ([], ["Error: pattern chars must be a-z: \"a b\""])
    equal (run (0, 4, "sekar 'ab'") [(0, 1, "4c"), (1, 1, "4d")])
        ([(0, 2, "4c"), (2, 2, "4d")], [])
    equal (run (0, 4, "sekar 'abab'") [(0, 1, "4c"), (1, 1, "4d")])
        ([(0, 1, "4c"), (1, 1, "4d"), (2, 1, "4c"), (3, 1, "4d")], [])
    equal (run (0, 4, "sekar 'abAb'") [(0, 1, "4c"), (1, 1, "4d")])
        ([(0, 1, "4c"), (1, 1, "4d"), (3, 1, "4d")], [])
