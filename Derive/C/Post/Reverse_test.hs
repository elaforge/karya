-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.C.Post.Reverse_test where
import Util.Test
import qualified Derive.DeriveTest as DeriveTest


test_reverse = do
    let run sub_tracks = extract $ DeriveTest.derive_blocks
            [ ("top=ruler",
                [ ("tempo", [(0, 0, "1")])
                , (">", [(1, 4, "reverse | sub")])
                ])
            , ("sub=ruler", sub_tracks)
            ]
        extract = DeriveTest.extract DeriveTest.e_note
    equal (run
        [ (">", [(0, 1, "")])
        , ("*", [(0, 0, "4c")])
        , (">", [(1, 1, "")])
        , ("*", [(1, 1, "4d")])
        ])
        ([(1, 2, "4d"), (3, 2, "4c")], [])
    equal (run
        [ (">", [(0, 1, ""), (1, 3, "")])
        , ("*", [(0, 0, "4c"), (1, 0, "4d")])
        , (">", [(0, 3, ""), (3, 1, "")])
        , ("*", [(0, 0, "5c"), (3, 0, "5d")])
        ])
        ([(1, 3, "4d"), (1, 1, "5d"), (2, 3, "5c"), (4, 1, "4c")], [])
