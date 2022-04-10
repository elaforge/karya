-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Repl.LNote_test where
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Repl.LNote as LNote


test_sort_on_pitch :: Test
test_sort_on_pitch = do
    let run high_left tracks =
            CmdTest.e_pitch_tracks <$>
                CmdTest.run_tracks_with_performance tracks cmd
            where cmd = CmdTest.select_all >> LNote.sort_on_pitch high_left
    let mktracks = concatMap UiTest.note_track
    let c_to_e = mktracks
            [ [(0, 1, "4c")]
            , [(1, 1, "4d")]
            , [(0, 4, "4e")]
            ]
    -- 0   1   2   3   4
    -- c-->
    --     d-->
    -- e-------------->
    io_equal (run False c_to_e) $ Right
        ([[(0, 1, "4c"), (1, 1, "4d")], [(0, 4, "4e")]], [])
    io_equal (run True c_to_e) $ Right
        ([[(0, 4, "4e")], [(0, 1, "4c"), (1, 1, "4d")]], [])

    let e_to_c = mktracks
            [ [(0, 1, "4e")]
            , [(1, 1, "4d")]
            , [(0, 4, "4c")]
            ]
    -- 0   1   2   3   4
    -- e-->
    --     d-->
    -- c-------------->
    io_equal (run False e_to_c) $ Right
        ([[(0, 4, "4c")], [(0, 1, "4e"), (1, 1, "4d")]], [])

    let tracks = mktracks
            [ [(0, 1, "4f")]
            , [(1, 1, "4e")]
            , [(0, 4, "4d")]
            , [(2, 2, "4c")]
            ]
    io_equal (run False tracks) $ Right
        ( [ [(2, 2, "4c")]
          , [(0, 4, "4d")]
          , [(0, 1, "4f"), (1, 1, "4e")]
          ]
        , []
        )
    io_equal (run True tracks) $ Right
        ( [ [(0, 1, "4f"), (1, 1, "4e")]
          , [(0, 4, "4d")]
          , [(2, 2, "4c")]
          ]
        , []
        )
