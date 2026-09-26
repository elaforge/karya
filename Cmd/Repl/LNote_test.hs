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
    let run highLeft tracks =
            CmdTest.ePitchTracks <$>
                CmdTest.runTracksWithPerformance tracks cmd
            where cmd = CmdTest.selectAll >> LNote.sortOnPitch highLeft
    let mktracks = concatMap UiTest.noteTrack
    let cToE = mktracks
            [ [(0, 1, "4c")]
            , [(1, 1, "4d")]
            , [(0, 4, "4e")]
            ]
    -- 0   1   2   3   4
    -- c-->
    --     d-->
    -- e-------------->
    ioEqual (run False cToE) $ Right
        ([[(0, 1, "4c"), (1, 1, "4d")], [(0, 4, "4e")]], [])
    ioEqual (run True cToE) $ Right
        ([[(0, 4, "4e")], [(0, 1, "4c"), (1, 1, "4d")]], [])

    let eToC = mktracks
            [ [(0, 1, "4e")]
            , [(1, 1, "4d")]
            , [(0, 4, "4c")]
            ]
    -- 0   1   2   3   4
    -- e-->
    --     d-->
    -- c-------------->
    ioEqual (run False eToC) $ Right
        ([[(0, 4, "4c")], [(0, 1, "4e"), (1, 1, "4d")]], [])

    let tracks = mktracks
            [ [(0, 1, "4f")]
            , [(1, 1, "4e")]
            , [(0, 4, "4d")]
            , [(2, 2, "4c")]
            ]
    ioEqual (run False tracks) $ Right
        ( [ [(2, 2, "4c")]
          , [(0, 4, "4d")]
          , [(0, 1, "4f"), (1, 1, "4e")]
          ]
        , []
        )
    ioEqual (run True tracks) $ Right
        ( [ [(0, 1, "4f"), (1, 1, "4e")]
          , [(0, 4, "4d")]
          , [(2, 2, "4c")]
          ]
        , []
        )
