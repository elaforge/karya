-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Info_test where
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Info as Info
import qualified Cmd.Simple as Simple

import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig
import qualified Ui.UiTest as UiTest

import           Global
import           Util.Test


test_block_tracks :: Test
test_block_tracks = do
    let f skel tracks = UiTest.eval Ui.empty $ do
            let ts = [(t, []) | t <- tracks]
            UiTest.mkblocksSkel [((UiTest.defaultBlockName, ts), skel)]
            Info.blockTracks UiTest.defaultBlockId
    let track title num = Ui.TrackInfo title (UiTest.mkTid num) num
            (UiTest.btrack (UiTest.mkTid num))
    equal (f [(1, 2)] [">", "*"])
        [ Info.Track (track ">" 1) (Info.Note [track "*" 2] [])
        , Info.Track (track "*" 2) (Info.Pitch (Just (track ">" 1)))
        ]
    equal (f [(1, 2)] [">1", ">2"])
        [ Info.Track (track ">1" 1) (Info.Note [] [track ">2" 2])
        , Info.Track (track ">2" 2) (Info.Note [] [])
        ]

test_track_status :: Test
test_track_status = do
    let f tracks num = CmdTest.eval ustate CmdTest.defaultCmdState
            (Info.getTrackStatus UiTest.defaultBlockId num)
            where
            ustate = (Ui.config#UiConfig.allocations #= allocs) $
                snd $ UiTest.runMkview [(t, []) | t <- tracks]
            allocs = Simple.allocations
                [("i", ("s/1", Simple.Midi $ map ("wdev",) [0..3]))]
    equal (f [">", "*"] 0) Nothing
    equal (f [">", "*"] 1) $ Just "> at 1: [] -- [* {collapse 2}]"
    equal (f [">", "*"] 2) $ Just "> at 1: [] -- [* {collapse 2}]"
    equal (f ["*", ">"] 2) $ Just "> at 2: [] -- [* {collapse 1}]"
    equal (f [">", "*"] 3) Nothing
    equal (f [">i", "vel", "ped"] 2) $
        Just ">i at 1: wdev [1..4] -- [vel {collapse 2}, ped {collapse 3}]"
