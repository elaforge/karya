module Cmd.Info_test where
import Util.Test
import qualified Ui.State as State
import qualified Ui.UiTest as UiTest
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Info as Info


test_block_tracks = do
    let f tracks = UiTest.eval
            (snd (UiTest.run_mkblock [(t, []) | t <- tracks]))
            (Info.block_tracks UiTest.default_block_id)
        track title num = State.TrackInfo title (UiTest.mk_tid num) num
    equal (f [">", "*"])
        [ Info.Track (track ">" 1) (Info.Note [track "*" 2])
        , Info.Track (track "*" 2) (Info.Pitch (Just (track ">" 1)))
        ]

test_track_status = do
    let f tracks num = CmdTest.eval ustate CmdTest.default_cmd_state
            (Info.get_track_status UiTest.default_block_id num)
            where
            ustate = UiTest.set_midi_config
                (UiTest.midi_config [("i", [0..3])]) $
                snd $ UiTest.run_mkview [(t, []) | t <- tracks]
    equal (f [">", "*"] 0) Nothing
    equal (f [">", "*"] 1) $
        Just "> at 1: [] -- [* {collapse 2}]"
    equal (f [">", "*"] 2) $
        Just "> at 1: [] -- [* {collapse 2}]"
    equal (f ["*", ">"] 2) $
        Just "> at 2: [] -- [* {collapse 1}]"
    equal (f [">", "*"] 3) Nothing
    equal (f [">i", "vel", "ped"] 2) $
        Just ">i at 1: s [0..3] -- [vel {collapse 2}, ped {collapse 3}]"
