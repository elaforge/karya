module Cmd.Info_test where
import Util.Test
import qualified Ui.State as State
import qualified Ui.UiTest as UiTest
import qualified Cmd.Info as Info


test_block_tracks = do
    let f tracks = UiTest.eval
            (snd (UiTest.run_mkblock [(t, []) | t <- tracks]))
            (Info.block_tracks UiTest.default_block_id)
        track title num = State.TrackInfo title (UiTest.mk_tid num) num
    equal (f [">", "*"])
        [ Info.Track (track ">" 1) (Info.Note (Just (track "*" 2)))
        , Info.Track (track "*" 2) (Info.Pitch (Just (track ">" 1)))
        ]
