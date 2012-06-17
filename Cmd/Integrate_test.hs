module Cmd.Integrate_test where
import Util.Control
import Util.Test
import qualified Ui.Event as Event
import qualified Ui.State as State
import qualified Ui.UiTest as UiTest

import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.ResponderTest as ResponderTest
import Types


test_integrate = do
    let states = mkstates "< | reverse"
            ("s/i1", [(0, 1, "4c"), (1, 1, "4d")], [])
        extract = e_tracks . ResponderTest.result_cmd
    res <- last <$>
        ResponderTest.respond_until_complete states (insert_event 1 (1, 1, ""))
    equal (extract res)
        [ (UiTest.bid "b00",
            [ (">s/i1", [(0, 1, ""), (1, 1, "")])
            , ("*twelve", [(0, 0, "4d"), (1, 0, "4c")])
            ])
        , (UiTest.bid "b1",
            [ (">s/i1", [(0, 1, ""), (1, 1, "")])
            , ("*", [(0, 0, "4c"), (1, 0, "4d")])
            ])
        ]
    res <- last <$>
        ResponderTest.respond_until_complete (ResponderTest.result_states res)
            (insert_event 1 (2, 1, "") >> insert_event 2 (2, 0, "4e"))
    equal (head (extract res))
        (UiTest.bid "b00",
            [ (">s/i1", [(0, 1, ""), (1, 1, ""), (2, 1, "")])
            , ("*twelve", [(0, 0, "4e"), (1, 0, "4d"), (2, 0, "4c")])
            ])


e_tracks :: CmdTest.Result a -> [(BlockId, [UiTest.TrackSpec])]
e_tracks res = UiTest.extract_all_tracks (CmdTest.result_ui_state res)

insert_event :: (State.M m) => TrackNum -> (ScoreTime, ScoreTime, String)
    -> m ()
insert_event tracknum (pos, dur, text) =
    State.insert_event (UiTest.mk_tid tracknum) pos (Event.event text dur)

mkstates :: String -> UiTest.NoteSpec -> ResponderTest.States
mkstates title notes = (UiTest.exec ui_state set_title, cmd_state)
    where
    (ui_state, cmd_state) = ResponderTest.mkstates (UiTest.note_spec notes)
    set_title = State.set_block_title UiTest.default_block_id title

run :: String -> [UiTest.TrackSpec] -> Cmd.CmdId a -> CmdTest.Result a
run title tracks = CmdTest.run ustate CmdTest.default_cmd_state
    where
    ustate = UiTest.exec State.empty $ do
        UiTest.mkblock_view (UiTest.default_block_name, tracks)
        State.set_block_title UiTest.default_block_id title
