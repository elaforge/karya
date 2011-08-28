module Cmd.Undo_test where
import Util.Control
import qualified Util.Rect as Rect
import Util.Test

import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.State as State
import qualified Ui.UiTest as UiTest
import qualified Ui.Update as Update

import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.ResponderTest as ResponderTest
import qualified Cmd.Undo as Undo


test_undo = do
    let extract_ui res = [c | (_, _, c:_) <- tracks]
            where [(">", tracks)] = UiTest.extract_tracks (e_ui res)
        tid = UiTest.mk_tid 0
        track_updates from to events =
            [Update.TrackUpdate tid (Update.TrackEvents from to
                (Events.make events))]
        states = ResponderTest.mkstates [(">", [(0, 1, "1"), (1, 1, "2")])]
        next res = ResponderTest.respond_cmd (ResponderTest.result_states res)

    res <- ResponderTest.respond_cmd states $
        State.insert_event tid 0 (Event.event "z" 1)
    res <- next res $ State.insert_event tid 1 (Event.event "q" 1)
    equal (e_updates res) (track_updates 1 2 [(1, Event.event "q" 1)])
    equal (extract_ui res) "zq"

    res <- next res Undo.undo
    equal (extract_ui res) "z2"
    equal (e_updates res) (track_updates 1 2 [(1, Event.event "2" 1)])

    res <- next res Undo.undo
    equal (extract_ui res) "12"
    equal (e_updates res) (track_updates 0 1 [(0, Event.event "1" 1)])

    -- no past to undo
    res <- next res Undo.undo
    equal (extract_ui res) "12"
    equal (e_updates res) []

    res <- next res Undo.redo
    equal (extract_ui res) "z2"
    equal (e_updates res) (track_updates 0 1 [(0, Event.event "z" 1)])

    res <- next res Undo.redo
    equal (extract_ui res) "zq"
    equal (e_updates res) (track_updates 1 2 [(1, Event.event "q" 1)])

    -- no future to redo
    res <- next res Undo.redo
    equal (extract_ui res) "zq"
    equal (e_updates res) []

test_undo_merge = do
    let states = ResponderTest.mkstates [(">", [])]
        vid = UiTest.default_view_id
    res1 <- ResponderTest.respond_cmd states $ do
        State.set_namespace "oogabooga"
        State.set_view_rect vid $ Rect.xywh 40 40 100 100
        State.insert_event (UiTest.mk_tid 0) 0 (Event.event "z" 1)
    res2 <- ResponderTest.respond_cmd (ResponderTest.result_states res1)
        Undo.undo

    pprint (e_hist_updates res1)

    -- some things aren't affected by undo
    -- namespace doesn't change
    let ns = State.config_namespace . State.state_config . e_ui
    equal (ns res1) "oogabooga"
    equal (UiTest.eval (e_ui res1) (Block.view_rect <$> State.get_view vid))
        (Rect.xywh 40 40 100 100)

    equal (ns res2) "oogabooga"
    equal (UiTest.eval (e_ui res2) (Block.view_rect <$> State.get_view vid))
        (Rect.xywh 40 40 100 100)

e_ui = CmdTest.result_ui_state . ResponderTest.result_cmd
e_hist_updates = e_hist . Cmd.state_history . CmdTest.result_cmd_state
    . ResponderTest.result_cmd
e_hist (Cmd.History past future serial) =
    (map Cmd.hist_entry_updates past,
        map Cmd.hist_entry_updates future, serial)
e_updates = ResponderTest.result_updates
