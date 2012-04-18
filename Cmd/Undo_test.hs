module Cmd.Undo_test where
import Util.Control
import qualified Util.Rect as Rect
import qualified Util.Seq as Seq
import Util.Test

import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Id as Id
import qualified Ui.State as State
import qualified Ui.Types as Types
import qualified Ui.UiTest as UiTest
import qualified Ui.Update as Update

import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Edit as Edit
import qualified Cmd.ResponderTest as ResponderTest
import qualified Cmd.Selection as Selection
import qualified Cmd.Undo as Undo

import qualified App.Config as Config
import Types


test_undo = do
    let track_updates from to events =
            [Update.TrackUpdate (UiTest.mk_tid 1) (Update.TrackEvents from to
                (Events.from_list events))]
        states = ResponderTest.mkstates [(">", [(0, 1, "1"), (1, 1, "2")])]
        next res = ResponderTest.respond_cmd (ResponderTest.result_states res)

    res <- ResponderTest.respond_cmd states $ Cmd.name "+z" $ insert_event 0 "z"
    res <- next res $ Cmd.name "+q" $ insert_event 1 "q"

    equal (extract_ui res) "zq"
    equal (e_updates res) (track_updates 1 2 [(1, Event.event "q" 1)])

    res <- next res Undo.undo
    -- Make sure the past and future have the expected names and states.
    equal (extract_hist res) (["+z: z2", "setup: 12"], ["+q: zq"])
    equal (extract_ui res) "z2"
    equal (e_updates res) (track_updates 1 2 [(1, Event.event "2" 1)])

    res <- next res Undo.undo
    equal (extract_hist res) (["setup: 12"], ["+z: z2", "+q: zq"])
    equal (extract_ui res) "12"
    equal (e_updates res) (track_updates 0 1 [(0, Event.event "1" 1)])

    -- no past to undo
    res <- next res Undo.undo
    equal (extract_ui res) "12"
    equal (e_updates res) []

    res <- next res Undo.redo
    equal (extract_hist res) (["+z: z2", "setup: 12"], ["+q: zq"])
    equal (extract_ui res) "z2"
    equal (e_updates res) (track_updates 0 1 [(0, Event.event "z" 1)])

    res <- next res Undo.redo
    equal (extract_ui res) "zq"
    equal (extract_hist res) (["+q: zq", "+z: z2", "setup: 12"], [])
    equal (e_updates res) (track_updates 1 2 [(1, Event.event "q" 1)])

    -- no future to redo
    res <- next res Undo.redo
    equal (extract_ui res) "zq"
    equal (e_updates res) []

test_suppress_history = do
    let states = ResponderTest.mkstates [(">", [(0, 1, "1"), (1, 1, "2")])]
        next res = ResponderTest.respond_cmd (ResponderTest.result_states res)
    let suppress = Cmd.suppress_history Cmd.RawEdit

    res <- ResponderTest.respond_cmd states $
        Cmd.name "toggle" Edit.cmd_toggle_raw_edit
    res <- next res $ suppress "+z" $ insert_event 0 "z"
    equal (extract_hist res) (["setup: 12"], [])
    res <- next res $ suppress "+q" $ insert_event 1 "q"
    equal (extract_hist res) (["setup: 12"], [])
    -- A non-recording cmd will cause the suppressed cmds to be recorded.
    res <- next res $ set_sel 1
    res <- next res $ Cmd.name "toggle" Edit.cmd_toggle_raw_edit
    equal (extract_hist res) (["+z: zq", "setup: 12"], [])


extract_hist :: ResponderTest.Result -> ([String], [String])
extract_hist res = (map extract past, map extract future)
    where
    Cmd.History past future _ = e_hist res
    Cmd.HistoryCollect _updates _cmd_names _suppress _suppressed =
        e_hist_collect res
    extract (Cmd.HistoryEntry state _ commands) =
        Seq.join "+" commands ++ ": " ++ ui_notes state

extract_ui :: ResponderTest.Result -> String
extract_ui = ui_notes . e_ui

ui_notes :: State.State -> String
ui_notes ui_state = [c | (_, _, c:_) <- tracks]
    where [(">", tracks)] = UiTest.extract_tracks ui_state

insert_event :: (State.M m) => ScoreTime -> String -> m ()
insert_event pos text =
    State.insert_event (UiTest.mk_tid 1) pos (Event.event text 1)

set_sel :: (Cmd.M m) => ScoreTime -> m ()
set_sel pos = Cmd.name "select" $ Selection.set_current Config.insert_selnum
    (Just (Types.point_selection 1 pos))


test_undo_merge = do
    let states = ResponderTest.mkstates [(">", [])]
        vid = UiTest.default_view_id
    res1 <- ResponderTest.respond_cmd states $ do
        State.set_namespace (Id.unsafe_namespace "oogabooga")
        State.set_view_rect vid $ Rect.xywh 40 40 100 100
        State.insert_event (UiTest.mk_tid 1) 0 (Event.event "z" 1)
    res2 <- ResponderTest.respond_cmd (ResponderTest.result_states res1)
        Undo.undo

    -- pprint (e_hist_updates res1)

    -- some things aren't affected by undo
    -- namespace doesn't change
    let ns = State.config_namespace . State.state_config . e_ui
    equal (ns res1) (Id.unsafe_namespace "oogabooga")
    equal (UiTest.eval (e_ui res1) (Block.view_rect <$> State.get_view vid))
        (Rect.xywh 40 40 100 100)

    equal (ns res2) (Id.unsafe_namespace "oogabooga")
    equal (UiTest.eval (e_ui res2) (Block.view_rect <$> State.get_view vid))
        (Rect.xywh 40 40 100 100)

e_ui :: ResponderTest.Result -> State.State
e_ui = CmdTest.result_ui_state . ResponderTest.result_cmd

e_hist_updates :: ResponderTest.Result
    -> ([[Update.CmdUpdate]], [[Update.CmdUpdate]])
e_hist_updates = hist_updates . e_hist

e_hist :: ResponderTest.Result -> Cmd.History
e_hist = Cmd.state_history . CmdTest.result_cmd_state . ResponderTest.result_cmd

e_hist_collect = Cmd.state_history_collect . CmdTest.result_cmd_state
    . ResponderTest.result_cmd

hist_updates :: Cmd.History -> ([[Update.CmdUpdate]], [[Update.CmdUpdate]])
hist_updates (Cmd.History past future _undo_redo) =
    (map Cmd.hist_updates past, map Cmd.hist_updates future)
e_updates = ResponderTest.result_updates
