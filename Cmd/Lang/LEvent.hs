-- | Lang cmds to deal with events.
module Cmd.Lang.LEvent where
import qualified Util.Seq as Seq

import Ui
import qualified Ui.Event as Event

import qualified Cmd.Cmd as Cmd
import qualified Cmd.ModifyEvents as ModifyEvents
import qualified Cmd.Selection as Selection


stretch :: ScoreTime -> Cmd.CmdL ()
stretch n = do
    selected <- Selection.events
    let start = maybe 0 id $ Seq.minimum $
            map (\(_, _, evts) -> Seq.mhead 0 fst evts) selected
    ModifyEvents.events_sorted $ \(pos, event) ->
        Just ((pos - start) * n + start, Event.modify_duration (*n) event)
