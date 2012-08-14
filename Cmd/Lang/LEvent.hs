-- | Lang cmds to deal with events.
module Cmd.Lang.LEvent where
import qualified Util.Seq as Seq
import qualified Ui.Event as Event
import qualified Cmd.Cmd as Cmd
import qualified Cmd.ModifyEvents as ModifyEvents
import qualified Cmd.Selection as Selection

import Types


stretch :: ScoreTime -> Cmd.CmdL ()
stretch n = do
    selected <- Selection.events
    let start = maybe 0 id $ Seq.minimum $
            map (\(_, _, evts) -> maybe 0 Event.start (Seq.head evts)) selected
    ModifyEvents.events $ ModifyEvents.event $
        Event.move (\p -> (p - start) * n + start) . Event.modify_duration (*n)
