{-# LANGUAGE NoMonomorphismRestriction #-}
-- | Lang cmds to deal with events.
module Cmd.Lang.LEvent where
import Util.Control
import qualified Util.Seq as Seq
import qualified Ui.Event as Event
import qualified Cmd.Cmd as Cmd
import qualified Cmd.ModifyEvents as ModifyEvents
import qualified Cmd.Selection as Selection

import Types


stretch :: ScoreTime -> Cmd.CmdL ()
stretch n = do
    selected <- Selection.events
    let start = fromMaybe 0 $ Seq.minimum $
            map (\(_, _, evts) -> maybe 0 Event.start (Seq.head evts)) selected
    ModifyEvents.selection $ ModifyEvents.event $
        Event.move (\p -> (p - start) * n + start) . Event.modify_duration (*n)
