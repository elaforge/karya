-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities for writing Convert modules, which take Score.Events to the
-- performer specific events.
module Perform.ConvertUtil where
import qualified Data.Set as Set

import qualified Util.Log as Log
import qualified Cmd.Cmd as Cmd
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT

import qualified Instrument.Common as Common
import qualified Instrument.Inst as Inst

import           Global


-- | Wrapper that performs common operations for convert functions.
-- Warn if the input isn't sorted, look up the instrument, and run
-- 'Cmd.inst_postproc'.
convert :: (Score.Event -> Cmd.ResolvedInstrument -> [LEvent.LEvent a])
    -> (ScoreT.Instrument -> Maybe Cmd.ResolvedInstrument)
    -> [Score.Event] -> [LEvent.LEvent a]
convert process lookupInst = go Nothing Set.empty
    where
    go _ _ [] = []
    go maybePrev warned (event : events) = increases $ case lookupInst inst of
        Nothing
            -- Only warn the first time an instrument isn't seen, to avoid
            -- spamming the log.
            | inst `Set.member` warned -> go (Just event) warned events
            | otherwise -> warn ("instrument not found: " <> pretty inst)
                : go (Just event) (Set.insert inst warned) events
        Just resolved ->
            map LEvent.Log postprocLogs ++ converted
                ++ go (Just event) warned events
            where
            code = Common.commonCode $ Inst.instCommon $
                Cmd.instInstrument resolved
            converted = map (LEvent.mapLog (addStack event)) $
                process event' resolved
            (event', postprocLogs) = Cmd.instPostproc code event
        where
        inst = Score.eventInstrument event
        -- Sorted is a postcondition of the deriver, verify that.
        increases events
            | Just prev <- maybePrev,
                    Score.eventStart event < Score.eventStart prev =
                warn ("start of " <> Score.shortEvent event
                    <> " less than previous " <> Score.shortEvent prev)
                : events
            | otherwise = events
        warn = LEvent.Log . Log.msg Log.Warn (Just (Score.eventStack event))

addStack :: Score.Event -> Log.Msg -> Log.Msg
addStack event msg = msg { Log.msgStack = Just (Score.eventStack event) }
