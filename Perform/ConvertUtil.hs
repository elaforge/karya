-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities for writing Convert modules, which take Score.Events to the
-- performer specific events.
module Perform.ConvertUtil where
import qualified Control.Monad.Error as Error
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.State.Strict as State

import Util.Control
import qualified Util.Log as Log
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack


type ConvertT state a =
    (Error.ErrorT Error (State.StateT state (Log.LogT Identity.Identity)) a)

newtype Error = Error (Maybe Text) deriving (Show)
instance Error.Error Error where strMsg = Error . Just . txt

convert :: state -> (Score.Event -> ConvertT state a)
    -> [Score.Event] -> [LEvent.LEvent a]
convert state convert_event = go state Nothing
    where
    go _ _ [] = []
    go state prev (event : rest) =
        converted ++ map LEvent.Log logs
            ++ go next_state (Just (Score.event_start event)) rest
        where
        (result, logs, next_state) = run_convert state
            (Score.event_stack event) (convert1 prev event)
        converted = case result of
            Nothing -> []
            Just event -> [LEvent.Event event]
    convert1 maybe_prev event = do
        -- Sorted is a postcondition of the deriver.
        whenJust maybe_prev $ \prev -> when (Score.event_start event < prev) $
            Log.warn $ "start time " <> pretty (Score.event_start event)
                <> " less than previous of " <> pretty prev
        convert_event event

run_convert :: state -> Stack.Stack -> ConvertT state a
    -> (Maybe a, [Log.Msg], state)
run_convert state stack conv = case val of
    Left (Error Nothing) -> (Nothing, logs, out_state)
    Left (Error (Just err)) ->
        (Nothing, Log.msg Log.Warn log_stack err : logs, out_state)
    Right val -> (Just val, logs, out_state)
    where
    run = Identity.runIdentity
        . Log.run . flip State.runStateT state . Error.runErrorT
    ((val, out_state), stackless_logs) = run conv
    logs = [msg { Log.msg_stack = log_stack } | msg <- stackless_logs]
    log_stack = Just (Stack.to_strings stack)

require :: Text -> Maybe a -> ConvertT st a
require msg = maybe (throw $ "event requires " <> msg) return

throw :: Text -> ConvertT st a
throw = Error.throwError . Error . Just

abort :: ConvertT st a
abort = Error.throwError (Error Nothing)
