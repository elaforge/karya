-- | Utilities for writing Convert modules, which take Score.Events to the
-- performer specific events.
module Perform.ConvertUtil where
import qualified Control.Monad.Error as Error
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.State.Strict as State

import Util.Control
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty

import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack


type ConvertT state a =
    (Error.ErrorT Error (State.StateT state (Log.LogT Identity.Identity)) a)

newtype Error = Error (Maybe String) deriving (Show)
instance Error.Error Error where strMsg = Error . Just

convert :: state
    -> (Score.Event -> ConvertT state (a, [Score.Event]))
    -- ^ The function returns the new event, along with optional Score.Events
    -- to push on to the front of the input events.  This way a single
    -- Score.Event can be split into multiple ones during conversion.
    -> [Score.Event] -> [LEvent.LEvent a]
convert state convert_event = go state Nothing
    where
    go _ _ [] = []
    go state prev (event : rest) =
        converted ++ map LEvent.Log logs
            ++ go next_state (Just (Score.event_start event))
                (additional ++ rest)
        where
        (result, logs, next_state) = run_convert state
            (Score.event_stack event) (convert1 prev event)
        (converted, additional) = case result of
            Nothing -> ([], [])
            Just (event, additional) -> ([LEvent.Event event], additional)
    convert1 maybe_prev event = do
        -- Sorted is a postcondition of the deriver.
        when_just maybe_prev $ \prev -> when (Score.event_start event < prev) $
            Log.warn $ "start time " ++ Pretty.pretty (Score.event_start event)
                ++ " less than previous of " ++ Pretty.pretty prev
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

require :: String -> Maybe a -> ConvertT st a
require msg = maybe (throw $ "event requires " ++ msg) return

throw :: String -> ConvertT st a
throw = Error.throwError . Error.strMsg

abort :: ConvertT st a
abort = Error.throwError (Error Nothing)
