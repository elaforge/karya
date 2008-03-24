module Msg.Match where

import qualified Msg.Msg as Msg
{-

key spec is
([context], Msg), goes directly into a Map


default_map =
    [ Map (key Key.Escape) c_quit
    , Map (has_block (key Key.Up))
    ]

-- | Context must have a view.
has_block = 

insert_selection = 0

{-
Nothing means the handler didn't match
Just (True, ...) means it matched but the next one should be tried too

Should be a MonadPlus
zero = Nothing
send_midi dev msg -- append to msgs
log msg -- append to log msgs
action act -- append to actions
modify state
modify namespace
lift ops into IO monad for queries (UI monad to restrict to UI ops?)
-}
Handler :: Namespace -> State -> Msg.Msg
    -> IO (Maybe (Bool, State, Namespace, [(Midi.Device, Midi.Message)],
        [Action], [Log.Msg]))

data HandlerState state = HandlerState
    { handler_state :: state
    , handler_namespace :: Namespace
    , handler_midi_out :: [(Midi.Device, Midi.Message)]
    , handler_actions :: [Action]
    , handler_log :: [Log.msg]
    }
data DefaultState = DefaultState
    { state_edit_mode :: Bool
    , state_event_length :: TimeInterval
    , state_advance :: TimeInterval
    }

type Handler = Msg.Msg -> HandlerM HandlerResult

type HandlerM a = State.StateT HandlerState IO a

data HandlerResult = Stop | Continue

-- | Get the ruler that applies to the given track.  Search left for the
-- closest ruler.
relevant_ruler :: Block.Block -> Block.TrackNum -> IO Ruler

select_point tracknum pos = Selection (tracknum, pos) (1, pos)

-- Get block from context, if there isn't one, abort the handler.
ui_block (Msg.Msg

-- | Go up and down by current step.
c_step_advance :: Msg.Msg -> Handler HandlerResult
c_step_advance msg = do
    view <- ui_block msg
    step <- state_step
    sel <- lift $ Block.get_selection view insert_selection
    ruler <- relevant_ruler (Block.view_block view) (Block.sel_start_track sel)
    case advance_time_step step ruler pos of
        Nothing -> log Log.Notice $ "can't advance to " ++ pretty step
            ++ " from pos"
        Just next_pos -> action $ Action.set_selection view insert_selection
            (select_point (Block.sel_start_track sel) next_pos)
    return Stop
-}

-- Msg.Ui (UiMsg.UiMsg {UiMsg.msg_data
--     = UiMsg.Kbd {UiMsg.kbd_key = Key.Escape}}) -> True
