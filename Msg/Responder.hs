{- | Manage the function that maps incoming 'Ui.Ui.Msg's to Actions.

'j' and 'k' go up and down by events if there is one, or the current step if
there isn't one.

Current step is set by number keys: ` is until next rank 0, 1 until next rank
1, etc.  Shift and the number key advances to next mark + offset from current
one, so it advances one mark's worth without aligning on marks.

Pressing 'insert mode' turns the track box red and all subsequent printable
keystrokes create a new event at the current position, or append to the
currently selected event.  'escape' (maybe space?) leaves insert mode.  Insert
/ escape could also be pedal down / up.

    Midi:
Midi notes insert that note if I'm in insert mode.  Focus on a block sets the
thru according to which track the primary select is on in that block.


-- namespace is so that responders can get the block list, etc.
Namespace = map names to tracks and blocks

-- State should be serializable so it can be saved and restored.
BlockState (stored in Block)
    insert_mode :: Bool
    default_advance :: TimeInterval

GlobalState =
    { block_defaults :: BlockConfig
    }

-- Having this all in IO makes it hard to test.
-- The responder needs to query and modify the block state though.
-- Hopefully I can pull a lot out into pure functions (e.g. TimeInterval)
-- and have the actual responder IO code just be compositions.
Responder :: Namespace -> State -> IO (State, [Action], [Log.Msg])

r InsertMode = (set_track_box red, state { insert_mode = True }, [])
r Escape = (set_track_box blue, state { insert_mode = False}, [])
r (Key (Control 'n')) =
    (Block.create (block_defaults state) >>= add_to_namespace,
        state, [BlockCreate state])
r (Key ArrowDown) = 
    (Block.get_selection >>= find_next_major >>= Block.set_selection,
        state, [SetSelection from_pos to_pos])
r (MidiNote nn) = 
    (Block.get_selection >>= Track.insert_event (midi_to_event nn)
        >> advance_selection (default_advance state)

r StartPlay = find_block_to_play >>= start_player_thread
r StopPlay = stop_player_thread

-- this happens after every action that modifies track data
track_modified block = for all_blocks update_derivation

Control keys add new block + view, or insert or delete 

Later, a separate responder listens on a socket, receives code, and executes
it.  "Code" is [Action].

I could eliminate the need for hs-plugins and write the interface in any
language if I allowed query-response (effectively arbitrary rpc) in addition to
actions.  Nah, hs-plugins would be faster and easier (esp. with lazy evaluated
result lists for [Event]).


State:
edit mode, insert mode
midi thru filter


current step mode

Each Responder takes a msg and the current state, and produces its results plus
a new state.  Other responders should be able to e.g. change the record state
and also have their personal state.

Responders are composed by modifying a Map MsgPattern Responder

Whoever records Actions should do some filtering.  A resize can result in
a million resize Actions, but I only need to remember the last one.  After
that, the action is written to the save log and put into the undo queue.

-}
module Msg.Responder where

import qualified Control.Monad as Monad
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TChan as TChan
import qualified System.IO.Unsafe as Unsafe

import qualified Util.Log as Log
import qualified Ui.Key as Key
import qualified Ui.UiMsg as UiMsg
import qualified Midi.Midi as Midi


data Msg = Ui UiMsg.UiMsg | Midi Midi.CompleteMessage -- | MOsc OscMsg.OscMsg
    deriving (Show)

-- | Do some IO (such as querying the UI), and return stuff to do.
type Responder = Namespace -> State -> Msg
    -> IO (Bool, State, Namespace, [(Midi.Device, Midi.Message)], [Action],
        [Log.Msg])

type Action = () -- TODO
type Namespace = () -- TODO, the block namespace
type State = () -- TODO

responder_mvar :: MVar.MVar Responder
responder_mvar = Unsafe.unsafePerformIO (MVar.newMVar default_responder)
default_responder :: Responder
default_responder ns state msg = do
    putStrLn $ "msg: " ++ case msg of
        Ui msg -> UiMsg.pretty_ui_msg msg
        _ -> show msg
    let
        quit = case msg of 
            Ui (UiMsg.UiMsg {UiMsg.msg_data
                = UiMsg.Kbd {UiMsg.kbd_key = Key.Escape}}) -> True
            _ -> False
    return (quit, ns, state, [], [], [])

-- | Replace the responder with the given one.
set_responder :: Responder -> IO ()
set_responder responder
    = MVar.modifyMVar_ responder_mvar (return . const responder)

-- | Runs in a loop, reading and handling msgs.
responder_thread :: Namespace -> State -> IO Msg
    -> (Midi.CompleteMessage -> IO ()) -> IO ()
responder_thread namespace state get_msg write_midi = do
    responder <- MVar.readMVar responder_mvar
    msg <- get_msg
    (quit, namespace', state', midi_msgs, actions, log_msgs)
        <- responder namespace state msg
    sequence_ [write_midi (dev, 0, msg) | (dev, msg) <- midi_msgs]
    mapM_ Log.write log_msgs
    -- mapM_ Action.record actions
    Monad.when (not quit) $
        responder_thread namespace' state' get_msg write_midi

-- | Pass to 'responder_thread'
read_msg_chans :: TChan.TChan UiMsg.UiMsg -> TChan.TChan Midi.CompleteMessage
    -> IO Msg
read_msg_chans ui_chan midi_chan = STM.atomically $
    fmap Ui (TChan.readTChan ui_chan)
    `STM.orElse` fmap Midi (TChan.readTChan midi_chan)

{-
data DefaultState = DefaultState
    { default_note_length :: TimeInterval
    , default_advance :: TimeInterval
    } deriving (Show)

advance_time_interval :: TimeInterval -> TrackPos -> Maybe TrackPos
advance_time_interval marklist interval start_pos = case interval of
    Absolute pos = Just (start_pos + pos)
    UntilMark matcher = matcher marklist start_pos
    MarkDistance matcher = matcher marklist start_pos + start_pos

-- | A variable time interval, used to find out how much to advance to advance
-- the cursor, how long an event should be, etc.
data TimeInterval
    = Absolute TrackPos -- ^ absolute time interval
    | UntilMark MarkMatch -- ^ until the next mark that matches
    | MarkDistance MarkMatch -- ^ until next mark + offset from previous mark

type MarkMatch = Ruler.Marklist -- ^ given this marklist
    -> TrackPos -- ^ starting at this pos
    -> Maybe TrackPos -- ^ advance until this pos, if found

-- | Get the pos of the next mark of the given rank.
match_rank :: Int -> MarkMatch
match_rank rank marklist start_pos
    | null matches = Nothing
    | otherwise = fst (head matches)
    where
    matches = filter ((==rank) . Ruler.mark_rank . snd)
        (Ruler.marklist_forward_from start_pos)

-}
