-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Common interface for the MIDI drivers.
module Midi.Interface where
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM.TChan as TChan
import qualified Control.DeepSeq as DeepSeq
import qualified Control.Monad.State.Strict as State

import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Tuple as Tuple
import qualified Data.Vector as Vector
import Data.Vector ((!))
import qualified Data.Vector.Unboxed as Unboxed
import qualified Data.Vector.Unboxed.Mutable as Mutable

import qualified Util.Num as Num
import qualified Midi.Midi as Midi
import Perform.RealTime (RealTime)
import Global


type ReadChan = TChan.TChan Midi.ReadMessage

-- | Produced by an @initialize@ function.
data RawInterface write_message = Interface {
    -- | Name of the MIDI driver.  Just for debugging.
    name :: String
    -- | ReadMessages from the opened ReadDevices become available on this
    -- channel.
    , read_channel :: ReadChan
    -- | Get currently connected read and write devices, along with a list
    -- of aliases for each one.
    , read_devices :: IO [(Midi.ReadDevice, [Midi.ReadDevice])]
    , write_devices :: IO [(Midi.WriteDevice, [Midi.WriteDevice])]

    -- | Start receiving messages on 'read_channel' from the given device.
    -- If it doesn't exist, return False.  But it's ok to connect to
    -- non-existent ReadDevices because if it does get plugged in, it will
    -- be connected automatically.
    , connect_read_device :: Midi.ReadDevice -> IO Bool
    -- | Stop receiving messages for this device.  False if it wasn't
    -- connected.
    , disconnect_read_device :: Midi.ReadDevice -> IO Bool

    -- | The same as 'connect_read_device'.
    , connect_write_device :: Midi.WriteDevice -> IO Bool

    -- | Write a message to the output.  To avoid overflow, try to not have
    -- more than a thousand or so pending.  False if the WriteDevice isn't
    -- connected.
    --
    -- Messages should be written in increasing time order, with a special
    -- case that timestamp 0 messages will be written immediately.
    , write_message :: write_message -> IO Bool
    -- | Deschedule all pending write messages.
    , abort :: IO ()
    -- | Current time according to the MIDI driver.
    , now :: IO RealTime
    }

instance Show (RawInterface a) where
    show interface = "((MidiInterface " ++ name interface ++ "))"

type Interface = RawInterface Message

-- | Annotate a WriteMessage with additional control messages.
data Message =
    Midi !Midi.WriteMessage
    -- | Turn off sounding notes on all devices and channels.
    | AllNotesOff !RealTime
    -- | Emit the messages for all devices that have been used.  This doesn't
    -- care if they have sounding notes because control state persists after
    -- NoteOff.
    | AllDevices !RealTime ![Midi.Message]
    deriving (Show, Eq)

instance Pretty Message where
    pretty msg = case msg of
        Midi msg -> pretty msg
        AllNotesOff time -> Text.unwords ["AllNotesOff", pretty time]
        AllDevices time msgs ->
            Text.unwords ["AllDevices", pretty time, pretty msgs]

instance DeepSeq.NFData Message where
    rnf (Midi msg) = DeepSeq.rnf msg
    rnf (AllNotesOff _) = ()
    rnf (AllDevices _ msgs) = DeepSeq.rnf msgs

track_interface :: RawInterface Midi.WriteMessage -> IO Interface
track_interface interface = do
    tracker <- note_tracker (write_message interface)
    return $ interface { write_message = tracker }

reset_pitch :: RealTime -> Message
reset_pitch time = AllDevices time $ all_channels (Midi.PitchBend 0)

reset_controls :: RealTime -> Message
reset_controls time = AllDevices time $
    concatMap all_channels [Midi.ResetAllControls, Midi.PitchBend 0]

all_channels :: Midi.ChannelMessage -> [Midi.Message]
all_channels msg = [Midi.ChannelMessage chan msg | chan <- [0..15]]

-- * implementation

type TrackerM a = State.StateT State IO a
type State = Map Midi.WriteDevice (Vector.Vector (Mutable.IOVector Int))

run :: State -> TrackerM Bool -> IO (State, Bool)
run state = fmap Tuple.swap . flip State.runStateT state

-- | Wrap a 'write_message' and keep track of which notes are on.  It can
-- then handle reset messages which need to know current state to reset it.
--
-- This is necessary because some synthesizers do not support AllNotesOff,
-- but also relieves callers of having to track which devices and channels
-- have active notes.
note_tracker :: (Midi.WriteMessage -> IO Bool) -> IO (Message -> IO Bool)
note_tracker write = do
    mstate <- MVar.newMVar Map.empty
    return $ \msg -> MVar.modifyMVar mstate $ \state -> run state $ do
        new_msgs <- handle_msg msg
        liftIO $ do
            mapM_ write new_msgs
            case msg of
                Midi wmsg -> write wmsg
                _ -> return True
    where
    handle_msg (Midi wmsg) = do
        case Midi.wmsg_msg wmsg of
            Midi.ChannelMessage chan (Midi.NoteOn key vel)
                | vel == 0 -> note_off dev chan key
                | otherwise -> note_on dev chan key
            Midi.ChannelMessage chan (Midi.NoteOff key _) ->
                note_off dev chan key
            _ -> return ()
        return []
        where dev = Midi.wmsg_dev wmsg
    handle_msg (AllNotesOff time) = all_notes_off time
    handle_msg (AllDevices time msgs) = send_devices time msgs

-- if dev in state:
--      state[dev][chan][key] -= 1
note_off :: Midi.WriteDevice -> Midi.Channel -> Midi.Key -> TrackerM ()
note_off dev chan key = when (Num.inRange 0 128 key) $ do
    state <- State.get
    whenJust (Map.lookup dev state) $ \chans -> liftIO $
        Mutable.modify (chans ! fromIntegral chan) (subtract 1)
            (Midi.from_key key)

-- if dev not in state:
--      state[dev] = [[0]*128] * 16
-- state[dev][chan][key] += 1
note_on :: Midi.WriteDevice -> Midi.Channel -> Midi.Key -> TrackerM ()
note_on dev chan key = when (Num.inRange 0 128 key) $ do
    state <- State.get
    case Map.lookup dev state of
        Nothing -> do
            chans <- liftIO $ Vector.fromList <$>
                replicateM 16 (Mutable.replicate 128 0)
            set chans
            State.modify (Map.insert dev chans)
        Just chans -> set chans
    where
    set chans = liftIO $
        Mutable.modify (chans ! fromIntegral chan) (+1) (Midi.from_key key)

-- | Send the given messages on all devices.
send_devices :: RealTime -> [Midi.Message] -> TrackerM [Midi.WriteMessage]
send_devices time msgs = do
    state <- State.get
    return $ concatMap mkmsgs (Map.keys state)
    where
    mkmsgs dev = map (Midi.WriteMessage dev time) msgs

all_notes_off :: RealTime -> TrackerM [Midi.WriteMessage]
all_notes_off time = do
    state <- State.get
    msgs <- liftIO $ forM (Map.toList state) $ \(dev, chans) ->
        forM (zip [0..] (Vector.toList chans)) $ \(chan, notes) -> do
            keys <- Unboxed.toList <$> Unboxed.freeze notes
            liftIO $ Mutable.set notes 0
            return $ map (note_off dev chan)
                [ Midi.Key (fromIntegral i)
                | (i, count) <- zip [0..] keys
                , _ <- [0 .. count-1]
                ]
    return (concat (concat msgs))
    where
    note_off dev chan key = Midi.WriteMessage dev time $
        Midi.ChannelMessage chan $ Midi.NoteOff key 0
