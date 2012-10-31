-- | Common interface for the MIDI drivers.
module Midi.Interface where
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM.TChan as TChan
import qualified Control.Monad.State.Strict as State
import qualified Control.Monad.Trans as Trans

import qualified Data.Map as Map
import qualified Data.Tuple as Tuple
import qualified Data.Vector as Vector
import Data.Vector ((!))
import qualified Data.Vector.Unboxed as Unboxed
import qualified Data.Vector.Unboxed.Mutable as Mutable

import Util.Control
import qualified Util.Num as Num
import qualified Midi.Midi as Midi
import Perform.RealTime (RealTime)


type ReadChan = TChan.TChan Midi.ReadMessage

-- | Produced by an @initialize@ function.
data Interface = Interface {
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
    , write_message :: Midi.WriteMessage -> IO Bool
    -- | Deschedule all pending write messages.
    , abort :: IO ()
    -- | Current time according to the MIDI driver.
    , now :: IO RealTime
    }

instance Show Interface where
    show interface = "((MidiInterface " ++ name interface ++ "))"

type State =
    Map.Map Midi.WriteDevice (Vector.Vector (Mutable.IOVector Bool))

-- | Annotate a WriteMessage with additional control messages.
data Message =
    Midi Midi.WriteMessage
    -- | Turn off sounding notes and reset controls on all devices and channels.
    | ResetAll RealTime
    -- | Emit PitchBend 0 for all devices that have been used.  This doesn't
    -- care if they have sounding notes because pitch bend state persists
    -- after NoteOff.
    | ResetAllPitch RealTime
    deriving (Show)

type TrackerM a = State.StateT State IO a

run :: State -> TrackerM Bool -> IO (State, Bool)
run state = fmap Tuple.swap . flip State.runStateT state

-- | Wrap a 'write_message' and keep track of which notes are on.  It can
-- then handle reset messages which need to know current state to reset it.
--
-- This is necessary because some synthesizers do not support ResetAll,
-- but also relieves callers of having to track which devices and channels
-- have active notes.
note_tracker :: (Midi.WriteMessage -> IO Bool) -> IO (Message -> IO Bool)
note_tracker write = do
    mstate <- MVar.newMVar Map.empty
    return $ \msg -> MVar.modifyMVar mstate $ \state -> run state $ do
        new_msgs <- handle_msg msg
        Trans.liftIO $ do
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
    handle_msg (ResetAll time) = reset_all time
    handle_msg (ResetAllPitch time) = send_devices time
        [Midi.ChannelMessage chan (Midi.PitchBend 0) | chan <- [0..15]]

-- if dev in state:
--      state[dev][chan][key] = False
note_off :: Midi.WriteDevice -> Midi.Channel -> Midi.Key -> TrackerM ()
note_off dev chan (Midi.Key key) = when (Num.in_range 0 129 key) $ do
    state <- State.get
    when_just (Map.lookup dev state) $ \chans -> Trans.liftIO $
        Mutable.write (chans ! fromIntegral chan) (fromIntegral key) False

-- if dev not in state:
--      state[dev] = [[0]*128] * 16
-- state[dev][chan][key] = True
note_on :: Midi.WriteDevice -> Midi.Channel -> Midi.Key -> TrackerM ()
note_on dev chan (Midi.Key key) = when (Num.in_range 0 129 key) $ do
    state <- State.get
    case Map.lookup dev state of
        Nothing -> do
            chans <- Trans.liftIO $ Vector.fromList <$>
                replicateM 16 (Mutable.replicate 128 False)
            set chans
            State.modify (Map.insert dev chans)
        Just chans -> set chans
    where
    set chans = Trans.liftIO $
        Mutable.write (chans ! fromIntegral chan) (fromIntegral key) True

-- | Send the given messages on all devices.
send_devices :: RealTime -> [Midi.Message] -> TrackerM [Midi.WriteMessage]
send_devices time msgs = do
    state <- State.get
    return $ concatMap mkmsgs (Map.keys state)
    where
    mkmsgs dev = map (Midi.WriteMessage dev time) msgs

reset_all :: RealTime -> TrackerM [Midi.WriteMessage]
reset_all time = do
    state <- State.get
    msgs <- Trans.liftIO $ forM (Map.toList state) $ \(dev, chans) ->
        forM (zip [0..] (Vector.toList chans)) $ \(chan, notes) -> do
            keys <- Unboxed.toList <$> Unboxed.freeze notes
            let on = [Midi.Key (fromIntegral i) | (i, True) <- zip [0..] keys]
            Trans.liftIO $ Mutable.set notes False
            return $ concatMap (note_off dev chan) on
    return (concat (concat msgs))
    where
    note_off dev chan key =
        map (Midi.WriteMessage dev time . Midi.ChannelMessage chan)
            [Midi.NoteOff key 0, Midi.ResetAllControls]
        -- send PitchBend 0 in case someone doesn't listen to ResetAllControls?
