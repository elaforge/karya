-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Common interface for the MIDI drivers.
module Midi.Interface (
    Initialize
    , WantMessage
    , ReadChan
    , RawInterface(..), Interface
    , Message(..)
    , trackInterface
    , resetPitch
    , resetControls

    , noteTracker
) where
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM.TChan as TChan
import qualified Control.DeepSeq as DeepSeq
import qualified Control.Monad.State.Strict as State

import qualified Data.ByteString as ByteString
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Tuple as Tuple
import qualified Data.Vector as Vector
import           Data.Vector ((!))
import qualified Data.Vector.Unboxed as Unboxed
import qualified Data.Vector.Unboxed.Mutable as Mutable

import qualified Util.Num as Num
import qualified Midi.Midi as Midi
import           Perform.RealTime (RealTime)

import           Global


-- | Not all synths observe AllNotesOff, but swam does.  Swam also
-- tends to get stuck notes.  TODO who doesn't support it?  And does this fix
-- swam?  If no one and yes, then I can remove the old tracking mechanism.
useAllNotesOff :: Bool
useAllNotesOff = True

type Error = Text

type ReadChan = TChan.TChan Midi.ReadMessage

type Initialize a = String -- ^ register this name with CoreMIDI
    -> WantMessage
    -> (Either Error (RawInterface Midi.WriteMessage) -> IO a)
    -> IO a

-- | Read msgs that return false are filtered.  This uses a raw ByteString to
-- avoid the decode overhead for messages that will be rejected, probably in
-- a high-priority callback.  TODO I could probably make this a bitmask or
-- something and put it down in the driver.
type WantMessage = ByteString.ByteString -> Bool

-- | Produced by an @initialize@ function.
data RawInterface writeMessage = Interface
    -- | Name of the MIDI driver.  Just for debugging.
    { name :: String
    -- | ReadMessages from the opened ReadDevices become available on this
    -- channel.
    , readChannel :: ReadChan
    -- | Get currently connected read and write devices, along with a list
    -- of aliases for each one.
    , readDevices :: IO [(Midi.ReadDevice, [Midi.ReadDevice])]
    , writeDevices :: IO [(Midi.WriteDevice, [Midi.WriteDevice])]

    -- | Start receiving messages on 'read_channel' from the given device.
    -- If it doesn't exist, return False.  But it's ok to connect to
    -- non-existent ReadDevices because if it does get plugged in, it will
    -- be connected automatically.
    , connectReadDevice :: Midi.ReadDevice -> IO Bool
    -- | Stop receiving messages for this device.  False if it wasn't
    -- connected.
    , disconnectReadDevice :: Midi.ReadDevice -> IO Bool

    -- | The same as 'connect_read_device'.
    , connectWriteDevice :: Midi.WriteDevice -> IO Bool

    -- | Write a message to the output.  To avoid overflow, try to not have
    -- more than a thousand or so pending.  False if the WriteDevice isn't
    -- connected.
    --
    -- Messages should be written in increasing time order, with a special
    -- case that timestamp 0 messages will be written immediately.
    , writeMessage :: writeMessage -> IO (Maybe Error)
    -- | Deschedule all pending write messages.
    , abort :: IO ()
    -- | Current time according to the MIDI driver.
    , now :: IO RealTime
    }

instance Show (RawInterface a) where
    show interface = "((MidiInterface " ++ name interface ++ "))"

type Interface = RawInterface Message

-- | Annotate a WriteMessage with additional control messages.
data Message
    = Midi !Midi.WriteMessage
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

trackInterface :: RawInterface Midi.WriteMessage -> IO Interface
trackInterface interface = do
    tracker <- noteTracker useAllNotesOff (writeMessage interface)
    pure $ interface { writeMessage = tracker }

resetPitch :: RealTime -> Message
resetPitch time = AllDevices time $ allChannels (Midi.PitchBend 0)

resetControls :: RealTime -> Message
resetControls time = AllDevices time $
    concatMap allChannels [Midi.ResetAllControls, Midi.PitchBend 0]

allChannels :: Midi.ChannelMessage -> [Midi.Message]
allChannels msg = [Midi.ChannelMessage chan msg | chan <- [0..15]]

-- * implementation

type TrackerM a = State.StateT State IO a
type State = Map Midi.WriteDevice (Vector.Vector (Mutable.IOVector Int))

run :: State -> TrackerM a -> IO (State, a)
run state = fmap Tuple.swap . flip State.runStateT state

-- | Wrap a 'write_message' and keep track of which notes are on.  It can
-- then handle reset messages which need to know current state to reset it.
--
-- This is necessary because some synthesizers do not support AllNotesOff,
-- but also relieves callers of having to track which devices and channels
-- have active notes.
noteTracker :: Bool -> (Midi.WriteMessage -> IO (Maybe err))
    -> IO (Message -> IO (Maybe err))
noteTracker useAllNotesOff write = do
    mstate <- MVar.newMVar Map.empty
    pure $ \msg -> MVar.modifyMVar mstate $ \state -> run state $ do
        newMsgs <- handleMsg msg
        liftIO $ do
            mapM_ write newMsgs
            case msg of
                Midi wmsg -> write wmsg
                _ -> pure Nothing
    where
    handleMsg (Midi wmsg) = do
        case Midi.wmsgMsg wmsg of
            Midi.ChannelMessage chan _
                | not (Num.inRange 0 15 chan) -> pure ()
            Midi.ChannelMessage chan (Midi.NoteOn key vel)
                | vel == 0 -> noteOff dev chan key
                | otherwise -> noteOn dev chan key
            Midi.ChannelMessage chan (Midi.NoteOff key _) ->
                noteOff dev chan key
            _ -> pure ()
        pure []
        where dev = Midi.wmsgDev wmsg
    handleMsg (AllNotesOff time) = allNotesOff useAllNotesOff time
    handleMsg (AllDevices time msgs) = sendDevices time msgs

-- if dev in state:
--      state[dev][chan][key] -= 1
noteOff :: Midi.WriteDevice -> Midi.Channel -> Midi.Key -> TrackerM ()
noteOff dev chan key = when (Num.inRange 0 128 key) $ do
    state <- State.get
    whenJust (Map.lookup dev state) $ \chans -> liftIO $
        Mutable.modify (chans ! fromIntegral chan) (subtract 1)
            (Midi.fromKey key)

-- if dev not in state:
--      state[dev] = [[0]*128] * 16
-- state[dev][chan][key] += 1
noteOn :: Midi.WriteDevice -> Midi.Channel -> Midi.Key -> TrackerM ()
noteOn dev chan key = when (Num.inRange 0 128 key) $ do
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
        Mutable.modify (chans ! fromIntegral chan) (+1) (Midi.fromKey key)

-- | Send the given messages on all devices.
sendDevices :: RealTime -> [Midi.Message] -> TrackerM [Midi.WriteMessage]
sendDevices time msgs = do
    state <- State.get
    pure $ concatMap mkmsgs (Map.keys state)
    where
    mkmsgs dev = map (Midi.WriteMessage dev time) msgs

allNotesOff :: Bool -> RealTime -> TrackerM [Midi.WriteMessage]
allNotesOff useAllNotesOff time = do
    state <- State.get
    -- Debug.traceM "state" =<< traverse (traverse Unboxed.freeze) state
    msgs <- liftIO $ forM (Map.toList state) $ \(dev, chans) ->
        forM (zip [0..] (Vector.toList chans)) $ \(chan, notes) ->
            if useAllNotesOff then pure
                [ Midi.WriteMessage dev time $
                    Midi.ChannelMessage chan Midi.AllNotesOff
                ]
            else do
                keys <- Unboxed.toList <$> Unboxed.freeze notes
                liftIO $ Mutable.set notes 0
                -- Emit a NoteOff for each current NoteOn I've seen.
                pure $ map (noteOff dev chan)
                    [ Midi.Key (fromIntegral i)
                    | (i, count) <- zip [0..] keys
                    , _ <- [0 .. count-1]
                    ]
    -- Debug.tracepM "msgs" $ concat $ concat msgs
    pure $ concat (concat msgs)
    where
    noteOff dev chan key = Midi.WriteMessage dev time $
        Midi.ChannelMessage chan $ Midi.NoteOff key 0
