-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Interface to CoreMIDI.

    Receiving messages in CoreMIDI is simple, there's a callback that gets
    called when one arrives.  However, it identifies them with a 'DeviceId',
    not a name.  You get the ID when you open the device for reading.  If the
    device is not present, then I record that I want it in 'Client', and
    'notify_callback' will hopefully add the DeviceId if it does appear.

    Sending messages is the same situation.  It's more complicated at the C
    level, which is documented in core_midi.cc.

    Other than that, CoreMIDI is pretty straightforward.  Since I only support
    a single MIDI initialization, the various open client and port types are
    kept as global variables inside the C binding.

    I originally tried to use PortMidi, but found it both lacking essential
    features (it couldn't schedule MIDI both for the future and for now), but
    also harder to write for.
-}
module Midi.CoreMidi (initialize) where
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as Exception

import qualified Data.ByteString as ByteString
import           Data.ByteString (ByteString)
import qualified Data.IORef as IORef
import qualified Data.Int as Int
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Foreign
import           Foreign (FunPtr, Ptr, Word8)
import qualified Foreign.C as C
import           Foreign.C (CInt(..), CString, CULong(..))

import           GHC.Stack (HasCallStack)

import qualified Util.FFI as FFI
import qualified Util.Log as Log
import qualified Midi.Encode as Encode
import qualified Midi.Interface as Interface
import qualified Midi.Midi as Midi

import qualified Perform.RealTime as RealTime
import           Perform.RealTime (RealTime)

import           Global


type Error = Text

-- * initialize

initialize :: Interface.Initialize a
initialize appName wantMessage app = do
    chan <- STM.newTChanIO
    client <- makeClient
    withReadCb chan $ \readCb -> withNotifyCb client $ \notifyCb ->
        C.withCString appName $ \appNamep -> do
            errMvar <- MVar.newEmptyMVar
            -- This, along with 'c_prime_runloop' below, is a bit of song and
            -- dance to get MIDI notifications to work in CoreMIDI.  It wants
            -- to send them on the current runloop when the client is created.
            -- But I don't use the OS X runloop, so I have to have
            -- c_initialize enter one right after it creates the client.
            --
            -- However, runloops on other threads apparently don't work unless
            -- the main runloop has been called at least once, which is what
            -- the 'c_prime_runloop' nonsense below is about.
            Concurrent.forkOS $ do
                err <- cInitialize appNamep readCb notifyCb
                mbErr <- errorStr err
                MVar.putMVar errMvar mbErr
                when (mbErr == Nothing) cCfRunloopRun
            MVar.takeMVar errMvar >>= \case
                Just err -> app (Left err)
                Nothing -> do
                    cPrimeRunloop
                    app (Right (mkinterface client chan))
                        `Exception.finally` terminate
    where
    withReadCb chan = Exception.bracket
        (makeReadCallback (readCallback wantMessage chan))
        Foreign.freeHaskellFunPtr
    withNotifyCb client = Exception.bracket
        (makeNotifyCallback (notifyCallback client))
        Foreign.freeHaskellFunPtr
    mkinterface client chan = Interface.Interface
        { name = "CoreMIDI"
        , readChannel = chan
        , readDevices = map ((, []) . Midi.readDevice) <$> getDevices True
        , writeDevices = map ((, []) . Midi.writeDevice) <$> getDevices False
        , connectReadDevice = connectReadDevice client
        , disconnectReadDevice = disconnectReadDevice client
        , connectWriteDevice = connectWriteDevice client
        , writeMessage = writeMessage client
        , abort = abort
        , now = now
        }

type ReadCallback = SourcePtr -> CTimestamp -> CInt -> Ptr Word8 -> IO ()
type SourcePtr = Foreign.StablePtr Midi.ReadDevice
-- typedef void (*ReadCallback)(void *p, Timestamp timestamp, int len,
--     const unsigned char *bytes);

type NotifyCallback = CString -> DeviceId -> CInt -> CInt -> IO ()
-- typedef void (*NotifyCallback)(const char *name, DeviceId dev_id,
--     int is_added, int is_read);

foreign import ccall "core_midi_terminate" terminate :: IO ()
foreign import ccall "core_midi_initialize"
    cInitialize :: CString -> FunPtr ReadCallback -> FunPtr NotifyCallback
        -> IO OSStatus
foreign import ccall "wrapper"
    makeReadCallback :: ReadCallback -> IO (FunPtr ReadCallback)
foreign import ccall "wrapper"
    makeNotifyCallback :: NotifyCallback -> IO (FunPtr NotifyCallback)
foreign import ccall "CFRunLoopRun" cCfRunloopRun :: IO ()
foreign import ccall "core_midi_prime_runloop" cPrimeRunloop :: IO ()

-- TODO this is run from the CoreMIDI callback.  The callback is supposed to
-- be low latency which means no allocation, but haskell has plenty of
-- allocation.  On the other hand, it hasn't been a problem in practice and
-- a separate thread monitoring a ringbuffer would just add more latency.
readCallback :: (ByteString -> Bool) -> Interface.ReadChan -> ReadCallback
readCallback wantMessage chan sourcep ctimestamp len bytesp = do
    -- Oddly enough, even though ByteString is Word8, the ptr packing function
    -- wants CChar.
    bytes <- ByteString.packCStringLen
        (Foreign.castPtr bytesp, fromIntegral len)
    rdev <- Foreign.deRefStablePtr sourcep
    when (wantMessage bytes) $ STM.atomically $ STM.writeTChan chan $
        Midi.ReadMessage rdev (decodeTime ctimestamp) (Encode.decode bytes)

notifyCallback :: Client -> NotifyCallback
notifyCallback client namep _devId cIsAdded cIsRead = do
    -- I could make connect_read_device and connect_write_device that take
    -- the dev_id directly, but that's too much work.
    name <- FFI.peekCString namep
    case (Foreign.toBool cIsAdded, Foreign.toBool cIsRead) of
        (True, True) -> do
            let dev = Midi.readDevice name
            reads <- IORef.readIORef (clientReads client)
            when (dev `Set.member` reads) $
                void $ connectReadDevice client dev
        (True, False) -> do
            let dev = Midi.writeDevice name
            writes <- IORef.readIORef (clientWrites client)
            when (dev `Map.member` writes) $
                void $ connectWriteDevice client dev
        -- I leave removes alone.  If a source disappears, I'll just stop
        -- getting msgs from it.  If a destination disappears, then writes
        -- to it will fail, exactly as if it weren't connected.  CoreMIDI
        -- doesn't let me look up the name and ID of disconnected devices
        -- anyway.
        _ -> pure ()

data Client = Client
    -- | I don't need to deal with DeviceIds for reads.
    { clientReads :: IORef.IORef (Set.Set Midi.ReadDevice)
    , clientWrites :: IORef.IORef (Map.Map Midi.WriteDevice (Maybe DeviceId))
    }

makeClient :: IO Client
makeClient = Client <$> IORef.newIORef Set.empty <*> IORef.newIORef Map.empty

type DeviceId = CInt

-- * devices

connectReadDevice :: Client -> Midi.ReadDevice -> IO Bool
connectReadDevice client dev =
    lookupDeviceId True (Midi.readDeviceText dev) >>= \case
        Nothing -> do
            -- This means I want the device if it ever gets plugged in.
            IORef.modifyIORef (clientReads client) (Set.insert dev)
            pure False
        Just devId -> do
            -- CoreMIDI lets you attach an arbitrary pointer to each
            -- connection to identify msgs coming in on that connection.  So
            -- I can use a stable ptr to the ReadDevice and have the read
            -- callback directly get a ReadDevice without having to look
            -- anything up.
            sourcep <- Foreign.newStablePtr dev
            ok <- check =<< cConnectReadDevice devId
                (Foreign.castStablePtrToPtr sourcep)
            if not ok then pure False else do
                IORef.modifyIORef (clientReads client) (Set.insert dev)
                pure True

foreign import ccall "core_midi_connect_read_device"
    cConnectReadDevice :: CInt -> Ptr () -> IO OSStatus

disconnectReadDevice :: Client -> Midi.ReadDevice -> IO Bool
disconnectReadDevice client dev = do
    maybeDevId <- lookupDeviceId True (Midi.readDeviceText dev)
    wanted <- Set.member dev <$> IORef.readIORef (clientReads client)
    IORef.modifyIORef (clientReads client) (Set.delete dev)
    case (maybeDevId, wanted) of
        (Nothing, True) -> pure True
        (Just devId, _) -> check =<< cDisconnectReadDevice devId
        _ -> pure False

foreign import ccall "core_midi_disconnect_read_device"
    cDisconnectReadDevice :: DeviceId -> IO OSStatus

connectWriteDevice :: Client -> Midi.WriteDevice -> IO Bool
connectWriteDevice client dev = do
    -- CoreMIDI doesn't have a notion of connected write devices, they are
    -- all implicitly connected and you need only emit a msg with the
    -- appropriate device id.
    maybeDevId <- lookupDeviceId False (Midi.writeDeviceText dev)
    case maybeDevId of
        Nothing -> do
            IORef.modifyIORef (clientWrites client) (Map.insert dev Nothing)
            pure False
        Just devId -> do
            IORef.modifyIORef (clientWrites client)
                (Map.insert dev (Just devId))
            pure True

lookupDeviceId :: Bool -> Text -> IO (Maybe DeviceId)
lookupDeviceId isRead dev = Foreign.alloca $ \devIdp -> do
    found <- FFI.withText dev $ \devp ->
        cLookupDeviceId (Foreign.fromBool isRead) devp devIdp
    if found == 0 then pure Nothing
        else Just <$> Foreign.peek devIdp

foreign import ccall "core_midi_lookup_device_id"
    cLookupDeviceId :: CInt -> CString -> Ptr DeviceId -> IO CInt

getDevices :: Bool -> IO [Text]
getDevices isRead = Foreign.alloca $ \namesp -> do
    len <- cGetDevices (Foreign.fromBool isRead) namesp
    nameArray <- Foreign.peek namesp
    cnames <- Foreign.peekArray (fromIntegral len) nameArray
    names <- mapM FFI.peekCString cnames
    mapM_ Foreign.free cnames
    Foreign.free nameArray
    pure names

foreign import ccall "core_midi_get_devices"
    cGetDevices :: CInt -> Ptr (Ptr CString) -> IO CInt

-- * write

-- | RealTime will be ignored for sysex msgs.
writeMessage :: Client -> Midi.WriteMessage -> IO (Maybe Error)
writeMessage client (Midi.WriteMessage dev ts msg)
    | not (Midi.validMsg msg) = pure $ Just $ "invalid msg: " <> showt msg
    | otherwise = do
        -- I could probably avoid this copy by using unsafe unpack and then a
        -- ForeignPtr or something to keep the gc off it, but MIDI is so slow
        -- that any sizable sysex will take forever to send anyway.
        writes <- IORef.readIORef (clientWrites client)
        case Map.lookup dev writes of
            Just (Just devId) ->
                ByteString.useAsCStringLen (Encode.encode msg) $
                \(bytesp, len) -> errorStr =<< cWriteMessage devId
                    (encodeTime ts) (fromIntegral len) (Foreign.castPtr bytesp)
            _ -> pure $ Just $ "device not in open WriteDevices: "
                <> pretty (Map.keys writes) <> ": " <> pretty dev

foreign import ccall "core_midi_write_message"
    cWriteMessage :: CInt -> CTimestamp -> CInt -> Ptr Word8 -> IO OSStatus

-- * misc

-- | Clear all pending msgs.
abort :: IO ()
abort = void $ check =<< cAbort
foreign import ccall "core_midi_abort" cAbort :: IO OSStatus

-- | Get current timestamp.
now :: IO RealTime
now = fmap decodeTime cGetNow
foreign import ccall "core_midi_get_now" cGetNow :: IO CTimestamp

type CTimestamp = CULong

decodeTime :: CTimestamp -> RealTime
decodeTime = RealTime.milliseconds . fromIntegral

encodeTime :: RealTime -> CTimestamp
encodeTime = fromIntegral . max 0 . RealTime.toMilliseconds

-- * errors

-- | Log any error and return False if there was one.
--
-- I previously threw an exception, but I feel like killing the whole app
-- is overkill.
check :: HasCallStack => OSStatus -> IO Bool
check err = errorStr err >>= \case
    Nothing -> pure True
    Just msg -> do
        Log.error msg
        pure False

type OSStatus = Int.Int32

errorStr :: OSStatus -> IO (Maybe Error)
errorStr err
    | err == 0 = pure Nothing
    | otherwise = do
        cstr <- cGetError err
        let prefix = "CoreMIDI (" <> showt err <> "): "
        Just . (prefix <>) <$> FFI.peekCString cstr

foreign import ccall "GetMacOSStatusErrorString"
    cGetError :: OSStatus -> IO CString
