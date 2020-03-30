-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Interface to CoreMIDI.

TODO documentation
-}
module Midi.CoreMidi (initialize) where
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as Exception

import qualified Data.ByteString as ByteString
import qualified Data.IORef as IORef
import qualified Data.Map as Map
import qualified Data.Set as Set

import           Foreign hiding (void)

import qualified Util.CUtil as CUtil
import qualified Util.Log as Log
import qualified Midi.Encode as Encode
import qualified Midi.Interface as Interface
import qualified Midi.Midi as Midi

import qualified Perform.RealTime as RealTime
import           Perform.RealTime (RealTime)

import           Control.Monad
import           Foreign.C
import           Global


type Error = Text

-- * initialize

initialize :: String -- ^ register this name with CoreMIDI
    -> (Midi.Message -> Bool) -- ^ read msgs that return false are filtered
    -> (Either Error (Interface.RawInterface Midi.WriteMessage) -> IO a)
    -> IO a
initialize app_name want_message app = do
    chan <- STM.newTChanIO
    client <- make_client
    with_read_cb chan $ \read_cb -> with_notify_cb client $ \notify_cb ->
        withCString app_name $ \app_namep -> do
            err_mvar <- MVar.newEmptyMVar
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
                err <- c_initialize app_namep read_cb notify_cb
                MVar.putMVar err_mvar (error_str err)
                when (error_str err == Nothing) c_cf_runloop_run
            err <- MVar.takeMVar err_mvar
            case err of
                Just err -> app (Left err)
                Nothing -> do
                    c_prime_runloop
                    app (Right (mkinterface client chan))
                        `Exception.finally` terminate
    where
    with_read_cb chan = Exception.bracket
        (make_read_callback (read_callback want_message chan))
        freeHaskellFunPtr
    with_notify_cb client = Exception.bracket
        (make_notify_callback (notify_callback client)) freeHaskellFunPtr
    mkinterface client chan = Interface.Interface
        { Interface.name = "CoreMIDI"
        , Interface.read_channel = chan
        , Interface.read_devices =
            map ((\d -> (d, [])) . Midi.read_device) <$> get_devices True
        , Interface.write_devices =
            map ((\d -> (d, [])) . Midi.write_device) <$> get_devices False
        , Interface.connect_read_device = connect_read_device client
        , Interface.disconnect_read_device = disconnect_read_device client
        , Interface.connect_write_device = connect_write_device client
        , Interface.write_message = write_message client
        , Interface.abort = abort
        , Interface.now = now
        }

type ReadCallback = SourcePtr -> CTimestamp -> CInt -> Ptr Word8 -> IO ()
type SourcePtr = StablePtr Midi.ReadDevice
-- typedef void (*ReadCallback)(void *p, Timestamp timestamp, int len,
--     const unsigned char *bytes);

type NotifyCallback = CString -> DeviceId -> CInt -> CInt -> IO ()
-- typedef void (*NotifyCallback)(const char *name, DeviceId dev_id,
--     int is_added, int is_read);

foreign import ccall "core_midi_terminate" terminate :: IO ()
foreign import ccall "core_midi_initialize"
    c_initialize :: CString -> FunPtr ReadCallback -> FunPtr NotifyCallback
        -> IO CError
foreign import ccall "wrapper"
    make_read_callback :: ReadCallback -> IO (FunPtr ReadCallback)
foreign import ccall "wrapper"
    make_notify_callback :: NotifyCallback -> IO (FunPtr NotifyCallback)
foreign import ccall "CFRunLoopRun" c_cf_runloop_run :: IO ()
foreign import ccall "core_midi_prime_runloop" c_prime_runloop :: IO ()

-- TODO this is run from the CoreMIDI callback.  The callback is supposed to
-- be low latency which means no allocation, but haskell has plenty of
-- allocation.  On the other hand, it hasn't been a problem in practice and
-- a separate thread monitoring a ringbuffer would just add more latency.
read_callback :: (Midi.Message -> Bool) -> Interface.ReadChan -> ReadCallback
read_callback want_message chan sourcep ctimestamp len bytesp = do
    -- Oddly enough, even though ByteString is Word8, the ptr packing function
    -- wants CChar.
    bytes <- ByteString.packCStringLen (castPtr bytesp, fromIntegral len)
    rdev <- deRefStablePtr sourcep
    let msg = Encode.decode bytes
    when (want_message msg) $ STM.atomically $ STM.writeTChan chan $
        Midi.ReadMessage rdev (decode_time ctimestamp) msg

notify_callback :: Client -> NotifyCallback
notify_callback client namep _dev_id c_is_added c_is_read = do
    -- I could make connect_read_device and connect_write_device that take
    -- the dev_id directly, but that's too much work.
    name <- CUtil.peekCString namep
    case (toBool c_is_added, toBool c_is_read) of
        (True, True) -> do
            let dev = Midi.read_device name
            reads <- IORef.readIORef (client_reads client)
            when (dev `Set.member` reads) $
                void $ connect_read_device client dev
        (True, False) -> do
            let dev = Midi.write_device name
            writes <- IORef.readIORef (client_writes client)
            when (dev `Map.member` writes) $
                void $ connect_write_device client dev
        -- I leave removes alone.  If a source disappears, I'll just stop
        -- getting msgs from it.  If a destination disappears, then writes
        -- to it will fail, exactly as if it weren't connected.  CoreMIDI
        -- doesn't let me look up the name and ID of disconnected devices
        -- anyway.
        _ -> return ()

data Client = Client {
    -- | I don't need to deal with DeviceIds for reads.
    client_reads :: IORef.IORef (Set.Set Midi.ReadDevice)
    , client_writes :: IORef.IORef (Map.Map Midi.WriteDevice (Maybe DeviceId))
    }

make_client :: IO Client
make_client = Client <$> IORef.newIORef Set.empty <*> IORef.newIORef Map.empty

type DeviceId = CInt

-- * devices

connect_read_device :: Client -> Midi.ReadDevice -> IO Bool
connect_read_device client dev = do
    maybe_dev_id <- lookup_device_id True (Midi.read_device_text dev)
    case maybe_dev_id of
        Nothing -> do
            -- This means I want the device if it ever gets plugged in.
            IORef.modifyIORef (client_reads client) (Set.insert dev)
            return False
        Just dev_id -> do
            -- CoreMIDI lets you attach an arbitrary pointer to each
            -- connection to identify msgs coming in on that connection.  So
            -- I can use a stable ptr to the ReadDevice and have the read
            -- callback directly get a ReadDevice without having to look
            -- anything up.
            sourcep <- newStablePtr dev
            ok <- check =<< c_connect_read_device dev_id
                (castStablePtrToPtr sourcep)
            if not ok then return False else do
                IORef.modifyIORef (client_reads client) (Set.insert dev)
                return True

foreign import ccall "core_midi_connect_read_device"
    c_connect_read_device :: CInt -> Ptr () -> IO CError

disconnect_read_device :: Client -> Midi.ReadDevice -> IO Bool
disconnect_read_device client dev = do
    maybe_dev_id <- lookup_device_id True (Midi.read_device_text dev)
    wanted <- Set.member dev <$> IORef.readIORef (client_reads client)
    IORef.modifyIORef (client_reads client) (Set.delete dev)
    case (maybe_dev_id, wanted) of
        (Nothing, True) -> return True
        (Just dev_id, _) -> check =<< c_disconnect_read_device dev_id
        _ -> return False

foreign import ccall "core_midi_disconnect_read_device"
    c_disconnect_read_device :: DeviceId -> IO CError

connect_write_device :: Client -> Midi.WriteDevice -> IO Bool
connect_write_device client dev = do
    -- CoreMIDI doesn't have a notion of connected write devices, they are
    -- all implicitly connected and you need only emit a msg with the
    -- appropriate device id.
    maybe_dev_id <- lookup_device_id False (Midi.write_device_text dev)
    case maybe_dev_id of
        Nothing -> do
            IORef.modifyIORef (client_writes client) (Map.insert dev Nothing)
            return False
        Just dev_id -> do
            IORef.modifyIORef (client_writes client)
                (Map.insert dev (Just dev_id))
            return True

lookup_device_id :: Bool -> Text -> IO (Maybe DeviceId)
lookup_device_id is_read dev = alloca $ \dev_idp -> do
    found <- CUtil.withText dev $ \devp ->
        c_lookup_device_id (fromBool is_read) devp dev_idp
    if found == 0 then return Nothing
        else Just <$> peek dev_idp

foreign import ccall "lookup_device_id"
    c_lookup_device_id :: CInt -> CString -> Ptr DeviceId -> IO CInt

get_devices :: Bool -> IO [Text]
get_devices is_read = alloca $ \namesp -> do
    len <- c_get_devices (fromBool is_read) namesp
    name_array <- peek namesp
    cnames <- peekArray (fromIntegral len) name_array
    names <- mapM CUtil.peekCString cnames
    mapM_ free cnames
    free name_array
    return names

foreign import ccall "get_devices"
    c_get_devices :: CInt -> Ptr (Ptr CString) -> IO CInt

-- * write

-- | RealTime will be ignored for sysex msgs.
write_message :: Client -> Midi.WriteMessage -> IO (Maybe Error)
write_message client (Midi.WriteMessage dev ts msg) = do
    -- I could probably avoid this copy by using unsafe unpack and then a
    -- ForeignPtr or something to keep the gc off it, but any sizable sysex
    -- will take forever to send anyway.
    writes <- IORef.readIORef (client_writes client)
    case Map.lookup dev writes of
        Just (Just dev_id) -> ByteString.useAsCStringLen (Encode.encode msg) $
            \(bytesp, len) -> error_str <$> c_write_message dev_id
                (encode_time ts) (fromIntegral len) (castPtr bytesp)
        _ -> return $ Just $
            "device not in " <> pretty (Map.keys writes) <> ": " <> pretty dev

foreign import ccall "core_midi_write_message"
    c_write_message :: CInt -> CTimestamp -> CInt -> Ptr Word8 -> IO CError

-- * misc

-- | Clear all pending msgs.
abort :: IO ()
abort = void $ check =<< c_abort
foreign import ccall "core_midi_abort" c_abort :: IO CError

-- | Get current timestamp.
now :: IO RealTime
now = fmap decode_time c_get_now
foreign import ccall "core_midi_get_now" c_get_now :: IO CTimestamp

type CTimestamp = CULong

decode_time :: CTimestamp -> RealTime
decode_time = RealTime.milliseconds . fromIntegral

encode_time :: RealTime -> CTimestamp
encode_time = fromIntegral . max 0 . RealTime.to_milliseconds

-- * errors

-- | Log any error and return False if there was one.
--
-- I previously threw an exception, but I feel like killing the whole app
-- is overkill.
check :: CError -> IO Bool
check err = case error_str err of
    Nothing -> return True
    Just msg -> do
        Log.error msg
        return False

type CError = CULong

-- TODO look up actual error msgs
error_str :: CError -> Maybe Error
error_str err
    | err == 0 = Nothing
    | otherwise = Just $ "CoreMIDI error: " <> showt err
