{-# LANGUAGE ForeignFunctionInterface, DeriveDataTypeable #-}
{- | Interface to CoreMIDI.

TODO documentation
-}
module Midi.CoreMidi (
    ReadChan, ReadDeviceId, WriteDeviceId, ReadMap, WriteMap
    , initialize, get_devices

    , connect_read_device, write_message
    , abort, now
) where
import qualified Control.Exception as Exception
import qualified Control.Concurrent.STM as STM
import qualified Data.ByteString as ByteString
import qualified Data.Map as Map
import qualified Data.Typeable as Typeable

import Foreign
import Foreign.C

import qualified Midi.Midi as Midi
import qualified Midi.Parse as Parse

import qualified Perform.Timestamp as Timestamp


type CError = CULong
no_error :: CError
no_error = 0

type ReadChan = STM.TChan Midi.ReadMessage

type ReadCallback = SourcePtr -> CTimestamp -> CInt -> Ptr Word8 -> IO ()
type SourcePtr = StablePtr Midi.ReadDevice
type CTimestamp = CULong
newtype ReadDeviceId = ReadDeviceId CInt deriving (Show)
newtype WriteDeviceId = WriteDeviceId CInt deriving (Show)

type ReadMap = Map.Map Midi.ReadDevice ReadDeviceId
type WriteMap = Map.Map Midi.WriteDevice WriteDeviceId

initialize :: (ReadChan -> IO a) -> IO a
initialize app = do
    chan <- STM.newTChanIO
    Exception.bracket (make_read_callback (chan_callback chan))
        freeHaskellFunPtr $ \cb -> do
            check_ =<< c_initialize cb
            app chan `Exception.finally` terminate

foreign import ccall "core_midi_terminate" terminate :: IO ()
foreign import ccall "core_midi_initialize"
    c_initialize :: FunPtr ReadCallback -> IO CError
foreign import ccall "wrapper"
    make_read_callback :: ReadCallback -> IO (FunPtr ReadCallback)

chan_callback :: ReadChan -> ReadCallback
chan_callback chan sourcep ctimestamp len bytesp = do
    -- Oddly enough, even though ByteString is Word8, the ptr packing function
    -- wants CChar.
    bytes <- ByteString.packCStringLen (castPtr bytesp, fromIntegral len)
    rdev <- deRefStablePtr sourcep
    let rmsg = Midi.ReadMessage
                rdev (decode_timestamp ctimestamp) (Parse.decode bytes)
    STM.atomically $ STM.writeTChan chan rmsg


get_devices :: IO (ReadMap, WriteMap)
get_devices = do
    (rdev_ids, rdev_ns) <- get_devs c_get_read_devices
    (wdev_ids, wdev_ns) <- get_devs c_get_write_devices
    let rdevs = zip (map Midi.ReadDevice rdev_ns) (map ReadDeviceId rdev_ids)
        wdevs = zip (map Midi.WriteDevice wdev_ns) (map WriteDeviceId wdev_ids)
    return (Map.fromList rdevs, Map.fromList wdevs)
    where
    get_devs c_read = alloca $ \lenp -> alloca $ \idsp -> alloca $ \namesp -> do
        check_ =<< c_read lenp idsp namesp
        len <- fmap fromIntegral (peek lenp)
        id_array <- peek idsp
        ids <- peekArray len id_array
        free id_array
        name_array <- peek namesp
        cnames <- peekArray len name_array
        names <- mapM peekCString cnames
        mapM_ free cnames
        free name_array
        return (ids, names)

foreign import ccall "core_midi_get_read_devices"
    c_get_read_devices :: Ptr CInt -> Ptr (Ptr CInt) -> Ptr (Ptr CString)
        -> IO CError
foreign import ccall "core_midi_get_write_devices"
    c_get_write_devices :: Ptr CInt -> Ptr (Ptr CInt) -> Ptr (Ptr CString)
        -> IO CError

connect_read_device :: Midi.ReadDevice -> ReadDeviceId -> IO ()
connect_read_device rdev rdev_id = do
    sourcep <- newStablePtr rdev
    check_ =<< c_connect_read_device rdev_id (castStablePtrToPtr sourcep)

foreign import ccall "core_midi_connect_read_device"
    c_connect_read_device :: ReadDeviceId -> Ptr () -> IO CError

-- | Timestamp will be ignored for sysex msgs.
write_message :: WriteDeviceId -> Timestamp.Timestamp -> Midi.Message -> IO ()
write_message (WriteDeviceId wdev_id) ts msg = do
    -- I could probably avoid this copy by using unsafe unpack and then a
    -- ForeignPtr or something to keep the gc off it, but any sizable sysex
    -- will take forever to send anyway.
    ByteString.useAsCStringLen (Parse.encode msg) $ \(bytesp, len) ->
        check_ =<< c_write_message wdev_id
            (encode_timestamp ts) (fromIntegral len) (castPtr bytesp)

foreign import ccall "core_midi_write_message"
    c_write_message :: CInt -> CTimestamp -> CInt -> Ptr Word8 -> IO CError

-- | Clear all pending msgs.
abort :: IO ()
abort = check_ =<< c_abort
foreign import ccall "core_midi_abort" c_abort :: IO CError

-- | Get current timestamp.
now :: IO Timestamp.Timestamp
now = fmap decode_timestamp c_get_now
foreign import ccall "core_midi_get_now" c_get_now :: IO CTimestamp


-- * util

newtype Error = Error String deriving (Show, Typeable.Typeable)
instance Exception.Exception Error

throw :: String -> a
throw = Exception.throw . Error

check val err
    | err == no_error = val
    | otherwise = throw (show err)
check_ = check (return ())

decode_timestamp :: CTimestamp -> Timestamp.Timestamp
decode_timestamp = Timestamp.from_millis . fromIntegral

encode_timestamp :: Timestamp.Timestamp -> CTimestamp
encode_timestamp = fromIntegral . Timestamp.to_millis
