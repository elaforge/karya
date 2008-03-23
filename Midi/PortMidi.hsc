{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -XEmptyDataDecls #-}
{-# OPTIONS_GHC -XDeriveDataTypeable #-}
{- | Wrapper for the portmidi library.
-}
module Midi.PortMidi (
    ReadStream, WriteStream -- opaque
    , initialize, terminate

    -- * devices
    , devices
    -- no direct access to the constructor
    , Device, device_interface, device_name, device_input, device_output
    , device_id
    , open_input, open_output, close_input, close_output

    -- * reading and writing streams
    , Event(..), Timestamp
    , read_events, write_event

    -- * errors
    , Error(..), catch, throw
) where

import Prelude hiding (catch)
import Control.Monad
import qualified Control.Exception as Exception
import qualified Data.Word as Word
import Data.Typeable (Typeable)
import Foreign
import Foreign.C

#include "portmidi.h"
#include "porttime.h"

-- * initialization

initialize = c_initialize >> pt_start
foreign import ccall unsafe "Pm_Initialize" c_initialize :: IO ()
terminate = pt_stop >> c_terminate
foreign import ccall unsafe "Pm_Terminate" c_terminate :: IO ()

type CPtError = CInt -- internal
-- XXX assumes pt won't return an error
-- Initializing porttime means that the midi read and write functions will
-- use it for timing.  I assume without timing doesn't work?
-- porttime uses 1ms resolution.
pt_start = c_pt_start 1 nullPtr nullPtr >> return ()
foreign import ccall unsafe "Pt_Start" c_pt_start :: CInt -> Ptr () -> Ptr ()
    -> IO CPtError

pt_stop = c_pt_stop >> return ()
foreign import ccall unsafe "Pt_Stop" c_pt_stop :: IO CPtError


-- * devices

data Device = Device
    { device_interface :: String
    , device_name :: String
    , device_input :: Bool
    , device_output :: Bool
    , device_id :: CInt
    } deriving (Show, Eq)

devices :: IO [Device]
devices = do
    n <- c_count_devices
    devs <- mapM (\n -> c_get_device_info n >>= peek) [0..n-1]
    return $ zipWith (\dev id -> dev { device_id = id}) devs [0..n]

foreign import ccall unsafe "Pm_CountDevices" c_count_devices :: IO CInt
foreign import ccall unsafe "Pm_GetDeviceInfo"
    c_get_device_info :: CInt -> IO (Ptr Device)

instance Storable Device where
    sizeOf _ = #size PmDeviceInfo
    alignment _ = undefined
    peek = peek_device_info
    poke = undefined

peek_device_info dev = do
    interf <- (#peek PmDeviceInfo, interf) dev >>= peekCString
    name <- (#peek PmDeviceInfo, name) dev >>= peekCString
    input <- (#peek PmDeviceInfo, input) dev :: IO CInt
    output <- (#peek PmDeviceInfo, output) dev :: IO CInt
    -- device_id will be filled in later
    return $ Device interf name (toBool input) (toBool output) 0

-- | A stream you can read from.
newtype ReadStream = ReadStream (Ptr CStream)
-- | A stream you can write to.
newtype WriteStream = WriteStream (Ptr CStream)
data CStream
-- | Union both streams, for operations that work on either.
newtype Stream = Stream (Either ReadStream WriteStream)
streamp_of (Stream (Left (ReadStream streamp))) = streamp
streamp_of (Stream (Right (WriteStream streamp))) = streamp

-- | Input and output buffer size.
buffer_size :: Int
buffer_size = 512

open_input :: Device -> IO ReadStream
open_input dev = alloca $ \streampp -> checked
    (c_open_input streampp (device_id dev) nullPtr (fromIntegral buffer_size)
        nullPtr nullPtr)
    (fmap ReadStream (peek streampp))
foreign import ccall unsafe "Pm_OpenInput"
    -- stream, inputDevice, inputDriverInfo
    c_open_input :: Ptr (Ptr CStream) -> CInt -> Ptr ()
        -- bufferSize, time_proc, time_info
        -> CLong -> Ptr () -> Ptr () -> IO CPmError

open_output :: Device -> IO WriteStream
open_output dev = alloca $ \streampp -> checked
    -- Pass a latency of 1 because portmidi requires this to obey outgoing
    -- timestamps.  'write_event' should subtract this from outgoing timestamps.
    (c_open_output streampp (device_id dev) nullPtr (fromIntegral buffer_size)
        nullPtr nullPtr 1)
    (fmap WriteStream (peek streampp))
foreign import ccall unsafe "Pm_OpenOutput"
    -- stream, inputDevice, outputDriverInfo
    c_open_output :: Ptr (Ptr CStream) -> CInt -> Ptr ()
        -- bufferSize, time_proc, time_info, latency
        -> CLong -> Ptr () -> Ptr () -> CLong -> IO CPmError

close_input :: ReadStream -> IO ()
close_input (ReadStream streamp) = checked_ (c_close streamp)
close_output :: WriteStream -> IO ()
close_output (WriteStream streamp) = checked_ (c_close streamp)
foreign import ccall unsafe "Pm_Close" c_close :: Ptr CStream -> IO CPmError


-- * reading and writing streams

{- not implemented, default filter (active sensing) is ok
foreign import ccall unsafe "Pm_SetFilter"
    c_set_filter :: Ptr CStream -> CLong -> IO CPmError
foreign import ccall unsafe "Pm_SetChannelMask"
    c_set_channel_mask :: Ptr CStream -> CInt -> IO CPmError
-}

-- | A single MIDI event happening at the given Timestamp.  The event is
-- stored as uninterpreted bytes, but should represent exactly one midi msg.
newtype Event = Event ([Word.Word8], Timestamp) deriving (Show, Eq)
type Timestamp = Integer
-- defined as a long
type CTimestamp = (#type PmTimestamp)

-- | Read pending Events.  This function doesn't block.  Every Event has
-- 3 bytes, which means that some may have trailing 0s, and sysex msgs will be
-- spread across many Events, and may have realtime Events interspersed.
-- 'enqueue' gives a nicer interface.
read_events :: ReadStream -> IO [Event]
read_events (ReadStream streamp) = allocaArray buffer_size $ \evt_array -> do
    nread <- c_read streamp evt_array (fromIntegral buffer_size)
    check_errno nread
    peekArray (fromIntegral nread) evt_array
foreign import ccall unsafe "Pm_Read"
    c_read :: Ptr CStream -> Ptr Event -> CLong -> IO CPmError

write_event :: WriteStream -> Event -> IO ()
write_event (WriteStream streamp) evt@(Event (bytes, ts))
    | is_sysex bytes = withArray bytes $ \bytesp -> checked_ $
        -- I think it's ok to cast (Ptr Word8) to (Ptr CUChar)
        -- Subtract 1 from timstamp, see 'open_output'.
        c_write_sysex streamp (to_c_long (ts-1)) (castPtr bytesp)
    | otherwise = checked_ $
        c_write_short streamp (to_c_long (ts-1)) (encode_message bytes)

to_c_long = fromIntegral
is_sysex = (==0xf0) . head

foreign import ccall unsafe "Pm_WriteShort"
    c_write_short :: Ptr CStream -> CTimestamp -> CLong -> IO CPmError
foreign import ccall unsafe "Pm_WriteSysEx"
    c_write_sysex :: Ptr CStream -> CTimestamp -> Ptr CUChar -> IO CPmError

instance Storable Event where
    sizeOf _ = #size PmEvent
    alignment _ = undefined
    peek = peek_event
    poke = undefined

peek_event evt = do
    msg <- (#peek PmEvent, message) evt :: IO (#type PmMessage)
    time <- (#peek PmEvent, timestamp) evt :: IO CTimestamp
    return $ Event (decode_message msg, fromIntegral time)

-- could probably get hsc2hs to pull the macros, but I don't mind doing this
-- in haskell
-- QuickCheck property: (decode_message . encode_message) n == n
decode_message :: Int32 -> [Word.Word8]
decode_message msg = [shift 0, shift 8, shift 16, shift 24]
    where
    shift x = fromIntegral (shiftR msg x .&. 0xff)
encode_message :: [Word.Word8] -> CLong
encode_message bytes
    | length bytes <= 4 = sum $
        zipWith enc (bytes ++ replicate (length bytes - 4) 0) [0, 8, 16, 24]
    | otherwise = error $ "can't encode msg with > 4 bytes " ++ show bytes
    where enc byte offset = shiftL (fromIntegral byte .&. 0xff) offset

-- * errors (called internally)

-- | A PortMidi exception just has an error string.
newtype Error = Error String deriving (Show, Typeable)

type CPmError = CInt -- internal

-- | Catch PortMidi exceptions.
catch :: IO a -> (Error -> IO a) -> IO a
catch = Exception.catchDyn
throw :: String -> a
throw err = Exception.throwDyn (Error err)


-- | run 'op' and throw if there's a problem, otherwise return if_ok
checked :: IO CPmError -> IO a -> IO a -- but maybe throw
checked op if_ok = do
    errno <- op
    err <- get_error_str errno
    case err of
        Just err -> throw err
        Nothing -> if_ok

checked_ op = checked op (return ())

-- | check an errno if you want to keep it on non-errors
check_errno errno = checked_ (return errno)

-- | helper for checked
get_error_str :: CPmError -> IO (Maybe String)
get_error_str errno
    | (#const pmNoError) <= errno = return Nothing
    | (#const pmHostError) == errno = fmap Just get_host_error_text
    | otherwise = fmap Just (get_error_text errno)

get_error_text :: CPmError -> IO String
get_error_text errno = c_get_error_text errno >>= peekCString
foreign import ccall unsafe "Pm_GetErrorText"
    c_get_error_text :: CInt -> IO CString

-- | Get an asynchronous host error on the stream if there was one.
-- Not used currently.
host_error :: Stream -> IO (Maybe String)
host_error stream = do
    has_err <- has_host_error (streamp_of stream)
    if has_err then fmap Just get_host_error_text else return Nothing

foreign import ccall "Pm_HasHostError"
    has_host_error :: (Ptr CStream) -> IO Bool

get_host_error_text :: IO String
get_host_error_text = allocaBytes errlen $ \errp -> do
    c_get_host_error_text errp (fromIntegral errlen)
    peekCString errp
    where errlen = 256
foreign import ccall unsafe "Pm_GetHostErrorText"
    c_get_host_error_text :: CString -> CInt -> IO ()
