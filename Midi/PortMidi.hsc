{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -XEmptyDataDecls #-}
{-

portmidi.h:
    For input, the buffersize specifies the number of input events to be
    buffered waiting to be read using Pm_Read(). For output, buffersize
    specifies the number of output events to be buffered waiting for output.
    (In some cases -- see below -- PortMidi does not buffer output at all
    and merely passes data to a lower-level API, in which case buffersize
    is ignored.)

However, it never appears to explain those "some cases".

    (In some cases -- see below -- PortMidi does not buffer output at all
    and merely passes data to a lower-level API, in which case buffersize
    is ignored.)

    (NOTE: time is measured relative to the time source indicated by time_proc.
    Timestamps are absolute, not relative delays or offsets.)

-}

module Midi.PortMidi (
    Stream -- opaque
    , initialize, terminate
    -- * errors
    , PmError

    -- * devices
    , devices
    -- no access to device_id, or to the constructor
    , Device, device_interface, device_name, device_input, device_output
    , open_input, open_output, close

    -- * reading and writing streams
    , Event(..)
    , read_events, enqueue
    , write_event
) where

import Control.Monad
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Chan as Chan
import qualified Data.Word as Word
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

data CStream
newtype Stream = Stream (Ptr CStream)

-- | Input and output buffer size.
buffer_size = 512 :: Int

open_input :: Device -> IO Stream
open_input dev = alloca $ \streampp -> checked
    (c_open_input streampp (device_id dev) nullPtr (fromIntegral buffer_size)
        nullPtr nullPtr)
    (fmap Stream (peek streampp))
foreign import ccall unsafe "Pm_OpenInput"
    -- stream, inputDevice, inputDriverInfo
    c_open_input :: Ptr (Ptr CStream) -> CInt -> Ptr ()
        -- bufferSize, time_proc, time_info
        -> CLong -> Ptr () -> Ptr () -> IO CPmError

open_output :: Device -> IO Stream
open_output dev = alloca $ \streampp -> checked
    -- Pass a latency of 1 because portmidi requires this to obey outgoing
    -- timestamps.  I don't know why.
    (c_open_output streampp (device_id dev) nullPtr (fromIntegral buffer_size)
        nullPtr nullPtr 1)
    (fmap Stream (peek streampp))
foreign import ccall unsafe "Pm_OpenOutput"
    -- stream, inputDevice, outputDriverInfo
    c_open_output :: Ptr (Ptr CStream) -> CInt -> Ptr ()
        -- bufferSize, time_proc, time_info, latency
        -> CLong -> Ptr () -> Ptr () -> CLong -> IO CPmError

close :: Stream -> IO ()
close (Stream streamp) = checked_ (c_close streamp)
foreign import ccall unsafe "Pm_Close" c_close :: Ptr CStream -> IO CPmError


-- * reading and writing streams

{- not implemented, default filter (active sensing) is ok
foreign import ccall unsafe "Pm_SetFilter"
    c_set_filter :: Ptr CStream -> CLong -> IO CPmError
foreign import ccall unsafe "Pm_SetChannelMask"
    c_set_channel_mask :: Ptr CStream -> CInt -> IO CPmError
-}

newtype Event = Event ([Word.Word8], Timestamp) deriving (Show, Eq)
type Timestamp = Integer
-- defined as a long
type CTimestamp = (#type PmTimestamp)

read_events :: Stream -> IO [Event]
read_events (Stream streamp) = allocaArray buffer_size $ \evt_array -> do
    nread <- c_read streamp evt_array (fromIntegral buffer_size)
    check_errno nread
    peekArray (fromIntegral nread) evt_array
foreign import ccall unsafe "Pm_Read"
    c_read :: Ptr CStream -> Ptr Event -> CLong -> IO CPmError


-- | Go into a loop, reading events from 'stream' and sticking them on 'chan'.
-- Never returns.
enqueue :: Stream -> Chan.Chan Event -> IO ()
enqueue stream chan = _enqueue Nothing []
    where
    wchan = Chan.writeChan chan
    delay = 1000 -- 1ms seems ok CPU-wise, and 10ms is audible
    _enqueue sysex [] = Concurrent.threadDelay delay
        >> read_events stream >>= _enqueue sysex
    _enqueue (Just sysex_evts) (evt:rest)
        -- realtime msgs may be interspersed in a sysex
        | is_realtime evt = wchan evt >> _enqueue (Just sysex_evts) rest
        -- sysex is terminated by either EOX or a non-realtime status
        -- (sysex was truncated)
        | has_eox evt || is_status evt
            = wchan (merge_evts (reverse (evt:sysex_evts)))
                >> _enqueue Nothing rest
        | otherwise = _enqueue (Just (evt:sysex_evts)) rest
    _enqueue Nothing (evt:rest)
        | is_sysex evt = _enqueue (Just [evt]) rest
        | otherwise = wchan evt >> _enqueue Nothing rest

evt_bytes (Event (bytes, _)) = bytes
has_eox = any (==0xf7) . evt_bytes
is_realtime = (>=0xf8) . head . evt_bytes
is_status = (>=0x80) . head . evt_bytes
is_sysex = (==0xf0) . head . evt_bytes
merge_evts [] = error "can't merge 0 Events"
merge_evts evts@(Event (_, ts):_) = Event (concatMap evt_bytes evts, ts)

write_event :: Stream -> Event -> IO ()
write_event (Stream streamp) evt@(Event (bytes, ts))
    | is_sysex evt = withArray bytes $ \bytesp -> checked_ $
        -- I think it's ok to cast (Ptr Word8) to (Ptr CUChar)
        c_write_sysex streamp (to_c_long ts) (castPtr bytesp)
    | otherwise = checked_ $
        c_write_short streamp (to_c_long ts) (encode_message bytes)

to_c_long = fromIntegral

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
decode_message :: Int32 -> [Word.Word8]
decode_message msg = [st, d1, d2]
    where
    [st, d1, d2] = map fromIntegral
        [msg .&. 0xff, shiftR msg 8 .&. 0xff, shiftR msg 16 .&. 0xff]
encode_message :: [Word.Word8] -> CLong
encode_message bytes
    | length bytes == 3 = let [st, d1, d2] = map fromIntegral bytes in
        shiftL d2 16 .&. 0xff0000 .|. shiftL d1 8 .&. 0xff00 .|. st .&. 0xff
    | otherwise = error $ "can't encode msg without 3 bytes: " ++ show bytes

-- * errors (called internally)

type CPmError = CInt -- internal
type PmError = String


-- | run 'op' and throw if there's a problem, otherwise return if_ok
checked :: IO CPmError -> IO a -> IO a -- but maybe throw
checked op if_ok = do
    errno <- op
    err <- get_error_str errno
    case err of
        Just errstr -> error $
            "PortMidi error " ++ show errno ++ ": " ++ errstr
        Nothing -> if_ok

checked_ op = checked op (return ())

-- | check an errno if you want to keep it on non-errors
check_errno errno = checked_ (return errno)

-- | helper for checked
get_error_str :: CPmError -> IO (Maybe PmError)
get_error_str errno
    | (#const pmNoError) <= errno = return Nothing
    | (#const pmHostError) == errno = fmap Just get_host_error_text
    | otherwise = fmap Just (get_error_text errno)

{-
host_error :: Stream -> IO (Maybe String)
host_error (Stream stream) = do
    has_err <- has_host_error stream
    if has_err then fmap Just get_host_error_text else return Nothing

foreign import ccall "Pm_HasHostError"
    has_host_error :: (Ptr CStream) -> IO Bool
-}

get_host_error_text :: IO String
get_host_error_text = return "hi there!"
{-
get_host_error_text = allocaBytes errlen $ \errp -> do
    c_get_host_error_text errp (fromIntegral errlen)
    peekCString errp
    where errlen = 256
foreign import ccall unsafe "Pm_GetHostErrorText"
    c_get_host_error_text :: CString -> CInt -> IO ()
-}

get_error_text :: CPmError -> IO PmError
get_error_text errno = c_get_error_text errno >>= peekCString
foreign import ccall unsafe "Pm_GetErrorText"
    c_get_error_text :: CInt -> IO CString


