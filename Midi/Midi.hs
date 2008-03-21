{-# OPTIONS_GHC -XDeriveDataTypeable #-}
{- | Midi interface.
Read
Write
Thru
Bandwidth monitoring

TODO
see if I'll need to write out of order msgs, if so write scheduler:
write: write msgs to heap in an array
read: wait for now-newest - write_ahead secs or there has been a write,
if scheduled time is <= now-write_ahead, send it
-}
module Midi.Midi (
    -- * initialize, open and close
    initialize
    , Device, device_name, device_input, device_output
    , devices
    , open_read_device, open_write_device
    -- * read and write
    , get_read_chan , write_msg
    -- * thru handling
    -- * bandwidth monitoring
    -- * message types
    , CompleteMessage
    , Message(..), ChannelMessage(..), CommonMessage(..)
    , RealtimeMessage(..)
    , Channel, Key, Velocity, Controller, Program, ControlValue
    -- * errors
    , Error(..), catch, throw
) where

import Prelude hiding (catch)
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TChan as TChan
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception
import qualified Data.Map as Map
import qualified Data.IORef as IORef
import Data.Typeable (Typeable)
import qualified System.IO.Unsafe as Unsafe

import qualified Util.Thread as Thread
import qualified Midi.PortMidi as PortMidi
import Midi.PortMidi (Device, device_input, device_output, Timestamp,
    Error(..), catch, throw)
import qualified Midi.Parse as Parse
import Midi.Parse (Message(..), ChannelMessage(..), CommonMessage(..),
    RealtimeMessage(..),
    Channel, Key, Velocity, Controller, Program, ControlValue)

-- | Run the given computation with the midi library initialized.
initialize :: IO () -> IO ()
initialize = Exception.bracket_ PortMidi.initialize terminate

terminate = do
    thread_ids <- MVar.readMVar read_thread_ids
    -- The threads' finally handlers should close their input streams.
    mapM_ Concurrent.killThread thread_ids
    dev_map <- MVar.readMVar dev_to_write_stream
    mapM_ PortMidi.close_output (Map.elems dev_map)
    PortMidi.terminate

-- | PortMidi has both interface and device name.  Interface is probably always
-- the same (e.g. CoreMIDI), but prepend it to the device name just for
-- completeness.
device_name :: Device -> String
device_name dev = interface ++ "/" ++ name ++ "/" ++ io
    where
    interface = PortMidi.device_interface dev
    name = PortMidi.device_name dev
    io = if PortMidi.device_input dev then "input" else "output"

-- This is just so I can put them in a Map, which doesn't let you pass
-- your own ordering function.
instance Ord Device where
    compare a b = compare (PortMidi.device_id a) (PortMidi.device_id b)

-- | Get a list of connected Devices.
devices :: IO [Device]
devices = PortMidi.devices

-- | Start putting msgs from given device into the queue that 'read_msg' reads
-- from.

open_read_device :: Device -> IO ()
open_read_device dev = do
    stream <- PortMidi.open_input dev
    -- This starts a separate thread for each input.  Would latency be better
    -- if enqueue took a list of inputs?  Then I'd have to keep separate sysex
    -- buffers for each one.
    chan <- IORef.readIORef read_channel_ref
    th_id <- Thread.start_thread ("enqueue " ++ device_name dev) $
        enqueue stream (enqueue_event chan dev)
        `Exception.finally` PortMidi.close_input stream
    MVar.modifyMVar_ read_thread_ids (return . (th_id:))
    return ()
    where
    enqueue_event chan dev (PortMidi.Event (bytes, ts))
        = STM.atomically $ TChan.writeTChan chan (dev, ts, Parse.decode bytes)

-- | Open the given device for writing.  Subsequent calls to 'write_msg' on
-- this device will work.
open_write_device :: Device -> IO ()
open_write_device dev = do
    stream <- PortMidi.open_output dev
    MVar.modifyMVar_ dev_to_write_stream
        (return . Map.insertWithKey err dev stream)
    where
    err dev _new_stream _old_stream
        = throw $ "write dev already open: " ++ device_name dev

type CompleteMessage = (Device, Timestamp, Message)

-- | Get a TChan that produces CompleteMessages
get_read_chan :: IO (TChan.TChan CompleteMessage)
get_read_chan = IORef.readIORef read_channel_ref

-- | A global map of open write devs.  It might be more elegant to just pass
-- the open stream to write_msg, but for now it seems easier to not expose
-- the streams to the outside world.
dev_to_write_stream :: MVar.MVar (Map.Map Device PortMidi.WriteStream)
dev_to_write_stream = Unsafe.unsafePerformIO (MVar.newMVar Map.empty)

read_thread_ids :: MVar.MVar [Concurrent.ThreadId]
read_thread_ids = Unsafe.unsafePerformIO (MVar.newMVar [])

-- | All midi read functions dump messages into this channel.
read_channel_ref :: IORef.IORef (TChan.TChan CompleteMessage)
read_channel_ref = Unsafe.unsafePerformIO (TChan.newTChanIO >>= IORef.newIORef)

-- | Write the given msg to 'dev'.  For the moment, 'timestamp' must always
-- be increasing for a given device.
-- TODO: solutions: write a scheduler
-- write a priority chan:
-- write_pchan tchan (ts, msg) = merge into tchan according to ts
-- read_pchan tchan = atomically $ if now-newest <= 1sec then newest else retry
-- actually, they're the same thing except the latter inserts some latency
-- for a buffer
write_msg :: CompleteMessage -> IO ()
write_msg (dev, timestamp, msg) = do
    dev_map <- MVar.readMVar dev_to_write_stream
    case Map.lookup dev dev_map of
        Nothing -> throw $ "device not open for writing: " ++ show dev
        Just stream -> PortMidi.write_event stream
            (PortMidi.Event (Parse.encode msg, timestamp))
    -- put it in the bw monitor


-- Go into a loop, reading events from 'stream' and sticking them on 'chan'.
-- Unlike 'read_events', each Event represents one MIDI msg, even if it's a
-- long sysex.  Never returns.
enqueue :: PortMidi.ReadStream -> (PortMidi.Event -> IO ()) -> IO ()
enqueue stream wchan = _enqueue Nothing []
    where
    delay = 1000 -- 1ms seems ok CPU-wise, and 10ms is audible
    _enqueue sysex [] = Concurrent.threadDelay delay
        >> PortMidi.read_events stream >>= _enqueue sysex
    _enqueue (Just sysex_evts) (evt:rest)
        -- realtime msgs may be interspersed in a sysex
        | is_realtime evt = wchan evt >> _enqueue (Just sysex_evts) rest
        | has_eox evt = wchan (merge_evts (reverse (evt:sysex_evts)))
            >> _enqueue Nothing rest
        -- non-realtime status byte means the sysex was truncated
        | is_status evt = wchan (merge_evts (reverse (sysex_evts)))
            >> _enqueue Nothing (evt:rest)
        | otherwise = _enqueue (Just (evt:sysex_evts)) rest
    _enqueue Nothing (evt:rest)
        | is_sysex evt = _enqueue (Just [evt]) rest
        | otherwise = wchan evt >> _enqueue Nothing rest

evt_bytes (PortMidi.Event (bytes, _)) = bytes
has_eox = any (==0xf7) . evt_bytes
is_realtime = (>=0xf8) . head . evt_bytes
is_status = (>=0x80) . head . evt_bytes
is_sysex = (==0xf0) . head . evt_bytes
merge_evts [] = error "can't merge 0 Events"
merge_evts evts@(PortMidi.Event (_, ts):_)
    = PortMidi.Event (concatMap evt_bytes evts, ts)
