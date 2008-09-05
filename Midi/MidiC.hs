{-# OPTIONS_GHC -XDeriveDataTypeable #-}
{- | Midi interface.
    - Read

    - Write

    - Thru

    - Bandwidth monitoring

    TODO
    see if I'll need to write out of order msgs, if so write scheduler:
    write: write msgs to heap in an array
    read: wait for now-newest - write_ahead secs or there has been a write,
    if scheduled time is <= now-write_ahead, send it
-}
module Midi.MidiC (
    -- * initialize, open and close
    initialize, ReadChan
    , devices
    , open_read_device, open_write_device
    , to_timestamp, from_timestamp
    -- * read and write
    , write_msg
    , abort
    -- * thru handling
    -- * bandwidth monitoring
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
import qualified System.IO.Unsafe as Unsafe

import qualified Util.Thread as Thread
import qualified Midi.PortMidi as PortMidi
import Midi.PortMidi (Error(..), catch, throw)
import qualified Midi.Parse as Parse
import qualified Midi.Midi as Midi
import qualified Perform.Timestamp as Timestamp


-- | TChan that produces ReadMessages.
-- Writes take WriteMessages, but reads produce Midi.ReadMessages
type ReadChan = TChan.TChan Midi.ReadMessage

-- | Run the given computation with the midi library initialized.
initialize :: (Midi.MidiC.ReadChan -> IO a) -> IO a
initialize = Exception.bracket
    (PortMidi.initialize >> TChan.newTChanIO) midi_terminate

midi_terminate _read_channel_ref = do
    thread_ids <- MVar.readMVar read_thread_ids
    -- The threads' finally handlers should close their input streams.
    mapM_ Concurrent.killThread thread_ids
    -- dev_map <- MVar.readMVar dev_to_write_stream
    -- mapM_ PortMidi.close_output (Map.elems dev_map)
    PortMidi.terminate

-- * Global MVars

-- | Record the read threads to kill them on shutdown.
-- This global could be eliminated by passing a ref from initialize.
read_thread_ids :: MVar.MVar [Concurrent.ThreadId]
read_thread_ids = Unsafe.unsafePerformIO (MVar.newMVar [])

-- * query

-- | Get a list of connected Devices.
devices :: IO (Map.Map Midi.ReadDevice PortMidi.ReadDevice,
    Map.Map Midi.WriteDevice PortMidi.WriteDevice)
devices = do
    (rdevs, wdevs) <- PortMidi.devices
    return (Map.fromList (key_on to_read_device rdevs),
        Map.fromList (key_on to_write_device wdevs))

key_on f xs = zip (map f xs) xs

-- | PortMidi has both interface and device name.  Interface is probably always
-- the same (e.g. CoreMIDI), but append it just for completeness.
-- Going name/interface instead of the other way around is a micro-optimization
-- since every midi msg write will compare devices.
to_read_device rdev = Midi.ReadDevice (name ++ "/" ++ interface)
    where
    interface = PortMidi.rdev_interface rdev
    name = PortMidi.rdev_name rdev

to_write_device wdev = Midi.WriteDevice (name ++ "/" ++ interface)
    where
    interface = PortMidi.wdev_interface wdev
    name = PortMidi.wdev_name wdev

from_timestamp :: Timestamp.Timestamp -> PortMidi.Timestamp
from_timestamp (Timestamp.Timestamp ts) = ts
to_timestamp :: PortMidi.Timestamp -> Timestamp.Timestamp
to_timestamp = Timestamp.Timestamp

-- | Start putting msgs from given device into the queue that 'read_msg' reads
-- from.
open_read_device :: ReadChan -> PortMidi.ReadDevice -> IO ()
open_read_device read_chan rdev = do
    stream <- PortMidi.open_input rdev
    -- This starts a separate thread for each input.  Would latency be better
    -- if enqueue took a list of inputs?  Then I'd have to keep separate sysex
    -- buffers for each one.
    th_id <- Thread.start_thread ("enqueue " ++ show rdev) $
        enqueue stream (enqueue_event read_chan rdev)
        `Exception.finally` PortMidi.close_input stream
    MVar.modifyMVar_ read_thread_ids (return . (th_id:))
    return ()
    where
    enqueue_event chan rdev (PortMidi.Event (bytes, ts)) =
        STM.atomically $ TChan.writeTChan chan
            (Midi.ReadMessage (to_read_device rdev) (to_timestamp ts)
                (Parse.decode bytes))

-- | Open the given device for writing.  Subsequent calls to 'write_msg' on
-- this device will work.
open_write_device :: PortMidi.WriteDevice -> IO PortMidi.WriteStream
open_write_device wdev = PortMidi.open_output wdev

-- | Write the given msg to 'dev'.  For the moment, 'timestamp' must always
-- be increasing for a given device.
write_msg :: (PortMidi.WriteStream, PortMidi.Timestamp, Midi.Message) -> IO ()
write_msg (stream, ts, msg) = do
    PortMidi.write_event stream (PortMidi.Event (Parse.encode msg, ts))
    -- put it in the bw monitor

abort :: PortMidi.WriteStream -> IO ()
abort = PortMidi.abort


-- * util

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
has_eox = any (==Midi.eox_byte) . evt_bytes
is_realtime = (>=0xf8) . head . evt_bytes
is_status = (>=0x80) . head . evt_bytes
is_sysex = (==Midi.sox_byte) . head . evt_bytes
merge_evts [] = error "can't merge 0 Events"
merge_evts evts@(PortMidi.Event (_, ts):_)
    = PortMidi.Event (concatMap evt_bytes evts, ts)
