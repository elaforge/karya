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
    with
    , Port, port_name, ports
    , open_read_port, open_write_port
    -- * read and write
    , read_msg , write_msg
    -- * thru handling
    -- * bandwidth monitoring
    -- * message types
    , Message(..), ChannelMessage(..), CommonMessage(..)
    , RealtimeMessage(..)
    , Channel, Key, Velocity, Controller, Program, ControlValue
    -- * errors
    , catch
) where

import Prelude hiding (catch)
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception
import qualified Data.Map as Map
import qualified Data.IORef as IORef
import Data.Typeable (Typeable)
import qualified System.IO.Unsafe as Unsafe

import qualified Util.Thread as Thread
import qualified Midi.PortMidi as PortMidi
import qualified Midi.Parse as Parse
import Midi.Parse (Message(..), ChannelMessage(..), CommonMessage(..),
    RealtimeMessage(..),
    Channel, Key, Velocity, Controller, Program, ControlValue)
import Midi.PortMidi (Timestamp)

-- | Run the given computation with the midi library initialized.
with :: IO a -> IO a
with = Exception.bracket_ PortMidi.initialize PortMidi.terminate

newtype Port = Port PortMidi.Device deriving (Show, Eq)
instance Ord Port where
    compare a b = compare (port_name a) (port_name b)

-- | PortMidi has both interface and device name.  Interface is probably always
-- the same (e.g. CoreMIDI), but prepend it to the port name just for
-- completeness.
port_name :: Port -> String
port_name (Port dev) = interface ++ "/" ++ name ++ "/" ++ io
    where
    interface = PortMidi.device_interface dev
    name = PortMidi.device_name dev
    io = if PortMidi.device_input dev then "input" else "output"

-- | Get a list of connected Ports.
ports :: IO [Port]
ports = PortMidi.devices >>= (return . map Port)

-- | Start putting msgs from given port into the queue that 'read_msg' reads
-- from.
open_read_port :: Port -> IO ()
open_read_port port@(Port dev) = do
    stream <- PortMidi.open_input dev
    -- This starts a separate thread for each input.  Would latency be better
    -- if enqueue took a list of inputs?  Then I'd have to keep separate sysex
    -- buffers for each one.
    chan <- IORef.readIORef read_channel_ref
    Thread.start_thread ("enqueue " ++ port_name port) $
        enqueue stream (enqueue_event chan port)
    return ()
    where
    enqueue_event chan port (PortMidi.Event (bytes, ts))
        = Chan.writeChan chan (port, ts, Parse.decode bytes)

-- | Open the given port for writing.  Subsequent calls to 'write_msg' on
-- this port will work.
open_write_port :: Port -> IO ()
open_write_port port@(Port dev) = do
    print dev
    stream <- PortMidi.open_output dev
    MVar.modifyMVar_ write_port_map (return . Map.insertWithKey err port stream)
    where
    err port _new_stream _old_stream
        = midi_error $ "write port already open: " ++ port_name port

-- | Get the next msg in the queue.
read_msg :: IO (Port, Timestamp, Message)
read_msg = IORef.readIORef read_channel_ref >>= Chan.readChan

write_port_map :: MVar.MVar (Map.Map Port PortMidi.WriteStream)
write_port_map = Unsafe.unsafePerformIO (MVar.newMVar Map.empty)

-- | All midi read functions dump messages into this channel.
read_channel_ref :: IORef.IORef (Chan.Chan (Port, Timestamp, Message))
read_channel_ref = Unsafe.unsafePerformIO (Chan.newChan >>= IORef.newIORef)

-- | Write the given msg to 'port'.  For the moment, 'timestamp' must always
-- be increasing for a given port.
-- TODO: solutions: write a scheduler
-- write a priority chan:
-- write_pchan tchan (ts, msg) = merge into tchan according to ts
-- read_pchan tchan = atomically $ if now-newest <= 1sec then newest else retry
-- actually, they're the same thing except the latter inserts some latency
-- for a buffer
write_msg :: Port -> Timestamp -> Message -> IO ()
write_msg port timestamp msg = do
    port_map <- MVar.readMVar write_port_map
    case Map.lookup port port_map of
        Nothing -> midi_error $ "port not open for writing: " ++ show port
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

-- * errors

newtype Error = Error String deriving (Show, Typeable)
midi_error = error
catch = PortMidi.catch
