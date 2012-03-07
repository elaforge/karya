-- | Common interface for the MIDI drivers.
module Midi.Interface where
import qualified Control.Concurrent.STM.TChan as TChan

import qualified Midi.Midi as Midi
import qualified Perform.RealTime as RealTime


type ReadChan = TChan.TChan Midi.ReadMessage

-- | Produced by an @initialize@ function.
data Interface = Interface {
    -- | ReadMessages from the opened ReadDevices become available on this
    -- channel.
    read_channel :: ReadChan
    -- | Get currently connected read and write devices.
    , read_devices :: IO [Midi.ReadDevice]
    , write_devices :: IO [Midi.WriteDevice]

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
    , now :: IO RealTime.RealTime
    }
