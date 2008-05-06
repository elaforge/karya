module Midi.TestMidi where
import Control.Monad
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TChan as TChan
import qualified Data.Map as Map
import qualified Data.Word as Word
import Text.Printf (printf)

import qualified Util.Seq as Seq
import qualified Derive.Timestamp as Timestamp

import qualified Midi.Midi as Midi
import qualified Midi.MidiC as MidiC


print_err = flip MidiC.catch (\err -> putStrLn $ "err got out: " ++ show err)
main = MidiC.initialize $ \read_chan -> print_err $ do
    (rdevs, wdevs) <- MidiC.devices
    putStrLn "read devs:"
    mapM_ print (Map.keys rdevs)
    putStrLn "write devs:"
    mapM_ print (Map.keys wdevs)

    MidiC.open_read_device read_chan (Map.elems rdevs !! 0)
    let out_dev = Map.keys wdevs !! 0
    wstream <- MidiC.open_write_device (wdevs Map.! out_dev)

    let wdev_streams = Map.fromList [(out_dev, wstream)]
    let write_msg (dev, ts, msg) = MidiC.write_msg
            (wdev_streams Map.! dev, MidiC.from_timestamp ts, msg)

    forever $ do
        (dev, ts, msg) <- STM.atomically (TChan.readTChan read_chan)
        -- print (MidiC.device_name dev, ts, msg)
        let msgs = thru (ts, msg)
        print ((ts, msg), msgs)
        sequence_ [write_msg (out_dev, ts, msg) | (ts, msg) <- msgs]

thru (ts, msg) = case msg of
    Midi.ChannelMessage ch (Midi.NoteOn key vel)
        -> [(ts1, Midi.ChannelMessage ch (Midi.NoteOn (key+12) vel))]
            
    Midi.ChannelMessage ch (Midi.NoteOff key vel)
        -> [(ts1, Midi.ChannelMessage ch (Midi.NoteOff (key+12) vel))]
    m -> []
    where
    ts1 = ts + Timestamp.Timestamp 500

show_msg (Midi.CommonMessage (Midi.SystemExclusive manuf bytes))
    = printf "Sysex %x: [%s]" manuf (Seq.join ", " (map hex bytes))
show_msg m = show m

hex :: Word.Word8 -> String
hex = printf "0x%02x"
