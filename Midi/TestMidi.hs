module Midi.TestMidi where
import Control.Monad
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TChan as TChan
import Text.Printf (printf)

import qualified Util.Seq as Seq
import qualified Util.Log as Log

import qualified Midi.Midi as Midi

import DUtil

print_err = flip Midi.catch (\err -> putStrLn $ "err got out: " ++ show err)
main = Midi.initialize $ print_err $ do
    devs <- Midi.devices
    putStrLn "devs:"
    plist (map Midi.device_name devs)

    Midi.open_read_device (devs !! 0)
    let out_dev = devs !! 5
    Midi.open_write_device out_dev

    read_chan <- Midi.get_read_chan
    forever $ do
        (dev, ts, msg) <- STM.atomically (TChan.readTChan read_chan)
        -- print (Midi.device_name dev, ts, msg)
        let msgs = thru (ts, msg)
        print ((ts, msg), msgs)
        sequence_ [Midi.write_msg (out_dev, ts, msg) | (ts, msg) <- msgs]

thru (ts, msg) = case msg of
    Midi.ChannelMessage ch (Midi.NoteOn key vel)
        -> [(ts1, Midi.ChannelMessage ch (Midi.NoteOn (key+12) vel))]
            
    Midi.ChannelMessage ch (Midi.NoteOff key vel)
        -> [(ts1, Midi.ChannelMessage ch (Midi.NoteOff (key+12) vel))]
    m -> []
    where
    ts1 = ts + 500

show_msg (Midi.CommonMessage (Midi.SystemExclusive manuf bytes))
    = printf "Sysex %x: [%s]" manuf (Seq.join ", " (map Log.hex bytes))
show_msg m = show m
