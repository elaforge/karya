module Midi.TestMidi where
import Control.Monad
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TChan as TChan
import qualified Control.Exception as Exception
import Text.Printf (printf)

import qualified Util.Seq as Seq
import qualified Util.Log as Log

import qualified Midi.Parse as Parse
import qualified Util.Thread as Thread

import qualified Midi.Midi as Midi

import DUtil

print_err = flip Midi.catch (\err -> putStrLn $ "err got out: " ++ show err)
main = Midi.initialize $ print_err $ do
    devs <- Midi.devices
    putStrLn "devs:"
    plist (map Midi.device_name devs)

    Midi.open_read_device (devs !! 0)
    Midi.open_write_device (devs !! 5)

    read_chan <- Midi.get_read_chan
    forever $ do
        (dev, ts, msg) <- STM.atomically (TChan.readTChan read_chan)
        -- print (Midi.device_name dev, ts, msg)
        let msgs = thru (ts, msg)
        print ((ts, msg), msgs)
        mapM_ (uncurry (Midi.write_msg (devs !! 5))) msgs

thru (ts, msg) = case msg of
    Parse.ChannelMessage ch (Parse.NoteOn key vel)
        -> [(ts1, Parse.ChannelMessage ch (Parse.NoteOn (key+12) vel))]
            
    Parse.ChannelMessage ch (Parse.NoteOff key vel)
        -> [(ts1, Parse.ChannelMessage ch (Parse.NoteOff (key+12) vel))]
    m -> []
    where
    ts1 = ts + 500

show_msg (Parse.CommonMessage (Parse.SystemExclusive manuf bytes))
    = printf "Sysex %x: [%s]" manuf (Seq.join ", " (map Log.hex bytes))
show_msg m = show m
