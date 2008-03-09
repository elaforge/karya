module Midi.TestMidi where
import Control.Monad
import qualified Control.Exception as Exception
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Chan as Chan
import Text.Printf (printf)

import qualified Seq
import qualified Util.Log as Log

import qualified Midi.PortMidi as PortMidi
import qualified Midi.Parse as Parse

import DUtil

end = PortMidi.terminate >> putStrLn "bye"
main = Exception.bracket_ PortMidi.initialize end $ do
    devs <- PortMidi.devices
    putStrLn "devices:"
    plist (map PortMidi.device_name devs)
    chan <- Chan.newChan
    -- let enqueue = Chan.writeChan chan
    -- in_stream <- PortMidi.open_input (devs !! 0)
    out_stream <- PortMidi.open_output (devs !! 5)
    mapM_ (open_read chan) devs
    forever $ do
        PortMidi.Event (bytes, ts) <- Chan.readChan chan
        putStrLn $ show_msg (Parse.decode bytes)
        let thru_evts = map (echo ts . Parse.encode) (thru (Parse.decode bytes))
        mapM_ (PortMidi.write_event out_stream) thru_evts

echo ts bytes = PortMidi.Event (bytes, ts + 500)

open_read chan dev
    | PortMidi.device_input dev = do
        stream <- PortMidi.open_input dev
        Concurrent.forkIO (PortMidi.enqueue stream (Chan.writeChan chan))
        return ()
    | otherwise = return ()

thru msg = case msg of
    Parse.ChannelMessage ch (Parse.NoteOn key vel)
        -> [Parse.ChannelMessage ch (Parse.NoteOn (key+12) vel)]
    Parse.ChannelMessage ch (Parse.NoteOff key vel)
        -> [Parse.ChannelMessage ch (Parse.NoteOff (key+12) vel)]
    m -> []

show_msg (Parse.CommonMessage (Parse.SystemExclusive manuf bytes))
    = printf "Sysex %x: [%s]" manuf (Seq.join ", " (map Log.hex bytes))
show_msg m = show m

decode (PortMidi.Event (bytes, ts)) = Parse.decode bytes
