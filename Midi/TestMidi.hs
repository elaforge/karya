module Midi.TestMidi where
import Control.Monad
import qualified Control.Exception as Exception
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Chan as Chan

import qualified Midi.PortMidi as PortMidi
import qualified Midi.Parse as Parse

import DUtil

end = PortMidi.terminate >> putStrLn "bye"
main = Exception.bracket_ PortMidi.initialize end $ do
    devs <- PortMidi.devices
    plist devs
    chan <- Chan.newChan
    in_stream <- PortMidi.open_input (devs !! 0)
    out_stream <- PortMidi.open_output (devs !! 5)
    Concurrent.forkIO (PortMidi.enqueue in_stream chan)
    forever $ do
        PortMidi.Event (bytes, ts) <- Chan.readChan chan
        print $ Parse.decode bytes
        let thru_evts = map (echo ts . Parse.encode) (thru (Parse.decode bytes))
        mapM_ (PortMidi.write_event out_stream) thru_evts

echo ts bytes = PortMidi.Event (bytes, ts + 500)

thru msg = case msg of
    Parse.ChannelMessage ch (Parse.NoteOn key vel)
        -> [Parse.ChannelMessage ch (Parse.NoteOn (key+12) vel)]
    Parse.ChannelMessage ch (Parse.NoteOff key vel)
        -> [Parse.ChannelMessage ch (Parse.NoteOff (key+12) vel)]
    m -> []

decode (PortMidi.Event (bytes, ts)) = Parse.decode bytes
