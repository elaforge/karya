module Midi.TestMidi where
import Control.Monad
import qualified Control.Exception as Exception
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Chan as Chan
import Text.Printf (printf)

import qualified Util.Seq as Seq
import qualified Util.Log as Log

import qualified Midi.Parse as Parse
import qualified Util.Thread as Thread

import qualified Midi.Midi as Midi

import DUtil

print_err = flip Midi.catch (\err -> putStrLn $ "err got out: " ++ show err)
main = Midi.with $ print_err $ do
    ports <- Midi.ports
    putStrLn "ports:"
    plist (map Midi.port_name ports)

    Midi.open_read_port (ports !! 0)
    Midi.open_write_port (ports !! 6)

    forever $ do
        (port, ts, msg) <- Midi.read_msg
        -- print (Midi.port_name port, ts, msg)
        let msgs = thru (ts, msg)
        print ((ts, msg), msgs)
        mapM_ (uncurry (Midi.write_msg (ports !! 6))) msgs

thru (ts, msg) = case msg of
    Parse.ChannelMessage ch (Parse.NoteOn key vel)
        -> [(ts1, Parse.ChannelMessage ch (Parse.NoteOn (key+12) vel)),
            (ts2, Parse.ChannelMessage ch (Parse.NoteOn (key+19) vel))]
            
    Parse.ChannelMessage ch (Parse.NoteOff key vel)
        -> [(ts1, Parse.ChannelMessage ch (Parse.NoteOff (key+12) vel)),
            (ts2, Parse.ChannelMessage ch (Parse.NoteOff (key+19) vel))]
    m -> []
    where
    ts1 = ts + 500
    ts2 = ts + 1000

show_msg (Parse.CommonMessage (Parse.SystemExclusive manuf bytes))
    = printf "Sysex %x: [%s]" manuf (Seq.join ", " (map Log.hex bytes))
show_msg m = show m
