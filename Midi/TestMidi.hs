module Midi.TestMidi where
import Control.Monad
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TChan as TChan
import qualified Control.Exception as Exception
import qualified Data.Map as Map
import qualified Data.Word as Word
import Text.Printf (printf)

import qualified Util.Seq as Seq
import qualified Perform.Timestamp as Timestamp
import Util.PPrint

import qualified Midi.Midi as Midi
import qualified Midi.PortMidiC as MidiC

import qualified Midi.PortMidi

print_err = Exception.handle $
    \err -> putStrLn $ "err got out: " ++ show (err :: MidiC.Error)
main = MidiC.initialize $ \read_chan -> print_err $ do
    (rdevs, wdevs) <- MidiC.devices
    putStrLn "read devs:"
    mapM_ print (Map.keys rdevs)
    putStrLn "write devs:"
    mapM_ print (Map.keys wdevs)

    let out_dev = Map.keys wdevs !! 0
        in_dev = last (Map.elems rdevs)
    putStrLn $ "rdev: " ++ show in_dev ++ " wdev: " ++ show out_dev
    MidiC.open_read_device read_chan in_dev
    wstream <- MidiC.open_write_device (wdevs Map.! out_dev)

    let wdev_streams = Map.fromList [(out_dev, wstream)]
    let write_msg (dev, ts, msg) = do
            -- putStrLn $ "wr: " ++ show (dev, ts, msg)
            MidiC.write_msg (wdev_streams Map.! dev,
                MidiC.from_timestamp ts, msg)

    thru_loop write_msg out_dev read_chan
    -- play_melody write_msg out_dev
    -- thru_melody write_msg out_dev

write_sysex wstream write_msg = do
    Concurrent.forkIO $ write_msg $
        ( Midi.WriteDevice "IAC Driver IAC Bus 1/CoreMIDI"
        , Timestamp.immediately, big_sysex)
    Concurrent.threadDelay 10000
    Midi.PortMidi.abort wstream

big_sysex = Midi.CommonMessage
    (Midi.SystemExclusive 42 (take 10000 (cycle [0..9]) ++ [0xf7]))

thru_loop write_msg out_dev read_chan = forever $ do
    Midi.ReadMessage dev ts msg <- STM.atomically
        (TChan.readTChan read_chan)
    print (ts, dev, msg)
    let msgs = thru (ts, msg)
    -- print msgs
    sequence_ [write_msg (out_dev, 0, msg) | (ts, msg) <- msgs]

thru_melody write_msg out_dev = do
    let play msg = write_msg (mknote out_dev (0, msg))
    let notes = [70, 68 .. 60]
    forM_ (zip (notes++[0]) (head notes : notes)) $ \(key, last) -> do
        play (Midi.NoteOff last 0)
        when (key /= 0) $
            play (Midi.NoteOn key 100)
        Concurrent.threadDelay (4 * 100000)

play_melody write_msg wdev = mapM_ write_msg (melody wdev)

melody wdev = map (mknote wdev) $ concat
    [[(ts, Midi.NoteOn key vel), (ts+400, Midi.NoteOff key vel)]
    | (ts, key) <- zip [0, 500..] score]
    where
    vel = 70
    score = [53, 55 .. 61]

mknote wdev (ts, msg) =
    (wdev, Timestamp.Timestamp ts, Midi.ChannelMessage 0 msg)

thru (ts, msg) = case msg of
    Midi.ChannelMessage ch (Midi.NoteOn key vel)
        -> [(ts1, Midi.ChannelMessage ch (Midi.NoteOn key vel))]
            
    Midi.ChannelMessage ch (Midi.NoteOff key vel)
        -> [(ts1, Midi.ChannelMessage ch (Midi.NoteOff key vel))]
    m -> []
    where
    ts1 = ts + Timestamp.Timestamp 500

show_msg (Midi.CommonMessage (Midi.SystemExclusive manuf bytes))
    = printf "Sysex %x: [%s]" manuf (Seq.join ", " (map hex bytes))
show_msg m = show m

hex :: Word.Word8 -> String
hex = printf "0x%02x"
