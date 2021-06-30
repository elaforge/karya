-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
-- | Test MIDI bindings, automatically and manually.
module Midi.TestMidi where
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM as STM
import Control.Monad
import qualified Numeric

import qualified Data.ByteString as ByteString
import qualified Data.Text.IO as Text.IO
import qualified Data.Time as Time
import qualified System.Environment
import qualified System.IO as IO

import qualified Util.Num as Num
import qualified Util.Seq as Seq
import qualified Util.Test as Test

#include "hsconfig.h"
#if defined(CORE_MIDI)
import qualified Midi.CoreMidi as MidiDriver
#elif defined(JACK_MIDI)
import qualified Midi.JackMidi as MidiDriver
#else
-- Not much point, but at least lets it compile.
import qualified Midi.StubMidi as MidiDriver
#endif

import qualified Midi.Interface as Interface
import qualified Midi.Midi as Midi
import qualified Midi.Mmc as Mmc
import qualified Midi.Encode

import qualified Perform.RealTime as RealTime
import Global


type Interface = Interface.RawInterface Midi.WriteMessage

main :: IO ()
main = MidiDriver.initialize "test_midi" want_message test_midi
    where
    want_message =
        (/= Midi.Encode.encode (Midi.RealtimeMessage Midi.ActiveSense))

type ReadMsg = IO (Maybe Midi.ReadMessage)
type WriteMsg = (RealTime.RealTime, Midi.Message) -> IO ()

test_midi :: Either Text Interface -> IO ()
test_midi (Left err) = errorIO $ "initializing midi: " <> err
test_midi (Right interface) = do
    rdevs <- Interface.read_devices interface
    putStrLn "read devs:"
    mapM_ (putStrLn . ("    " <>) . prettys) rdevs
    wdevs <- Interface.write_devices interface
    putStrLn "write devs:"
    mapM_ (putStrLn . ("    " <>) . prettys) wdevs
    rdevs <- return $ map fst rdevs

    let open = open_devs interface
    args <- System.Environment.getArgs
    case args of
        [] -> do
            putStrLn "monitoring (pass arg 'help' for help)"
            (_, read_msg) <- open True rdevs Nothing
            monitor read_msg
        ["record-sysex"] -> do
            (_, read_msg) <- open True rdevs Nothing
            record_sysex read_msg
        ["send-sysex", out_dev, fname] -> do
            (write_msg, _) <- open True [] (Just out_dev)
            send_sysex write_msg fname
        ["help"] -> putStrLn usage
        ["melody", out_dev] -> do
            putStrLn "playing melody"
            (write_msg, _) <- open True [] (Just out_dev)
            melody interface write_msg
            putStrLn "return to quit... "
            void getLine
        ["melody-thru", out_dev] -> do
            putStrLn "playing melody + thru"
            (write_msg, read_msg) <- open True rdevs (Just out_dev)
            thru_melody interface write_msg read_msg
        ["mmc", msg, out_dev] -> do
            (write_msg, _) <- open True [] (Just out_dev)
            mmc write_msg msg
        ("monitor" : mdevs@(_:_)) -> do
            putStrLn $ "monitoring: " ++ Seq.join ", " mdevs
            (_, read_msg) <-
                open True (map (Midi.read_device . txt) mdevs) Nothing
            monitor read_msg
        ["spam", out_dev, n_str] -> do
            putStrLn $ "spamming " ++ n_str ++ " msgs"
            (write_msg, _) <- open True rdevs (Just out_dev)
            n <- readIO n_str
            spam interface write_msg n
            getChar
            return ()
        ["pb-range", out_dev, semis] -> do
            semis <- readIO semis
            (write_msg, _) <- open True rdevs (Just out_dev)
            pitch_bend_range write_msg semis
        ["program", out_dev] ->
            uncurry program_change =<< open False [] (Just out_dev)
        ["test", loopback] -> do
            (write_msg, read_msg) <- open False
                [Midi.read_device (txt loopback)] (Just loopback)
            run_tests interface write_msg read_msg
        ["thru", out_dev] -> do
            putStrLn "playing thru"
            (write_msg, read_msg) <- open True rdevs (Just out_dev)
            thru_loop write_msg read_msg
        _ -> do
            putStrLn "unknown command"
            putStrLn usage
    where
    open_devs :: Interface -> Bool -> [Midi.ReadDevice]
        -> Maybe String -> IO (WriteMsg, ReadMsg)
    open_devs interface blocking rdevs maybe_wdev = do
        oks <- mapM (Interface.connect_read_device interface) rdevs
        forM_ [rdev | (rdev, False) <- zip rdevs oks] $ \missing ->
            putStrLn $ "rdev not found: " ++ show missing
        let read_msg = (if blocking then blocking_get else nonblocking_get)
                (Interface.read_channel interface)
        case Midi.write_device . txt <$> maybe_wdev of
            Nothing -> return
                (const (error "write device not opened"), read_msg)
            Just wdev -> do
                ok <- Interface.connect_write_device interface wdev
                unless ok $
                    error $ "required wdev " ++ show wdev ++ " not found"
                return (make_write_msg interface wdev, read_msg)
    make_write_msg interface wdev (ts, msg) = do
        putStrLn $ "write: " <> show ts <> ": " <> show msg
        mb_err <- Interface.write_message interface
            (Midi.WriteMessage wdev ts msg)
        whenJust mb_err $ Text.IO.putStrLn . ("error: "<>)

nonblocking_get :: Interface.ReadChan -> ReadMsg
nonblocking_get read_chan = STM.atomically $
    fmap Just (STM.readTChan read_chan) `STM.orElse` return Nothing

blocking_get :: Interface.ReadChan -> ReadMsg
blocking_get read_chan = STM.atomically $ fmap Just (STM.readTChan read_chan)

usage :: String
usage =
    "(no arg)             monitor all inputs\n\
    \record-sysex         save incoming sysex msgs to files\n\
    \send-sysex <out> fn  read a raw sysex from fn and send it to the port\n\
    \monitor <a> <b> ...  monitor input ports 'a' and 'b'\n\
    \help                 print this usage\n\
    \thru <out>           msgs from any input are relayed to <out>\n\
    \melody <out>         play a melody on <out>\n\
    \melody-thru <out>    play a melody on <out>, also relaying msgs thru\n\
    \spam <out> n         spam <out> with 'n' msgs in rapid succession\n\
    \test                 run some semi-automatic tests\n\
    \pb-range <out> n     send pitch bend range\n"


-- * program change

program_change :: WriteMsg -> ReadMsg -> IO ()
program_change write_message _read_message = forever $ do
    putStr "> "
    IO.hFlush IO.stdout
    line <- getLine
    let msg = Midi.ChannelMessage 0 $ Midi.ProgramChange (read line)
    print msg
    write_message (0, msg)

-- * sysex

record_sysex :: ReadMsg -> IO ()
record_sysex read_msg = loop 0
    where
    loop n = do
        Just (Midi.ReadMessage _ _ msg) <- read_msg
        wrote <- case msg of
            Midi.CommonMessage (Midi.SystemExclusive manuf bytes) -> do
                let fn = "record-sysex" ++ show n ++ ".syx"
                putStrLn $ "sysex " ++ untxt (Num.hex 2 manuf) ++ " "
                    ++ show (ByteString.length bytes) ++ " bytes -> "
                    ++ fn
                ByteString.writeFile fn (Midi.Encode.encode msg)
                return True
            _ -> return False
        loop (if wrote then n+1 else n)

send_sysex :: WriteMsg -> FilePath -> IO ()
send_sysex write_msg fname = do
    msg <- Midi.Encode.decode <$> ByteString.readFile fname
    write_msg (0, msg)
    putStrLn "sending asynchronously, hit return when the blinkenlights stop"
    _ <- getLine
    return ()

-- * monitor

monitor :: ReadMsg -> IO ()
monitor read_msg = forever $ do
    Just (Midi.ReadMessage dev ts msg) <- read_msg
    print (ts, dev, msg)


-- * thru

thru_loop :: WriteMsg -> ReadMsg -> IO ()
thru_loop write_msg read_msg = forever $ do
    Just (Midi.ReadMessage dev ts msg) <- read_msg
    putStrLn $ "thru: " ++ show (ts, dev, msg)
    write_msg (0, msg)


-- * melody

-- | Play a melody while also allowing msgs thru, to test merging.
thru_melody :: Interface -> WriteMsg -> ReadMsg -> IO ()
thru_melody interface write_msg read_msg = do
    melody interface write_msg
    thru_loop write_msg read_msg

melody :: Interface -> WriteMsg -> IO ()
melody interface write_msg = do
    now <- Interface.now interface
    mapM_ write_msg (notes now)

-- | Write notes over time.
notes :: RealTime.RealTime -> [(RealTime.RealTime, Midi.Message)]
notes start_ts = concat
    [[(ts, note_on nn), (ts + RealTime.seconds 0.4, note_off nn)]
        | (ts, nn) <- zip (Seq.range_ start_ts (RealTime.seconds 0.5)) score]
    where score = [53, 55 .. 61]

-- * mmc

mmc :: WriteMsg -> String -> IO ()
mmc write_msg msg = write_msg (0, Mmc.encode 127 mmc_msg)
    where
    mmc_msg
        | Just smpte <- parse_smpte msg = Mmc.Goto smpte 0
        | msg == "play" = Mmc.Play
        | msg == "stop" = Mmc.Stop
        | otherwise = error $ "unknown msg: " ++ msg

parse_smpte :: String -> Maybe Midi.Smpte
parse_smpte txt = do
    [h, m, s] <- Just $ Seq.split ":" txt
    Midi.Smpte <$> int h <*> int m <*> int s <*> return 0
    where
    int s = case Numeric.readDec s of
        (n, "") : _ -> Just n
        _ -> Nothing


-- * spam

spam :: Interface -> WriteMsg -> Int -> IO ()
spam interface write_msg n = do
    now <- Interface.now interface
    mapM_ write_msg [(now + RealTime.milliseconds (i*10), msg)
        | (i, msg) <- zip [0..] msgs]
    where
    msgs = take n [Midi.ChannelMessage 0 msg | nn <- cycle [0..127],
        msg <- [Midi.NoteOn nn 127, Midi.NoteOff nn 127]]

-- * pitch bend range

pitch_bend_range :: WriteMsg -> Double -> IO ()
pitch_bend_range write_msg semis = do
    let msgs = map (Midi.ChannelMessage 0) (Midi.pitch_bend_range semis)
    Test.pprint msgs
    mapM_ (write_msg . (,) 0) msgs

-- * tests

-- | Test a few things by writing and reading both ends of the same port.
run_tests :: Interface -> WriteMsg -> ReadMsg -> IO ()
run_tests interface write_msg read_msg = do
    putStrLn "---- abort"
    test_abort interface write_msg read_msg
    putStrLn "---- merge"
    test_merge interface write_msg read_msg
    putStrLn "---- sysex"
    test_sysex write_msg read_msg

-- | Ensure that aborts really cancel pending msgs.
test_abort :: Interface -> WriteMsg -> ReadMsg -> IO ()
test_abort interface write_msg read_msg = do
    now <- Interface.now interface
    let msgs = [note_on 10, note_on 20, chan_msg (Midi.PitchBend 42)]
    -- Msgs start from now + 0.5 so the abort should cancel all of them.
    mapM_ write_msg [(now + RealTime.seconds (i * 0.5), msg)
        | (i, msg) <- zip [5..] msgs]
    sleep 0.2
    Interface.abort interface
    sleep 1
    msgs <- read_all read_msg
    -- OS X used to have a bug that emitted weird pitchbend after an abort.
    -- It's been fixed but now it emits pb0, presumably because they can't
    -- be totally sure a pitchbend hasn't been emitted when the abort is
    -- received.
    Test.equal (filter (not . is_pb0) msgs) []
    putStrLn "msgs after abort (pitchbend 0 expected on CoreMIDI):"
    Test.pprint msgs
    where
    is_pb0 msg = case Midi.rmsg_msg msg of
        Midi.ChannelMessage _ (Midi.PitchBend 0) -> True
        _ -> False

-- | Ensure that timestamp 0 msgs get merged in ahead of timed msgs, as per
-- 'Interface.write_message'.
test_merge :: Interface -> WriteMsg -> ReadMsg -> IO ()
test_merge interface write_msg read_msg = do
    now <- Interface.now interface
    mapM_ write_msg
        [ (now, note_on 10)
        , (now + RealTime.seconds 0.25, note_on 11)
        , (now + RealTime.seconds 0.5, note_on 12)
        ]
    sleep 0.1
    write_msg (0, note_on 100)
    sleep 1
    msgs <- read_all read_msg
    void $ Test.equal (map Midi.rmsg_msg msgs)
        [note_on 10, note_on 100, note_on 11, note_on 12]

test_sysex :: WriteMsg -> ReadMsg -> IO ()
test_sysex write_msg read_msg = do
    let size = 20
    let msg = Midi.CommonMessage $ Midi.SystemExclusive 42
            (ByteString.pack (take (size*1024) (cycle [0..9]) ++ [0xf7]))
    write_msg (0, msg)
    putStrLn "waiting for sysex to arrive..."
    result <- read_until 10 read_msg
    (out, secs) <- case result of
        Nothing -> errorIO "no sysex arrived!"
        Just val -> return val
    putStrLn $ show secs ++ " seconds for " ++ show size ++ " bytes"
    let out_msg = Midi.rmsg_msg out
    void $ if out_msg == msg
        then Test.success "sysex equal"
        else Test.failure $ "got sysex: " <> showt out_msg


-- * util

chan_msg = Midi.ChannelMessage 0
note_on nn = chan_msg (Midi.NoteOn nn 70)
note_off nn = chan_msg (Midi.NoteOff nn 70)

sleep :: Double -> IO ()
sleep = Concurrent.threadDelay . floor . (*1000000)

read_until :: Time.NominalDiffTime -> ReadMsg
    -> IO (Maybe (Midi.ReadMessage, Time.NominalDiffTime))
read_until timeout read_msg = do
    started <- Time.getCurrentTime
    let abort_at = timeout `Time.addUTCTime` started
    let go = do
            msg <- read_msg
            now <- Time.getCurrentTime
            case msg of
                Just m -> return (Just (m, now `Time.diffUTCTime` started))
                Nothing -> do
                    if now > abort_at
                        then return Nothing
                        else sleep 0.25 >> go
    go

read_all :: ReadMsg -> IO [Midi.ReadMessage]
read_all read_msg = do
    msg <- read_msg
    case msg of
        Just m -> do
            rest <- read_all read_msg
            return (m:rest)
        Nothing -> return []
