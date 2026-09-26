-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Test MIDI bindings, automatically and manually.
module Midi.TestMidi where
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM as STM
import qualified Data.ByteString as ByteString
import qualified Data.Text.IO as Text.IO
import qualified Data.Time as Time

import qualified Numeric
import qualified System.Environment
import qualified System.IO as IO

import qualified Util.Lists as Lists
import qualified Util.Num as Num
import qualified Util.Test as Test

import qualified Midi.Encode
import qualified Midi.Interface as Interface
import qualified Midi.Midi as Midi
import qualified Midi.MidiDriver as MidiDriver
import qualified Midi.Mmc as Mmc

import qualified Perform.RealTime as RealTime

import           Global


type Interface = Interface.RawInterface Midi.WriteMessage

main :: IO ()
main = MidiDriver.initialize "test_midi" wantMessage test_midi
    where
    wantMessage =
        (/= Midi.Encode.encode (Midi.RealtimeMessage Midi.ActiveSense))

type ReadMsg = IO (Maybe Midi.ReadMessage)
type WriteMsg = (RealTime.RealTime, Midi.Message) -> IO ()

test_midi :: Either Text Interface -> IO ()
test_midi (Left err) = errorIO $ "initializing midi: " <> err
test_midi (Right interface) = do
    rdevs <- Interface.readDevices interface
    putStrLn "read devs:"
    mapM_ (Text.IO.putStrLn . ("    " <>) . pretty) rdevs
    wdevs <- Interface.writeDevices interface
    putStrLn "write devs:"
    mapM_ (Text.IO.putStrLn . ("    " <>) . pretty) wdevs
    rdevs <- pure $ map fst rdevs

    let open = openDevices interface
    args <- System.Environment.getArgs
    case args of
        [] -> pure ()
        ["monitor"] -> do
            putStrLn "monitoring (pass arg 'help' for help)"
            (_, readMsg) <- open True rdevs Nothing
            monitor readMsg
        ["record-sysex"] -> do
            (_, readMsg) <- open True rdevs Nothing
            recordSysex readMsg
        ["send-sysex", outDev, fname] -> do
            (writeMsg, _) <- open True [] (Just outDev)
            sendSysex writeMsg fname
        ["help"] -> putStrLn usage
        ["melody", outDev] -> do
            putStrLn "playing melody"
            (writeMsg, _) <- open True [] (Just outDev)
            melody interface writeMsg
            putStrLn "return to quit... "
            void getLine
        ["melody-thru", outDev] -> do
            putStrLn "playing melody + thru"
            (writeMsg, readMsg) <- open True rdevs (Just outDev)
            thruMelody interface writeMsg readMsg
        ["mmc", msg, outDev] -> do
            (writeMsg, _) <- open True [] (Just outDev)
            mmc writeMsg msg
        ("monitor" : mdevs@(_:_)) -> do
            putStrLn $ "monitoring: " ++ Lists.join ", " mdevs
            (_, readMsg) <-
                open True (map (Midi.readDevice . txt) mdevs) Nothing
            monitor readMsg
        ["spam", outDev, nStr] -> do
            putStrLn $ "spamming " ++ nStr ++ " msgs"
            (writeMsg, _) <- open True rdevs (Just outDev)
            n <- readIO nStr
            spam interface writeMsg n
            getChar
            pure ()
        ["pb-range", outDev, semis] -> do
            semis <- readIO semis
            (writeMsg, _) <- open True rdevs (Just outDev)
            pitchBendRange writeMsg semis
        ["program", outDev] ->
            uncurry programChange =<< open False [] (Just outDev)
        ["test", loopback] -> do
            (writeMsg, readMsg) <- open False
                [Midi.readDevice (txt loopback)] (Just loopback)
            runTests interface writeMsg readMsg
        ["thru", outDev] -> do
            putStrLn "playing thru"
            (writeMsg, readMsg) <- open True rdevs (Just outDev)
            thruLoop writeMsg readMsg
        _ -> do
            putStrLn "unknown command"
            putStrLn usage

openDevices :: Interface -> Bool -> [Midi.ReadDevice]
    -> Maybe String -> IO (WriteMsg, ReadMsg)
openDevices interface blocking rdevs maybeWdev = do
    oks <- mapM (Interface.connectReadDevice interface) rdevs
    forM_ [rdev | (rdev, False) <- zip rdevs oks] $ \missing ->
        putStrLn $ "rdev not found: " ++ show missing
    let readMsg = (if blocking then blockingGet else nonblockingGet)
            (Interface.readChannel interface)
    case Midi.writeDevice . txt <$> maybeWdev of
        Nothing -> pure
            (const (error "write device not opened"), readMsg)
        Just wdev -> do
            ok <- Interface.connectWriteDevice interface wdev
            unless ok $
                error $ "required wdev " ++ show wdev ++ " not found"
            pure (makeWriteMsg interface wdev, readMsg)
    where
    makeWriteMsg interface wdev (ts, msg) = do
        putStrLn $ "write: " <> show ts <> ": " <> show msg
        mbErr <- Interface.writeMessage interface
            (Midi.WriteMessage wdev ts msg)
        whenJust mbErr $ Text.IO.putStrLn . ("error: "<>)

nonblockingGet :: Interface.ReadChan -> ReadMsg
nonblockingGet readChan = STM.atomically $
    fmap Just (STM.readTChan readChan) `STM.orElse` pure Nothing

blockingGet :: Interface.ReadChan -> ReadMsg
blockingGet readChan = STM.atomically $ fmap Just (STM.readTChan readChan)

usage :: String
usage =
    "(no arg)             print devs and exit\n\
    \monitor              monitor all inputs\n\
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

programChange :: WriteMsg -> ReadMsg -> IO ()
programChange writeMessage _readMessage = forever $ do
    putStr "> "
    IO.hFlush IO.stdout
    line <- getLine
    let msg = Midi.ChannelMessage 0 $ Midi.ProgramChange (read line)
    print msg
    writeMessage (0, msg)

-- * sysex

recordSysex :: ReadMsg -> IO ()
recordSysex readMsg = loop 0
    where
    loop n = do
        Just (Midi.ReadMessage _ _ msg) <- readMsg
        wrote <- case msg of
            Midi.CommonMessage (Midi.SystemExclusive manuf bytes) -> do
                let fn = "record-sysex" ++ show n ++ ".syx"
                putStrLn $ "sysex " ++ untxt (Num.hex 2 manuf) ++ " "
                    ++ show (ByteString.length bytes) ++ " bytes -> "
                    ++ fn
                ByteString.writeFile fn (Midi.Encode.encode msg)
                pure True
            _ -> pure False
        loop (if wrote then n+1 else n)

sendSysex :: WriteMsg -> FilePath -> IO ()
sendSysex writeMsg fname = do
    msg <- Midi.Encode.decode <$> ByteString.readFile fname
    writeMsg (0, msg)
    putStrLn "sending asynchronously, hit return when the blinkenlights stop"
    _ <- getLine
    pure ()

-- * monitor

monitor :: ReadMsg -> IO ()
monitor readMsg = forever $ do
    Just (Midi.ReadMessage dev ts msg) <- readMsg
    print (ts, dev, msg)


-- * thru

thruLoop :: WriteMsg -> ReadMsg -> IO ()
thruLoop writeMsg readMsg = forever $ do
    Just (Midi.ReadMessage dev ts msg) <- readMsg
    putStrLn $ "thru: " ++ show (ts, dev, msg)
    writeMsg (0, msg)


-- * melody

-- | Play a melody while also allowing msgs thru, to test merging.
thruMelody :: Interface -> WriteMsg -> ReadMsg -> IO ()
thruMelody interface writeMsg readMsg = do
    melody interface writeMsg
    thruLoop writeMsg readMsg

melody :: Interface -> WriteMsg -> IO ()
melody interface writeMsg = do
    now <- Interface.now interface
    mapM_ writeMsg (notes now)

-- | Write notes over time.
notes :: RealTime.RealTime -> [(RealTime.RealTime, Midi.Message)]
notes startTs = concat
    [[(ts, noteOn nn), (ts + RealTime.seconds 0.4, noteOff nn)]
        | (ts, nn) <- zip (Lists.range_ startTs (RealTime.seconds 0.5)) score]
    where score = [53, 55 .. 61]

-- * mmc

mmc :: WriteMsg -> String -> IO ()
mmc writeMsg msg = writeMsg (0, Mmc.encode 127 mmcMsg)
    where
    mmcMsg
        | Just smpte <- parseSmpte msg = Mmc.Goto smpte 0
        | msg == "play" = Mmc.Play
        | msg == "stop" = Mmc.Stop
        | otherwise = error $ "unknown msg: " ++ msg

parseSmpte :: String -> Maybe Midi.Smpte
parseSmpte txt = do
    [h, m, s] <- Just $ Lists.split (==':') txt
    Midi.Smpte <$> int h <*> int m <*> int s <*> pure 0
    where
    int s = case Numeric.readDec s of
        (n, "") : _ -> Just n
        _ -> Nothing


-- * spam

spam :: Interface -> WriteMsg -> Int -> IO ()
spam interface writeMsg n = do
    now <- Interface.now interface
    mapM_ writeMsg [(now + RealTime.milliseconds (i*10), msg)
        | (i, msg) <- zip [0..] msgs]
    where
    msgs = take n [Midi.ChannelMessage 0 msg | nn <- cycle [0..127],
        msg <- [Midi.NoteOn nn 127, Midi.NoteOff nn 127]]

-- * pitch bend range

pitchBendRange :: WriteMsg -> Double -> IO ()
pitchBendRange writeMsg semis = do
    let msgs = map (Midi.ChannelMessage 0) (Midi.pitchBendRange semis)
    Test.pprint msgs
    mapM_ (writeMsg . (,) 0) msgs

-- * tests

-- | Test a few things by writing and reading both ends of the same port.
runTests :: Interface -> WriteMsg -> ReadMsg -> IO ()
runTests interface writeMsg readMsg = do
    putStrLn "---- abort"
    test_abort interface writeMsg readMsg
    putStrLn "---- merge"
    test_merge interface writeMsg readMsg
    putStrLn "---- sysex"
    test_sysex writeMsg readMsg

-- | Ensure that aborts really cancel pending msgs.
test_abort :: Interface -> WriteMsg -> ReadMsg -> IO ()
test_abort interface writeMsg readMsg = do
    now <- Interface.now interface
    let msgs = [noteOn 10, noteOn 20, chanMsg (Midi.PitchBend 42)]
    -- Msgs start from now + 0.5 so the abort should cancel all of them.
    mapM_ writeMsg [(now + RealTime.seconds (i * 0.5), msg)
        | (i, msg) <- zip [5..] msgs]
    sleep 0.2
    Interface.abort interface
    sleep 1
    msgs <- readAll readMsg
    -- OS X used to have a bug that emitted weird pitchbend after an abort.
    -- It's been fixed but now it emits pb0, presumably because they can't
    -- be totally sure a pitchbend hasn't been emitted when the abort is
    -- received.
    Test.equal (filter (not . isPb0) msgs) []
    putStrLn "msgs after abort (pitchbend 0 expected on CoreMIDI):"
    Test.pprint msgs
    where
    isPb0 msg = case Midi.rmsgMsg msg of
        Midi.ChannelMessage _ (Midi.PitchBend 0) -> True
        _ -> False

-- | Ensure that timestamp 0 msgs get merged in ahead of timed msgs, as per
-- 'Interface.write_message'.
test_merge :: Interface -> WriteMsg -> ReadMsg -> IO ()
test_merge interface writeMsg readMsg = do
    now <- Interface.now interface
    mapM_ writeMsg
        [ (now, noteOn 10)
        , (now + RealTime.seconds 0.25, noteOn 11)
        , (now + RealTime.seconds 0.5, noteOn 12)
        ]
    sleep 0.1
    writeMsg (0, noteOn 100)
    sleep 1
    msgs <- readAll readMsg
    void $ Test.equal (map Midi.rmsgMsg msgs)
        [noteOn 10, noteOn 100, noteOn 11, noteOn 12]

test_sysex :: WriteMsg -> ReadMsg -> IO ()
test_sysex writeMsg readMsg = do
    let size = 20
    let msg = Midi.CommonMessage $ Midi.SystemExclusive 42
            (ByteString.pack (take (size*1024) (cycle [0..9]) ++ [0xf7]))
    writeMsg (0, msg)
    putStrLn "waiting for sysex to arrive..."
    result <- readUntil 10 readMsg
    (out, secs) <- case result of
        Nothing -> errorIO "no sysex arrived!"
        Just val -> pure val
    putStrLn $ show secs ++ " seconds for " ++ show size ++ " bytes"
    let outMsg = Midi.rmsgMsg out
    void $ if outMsg == msg
        then Test.success "sysex equal"
        else Test.failure $ "got sysex: " <> showt outMsg


-- * util

chanMsg = Midi.ChannelMessage 0
noteOn nn = chanMsg (Midi.NoteOn nn 70)
noteOff nn = chanMsg (Midi.NoteOff nn 70)

sleep :: Double -> IO ()
sleep = Concurrent.threadDelay . floor . (*1000000)

readUntil :: Time.NominalDiffTime -> ReadMsg
    -> IO (Maybe (Midi.ReadMessage, Time.NominalDiffTime))
readUntil timeout readMsg = do
    started <- Time.getCurrentTime
    let abortAt = timeout `Time.addUTCTime` started
    let go = do
            msg <- readMsg
            now <- Time.getCurrentTime
            case msg of
                Just m -> pure (Just (m, now `Time.diffUTCTime` started))
                Nothing -> do
                    if now > abortAt
                        then pure Nothing
                        else sleep 0.25 >> go
    go

readAll :: ReadMsg -> IO [Midi.ReadMessage]
readAll readMsg = do
    msg <- readMsg
    case msg of
        Just m -> do
            rest <- readAll readMsg
            pure (m:rest)
        Nothing -> pure []
