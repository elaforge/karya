-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Standalone driver for tscore.
module Derive.TScore.TScoreMain (main) where
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception
import qualified Control.Monad.Except as Except

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Tuple as Tuple
import qualified Data.Vector as Vector

import qualified GHC.IO.Encoding as Encoding
import qualified System.Console.GetOpt as GetOpt
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO

import qualified Util.Audio.PortAudio as PortAudio
import qualified Util.Lists as Lists
import qualified Util.Log as Log
import qualified Util.Maps as Maps
import qualified Util.Pretty as Pretty
import qualified Util.Texts as Texts
import qualified Util.Thread as Thread

import qualified App.Config as App.Config
import qualified App.Path as Path
import qualified App.StaticConfig as StaticConfig

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Ky as Ky
import qualified Cmd.Performance as Performance
import qualified Cmd.SaveGitT as SaveGitT
import qualified Cmd.Simple as Simple

import qualified Derive.DeriveSaved as DeriveSaved
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.TScore.TScore as TScore

import qualified Instrument.Inst as Inst
import qualified Local.Config
import qualified Midi.Interface as Interface
import qualified Midi.Midi as Midi
import qualified Midi.MidiDriver as MidiDriver

import qualified Perform.Im.Convert as Im.Convert
import qualified Perform.Midi.Play as Midi.Play
import qualified Perform.Sc.Note as Sc.Note
import qualified Perform.Sc.Patch as Sc.Patch
import qualified Perform.Sc.Play as Sc.Play
import qualified Perform.Transport as Transport

import qualified Synth.Shared.Note as Shared.Note
import qualified Synth.StreamAudio as StreamAudio
import qualified Ui.Transform as Transform
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig

import           Global
import           Types


-- TODO this compiles Cmd.GlobalKeymap, why?
-- SyncKeycaps -> User.Elaforge.Config -> Local.Config -> DeriveSaved
--
-- So I must either split config into interactive and non-interactive, or have
-- some hack to open keycaps without directly calling SyncKeycaps.

-- * main

main :: IO ()
main = do
    Encoding.setLocaleEncoding Encoding.utf8
    Log.configure $ \state -> state { Log.statePriority = Log.Notice }
    (flags, args, errors) <- GetOpt.getOpt GetOpt.Permute options <$>
        Environment.getArgs
    unless (null errors) $ usage errors
    if  | Check `elem` flags -> mapM_ checkScore args
        | Dump `elem` flags -> mapM_ dumpScore args
        | List `elem` flags -> listDevices
        | otherwise -> case args of
            [fname] -> playScore (Lists.last [d | Device d <- flags]) fname
            _ -> usage []
    where
    usage errors = die $ Text.stripEnd $ Text.unlines $ map txt errors ++
        [ "usage: tscore [ flags ] input.tscore"
        , txt $ dropWhile (=='\n') $ GetOpt.usageInfo "" options
        ]

data Flag = Check | Dump | List | Device String
    deriving (Eq, Show)

options :: [GetOpt.OptDescr Flag]
options =
    [ GetOpt.Option [] ["check"] (GetOpt.NoArg Check) "check score only"
    , GetOpt.Option [] ["device"] (GetOpt.ReqArg Device "dev")
        "use named device"
    , GetOpt.Option [] ["dump"] (GetOpt.NoArg Dump) "dump score"
    , GetOpt.Option [] ["list"] (GetOpt.NoArg List) "list output devices"
    ]

die :: Text -> IO a
die msg = do
    Text.IO.hPutStrLn IO.stderr msg
    Exit.exitFailure

getDevice :: String -> IO StreamAudio.Device
getDevice name = do
    (devs, _) <- StreamAudio.getDevices
    maybe (errorIO $ "unknown device: " <> showt name) (pure . snd) $
        List.find ((==name) . fst) devs

initializeMidi :: (Interface.Interface -> IO a) -> IO a
initializeMidi app = MidiDriver.initialize "tscore" (const False) $ \case
    Left err -> die $ "error initializing midi: " <> err
    Right midiInterface -> app =<< Interface.trackInterface midiInterface

checkScore :: FilePath -> IO ()
checkScore fname = do
    source <- Text.IO.readFile fname
    cmdConfig <- DeriveSaved.loadCmdConfig
    case TScore.parseScore cmdConfig source of
        Left err -> Text.IO.putStrLn $ txt fname <> ": " <> err
        Right uiState -> Text.IO.putStr $ Transform.showStats uiState

listDevices :: IO ()
listDevices = PortAudio.initialize $ initializeMidi $ \midiInterface -> do
    putStrLn "Audio devices:"
    (devs, defaultDev) <- StreamAudio.getDevices
    forM_ (map fst devs) $ \dev ->
        putStrLn $ (if dev == defaultDev then "* " else "  ") <> show dev
    putStrLn "Midi devices:"
    wdevs <- Interface.writeDevices midiInterface
    staticConfig <- Local.Config.loadStaticConfig
    printMidiDevices wdevs
        (StaticConfig.wdevMap (StaticConfig.midi staticConfig))

dumpScore :: FilePath -> IO ()
dumpScore fname = do
    source <- Text.IO.readFile fname
    cmdConfig <- DeriveSaved.loadCmdConfig
    (uiState, cmdState) <- either die pure =<< loadScore cmdConfig source
    dump <- either (die . pretty) pure $ Ui.eval uiState Simple.dumpState
    Pretty.pprint dump

    blockId <- maybe (die "no root block") pure $
        Ui.config#UiConfig.root #$ uiState
    let (events, logs) = derive uiState cmdState blockId
    mapM_ Log.write logs
    mapM_ (Text.IO.putStrLn . Score.shortEvent) events
    putStrLn "\n\tscore events:"
    mapM_ Log.write logs
    mapM_ (Text.IO.putStrLn . Score.shortEvent) events

    ((imNotes, logs), events) <-
        pure $ convertIm uiState cmdState blockId events
    unless (null imNotes && null logs) $ do
        putStrLn "\n\tim events:"
        mapM_ Log.write logs
        mapM_ Pretty.pprint imNotes

    let ((midiMsgs, scMsgs), logs) =
            DeriveSaved.perform cmdState uiState events
    mapM_ Log.write logs
    unless (null midiMsgs) $ do
        putStrLn "\n\tmidi msgs:"
        mapM_ Pretty.pprint midiMsgs
    unless (null scMsgs) $ do
        putStrLn "\n\tsc msgs:"
        mapM_ Pretty.pprint scMsgs

convertIm :: Ui.State -> Cmd.State -> BlockId
    -> Vector.Vector Score.Event
    -> (([Shared.Note.Note], [Log.Msg]), Vector.Vector Score.Event)
convertIm uiState cmdState blockId events =
    ( LEvent.partition $
        Im.Convert.convert blockId lookupInst $ Vector.toList imEvents
    , restEvents
    )
    where
    (imEvents, restEvents) = Vector.partition isImEvent events
    isImEvent =
        maybe False (isIm . Cmd.instInstrument) . lookupInst
            . Score.eventInstrument
    isIm (Inst.Inst (Inst.Im {}) _) = True
    isIm _ = False
    lookupInst = either (const Nothing) Just
        . Cmd.stateLookupInstrument uiState cmdState

playScore :: Maybe String -> FilePath -> IO ()
playScore mbDevice fname = PortAudio.initialize $ initializeMidi $
        \midiInterface -> do
    source <- Text.IO.readFile fname
    cmdConfig <- loadCmdConfig midiInterface
    (uiState, cmdState) <- either die pure =<< loadScore cmdConfig source
    initializeInstruments uiState cmdState

    blockId <- maybe (die "no root block") pure $
        Ui.config#UiConfig.root #$ uiState
    let (imEvents, logs) = derive uiState cmdState blockId
    mapM_ Log.write logs

    let start = 0 -- TODO from cmdline

    let scorePath = fname
    (procs, events) <- performIm scorePath cmdState uiState imEvents
        blockId
    playCtl <- Transport.playControl
    players <- Transport.activePlayers
    unless (null procs) $ do
        mbDevice <- traverse getDevice mbDevice
        playIm mbDevice scorePath players playCtl blockId start imEvents
            (getImInstruments uiState) procs

    let ((midiMsgs, scMsgs), logs) =
            DeriveSaved.perform cmdState uiState events
    mapM_ Log.write logs
    unless (null scMsgs) $ Sc.Play.play
        (Sc.Play.State { _playControl = playCtl, _players = players })
        (Sc.Note.PlayNotes { shift = 0, stretch = 1, notes = scMsgs })
        Nothing

    -- TODO since I have to wait for im, I may have to bump MIDI forward.
    -- Of course they won't be very in sync anyway...
    unless (null midiMsgs) $
        playMidi playCtl players midiInterface cmdState uiState midiMsgs

    Thread.startLogged "kbd" $ do
        putStrLn "press return to stop player"
        _ <- IO.getLine
        Transport.stopPlayer playCtl
    putStrLn "waiting for players to complete..."
    Transport.waitPlayerStopped players
    Thread.delay 0.1 -- let threads print final logs before exiting
    putStrLn "done"

initializeInstruments :: Ui.State -> Cmd.State -> IO ()
initializeInstruments uiState cmdState = do
    (result, logs) <- Cmd.eval uiState cmdState scInitialize
    case result of
        Left err -> Log.warn $ txt err
        Right () -> pure ()
    mapM_ Log.write logs

-- | Ask scsynth to load the patches.  TODO: if this gets slow, is there some
-- way to detect if the server is up to date and skip this?
scInitialize :: Cmd.CmdT IO ()
scInitialize = do
    insts <- Ui.getConfig $ Map.keys . UiConfig.unallocations
        . UiConfig.configAllocations
    patches <- mapMaybe Cmd.scPatch <$> mapM Cmd.getInstrument insts
    unless (null patches) $ liftIO $ do
        Sc.Play.addDefaultGroup
        scInitializePatches patches

scInitializePatches :: [Sc.Patch.Patch] -> IO ()
scInitializePatches patches = Sc.Play.version >>= \case
    Left err -> errorIO $ "can't initialize sc patches: " <> err
    Right msg -> do
        Text.IO.putStrLn $ "found scsynth: " <> msg
        Text.IO.putStrLn $ "loading patches: "
            <> Text.unwords (map (Texts.toText . Sc.Patch.name) patches)
        mapM_ Sc.Play.initializePatch patches
        ((), dur) <- Thread.timeActionText $ Sc.Play.sync
        Text.IO.putStrLn $ "sync took: " <> dur

-- * midi

playMidi :: Transport.PlayControl -> Transport.ActivePlayers
    -> Interface.Interface -> Cmd.State -> Ui.State
    -> [LEvent.LEvent Midi.WriteMessage] -> IO ()
playMidi playCtl players midiInterface cmdState uiState midiMsgs = do
    wdevs <- Interface.writeDevices midiInterface
    mapM_ (Interface.connectWriteDevice midiInterface) (map fst wdevs)
    mvar <- MVar.newMVar uiState
    Midi.Play.play (midiState mvar) Nothing "tscore" midiMsgs Nothing
    where
    midiState mvar = Midi.Play.State
        { _playControl = playCtl
        , _players = players
        , _info = transportInfo mvar
        , _imEnd = Nothing
        }
    transportInfo mvar = Transport.Info
        { infoSendStatus = \status -> print status -- TODO
        , infoMidiWriter = Cmd.stateMidiWriter cmdState
        , infoMidiAbort = Interface.abort midiInterface
        , infoGetCurrentTime = Interface.now midiInterface
        -- This is unused by midi player, but Midi.Play wants it anyway
        , infoState = mvar
        }

printMidiDevices :: [(Midi.WriteDevice, [Midi.WriteDevice])]
    -> Map Midi.WriteDevice Midi.WriteDevice -> IO ()
printMidiDevices wdevs wdevMap =
    forM_ wdevs $ \(wdev, aliases) -> Text.IO.putStrLn $ Text.unwords $
        [ "  " <> pretty wdev
        , if null aliases then "" else pretty aliases
        , maybe "" (("<- "<>) . Text.intercalate ", " . map pretty) $
            Map.lookup wdev wdevToNames
        ]
    where wdevToNames = Maps.multimap $ map Tuple.swap $ Map.toList wdevMap

-- * derive

derive :: Ui.State -> Cmd.State -> BlockId
    -> (Vector.Vector Score.Event, [Log.Msg])
derive uiState cmdState blockId = (Cmd.perfEvents perf, warns ++ logs)
    where
    (perf, logs) = Performance.derive uiState cmdState blockId
    warns = filter ((>=Log.Warn) . Log.msgPriority) (Cmd.perfLogs perf)

-- Derived from Solkattu.Play.derive_to_disk.
performIm :: FilePath -> Cmd.State -> Ui.State -> Vector.Vector Score.Event
    -> BlockId -> IO ([Performance.Process], Vector.Vector Score.Event)
performIm scorePath cmdState uiState events blockId = do
    let imConfig = Cmd.configIm (Cmd.stateConfig cmdState)
        lookupInst = either (const Nothing) Just
            . Cmd.stateLookupInstrument uiState cmdState
    (procs, nonIm) <- Performance.evaluateIm imConfig lookupInst scorePath
        0 1 blockId events
    pure (procs, nonIm)

playIm :: Maybe StreamAudio.Device -> FilePath -> Transport.ActivePlayers
    -> Transport.PlayControl -> BlockId -> RealTime
    -> Vector.Vector Score.Event -> Set ScoreT.Instrument
    -> [Performance.Process] -> IO ()
playIm mbDevice scorePath players playCtl blockId start events
        imInstruments procs = do
    putStrLn $ "\nim render:"
    forM_ procs $ \(cmd, args) ->
        putStrLn $ unwords $ "%" : cmd : args
    ready <- MVar.newEmptyMVar
    Thread.startLogged "play_im" $ do
        ok <- Performance.waitForSubprocesses
            (MVar.putMVar ready ())
            expectedInstruments
            (Set.fromList procs)
        unless ok $ Log.warn "background render had a problem"
    Log.debug $ "wait for instruments: " <> pretty expectedInstruments
    MVar.takeMVar ready
    let Transport.PlayControl quit = playCtl
    let muted = mempty
    Transport.playerStarted players
    Thread.startLogged "stream_audio" $
        StreamAudio.play False mbDevice quit scorePath blockId muted start
            `Exception.finally` Transport.playerStopped players
    pure ()
    where
    -- This is a bit too tricky.  I want to make sure all instruments have
    -- the chunk at 'start' rendered.  But I can only wait for im instruments,
    -- or I'll be waiting forever, and then only the ones that are actually
    -- used.  I could filter on start, but since im emits empty chunks for
    -- instruments that haven't started, it should be harmless to wait for them
    -- all.
    expectedInstruments = Vector.foldl' add mempty $
        Vector.filter ((`Set.member` imInstruments) . Score.eventInstrument)
        events
    add = flip $ Set.insert . Score.eventInstrument

getImInstruments :: Ui.State -> Set ScoreT.Instrument
getImInstruments = Set.fromList . map fst
    . filter (UiConfig.isImAllocation . snd) . Map.toList
    . (Ui.config#UiConfig.allocationsMap #$)

-- * load

type Error = Text

loadCmdConfig :: Interface.Interface -> IO Cmd.Config
loadCmdConfig midiInterface = do
    staticConfig <- Local.Config.loadStaticConfig
    appDir <- Path.getAppDir
    saveDir <- Path.canonical $ Path.toAbsolute appDir App.Config.saveDir
    pure $ StaticConfig.cmdConfig appDir saveDir midiInterface
        staticConfig (SaveGitT.User "user" "name")

loadScore :: Cmd.Config -> Text -> IO (Either Error (Ui.State, Cmd.State))
loadScore cmdConfig source = Except.runExceptT $ do
    uiState <- tryRight $ TScore.parseScore cmdConfig source
    -- TODO adjust starting line in error
    (builtins, aliases, allocs) <- tryRight . first ("parsing %ky: "<>)
        =<< liftIO (Ky.load kyPaths (Ui.config#UiConfig.ky #$ uiState))
    let cmdState = DeriveSaved.addLibrary builtins aliases $
            Cmd.initialState cmdConfig
    pure (Ui.config#UiConfig.allocations #= allocs $ uiState, cmdState)
    where
    -- For now, I don't support ky import.
    kyPaths = []
