-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities for cmd tests.
module Cmd.CmdTest where
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.Chan as Chan
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import qualified System.IO.Unsafe as Unsafe

import qualified Util.Debug as Debug
import qualified Util.Log as Log
import qualified App.Config as Config
import qualified Cmd.Cmd as Cmd
import qualified Cmd.InputNote as InputNote
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Cmd.Msg as Msg
import qualified Cmd.Perf as Perf
import qualified Cmd.Performance as Performance
import           Cmd.TestInstances ()

import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Expr as Expr
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.Stream as Stream

import qualified Midi.Interface as Interface
import qualified Midi.Midi as Midi
import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import qualified Ui.Diff as Diff
import qualified Ui.Key as Key
import qualified Ui.Sel as Sel
import qualified Ui.Types as Types
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig
import qualified Ui.UiMsg as UiMsg
import qualified Ui.UiTest as UiTest
import qualified Ui.Update as Update

import           Global
import           Types


-- * running cmds

data Result val = Result
    -- | A Nothing val means it aborted.
    { resultVal :: Either Text (Maybe val)
    , resultCmdState :: Cmd.State
    , resultUiState :: Ui.State
    , resultUiDamage :: Update.UiDamage
    , resultLogs :: [Log.Msg]
    , resultThru :: [Cmd.Thru]
    }

resultFailed :: Result a -> Maybe Text
resultFailed = either Just (const Nothing) . resultOk

resultOk :: Result a -> Either Text a
resultOk res = case resultVal res of
    Right (Just val) -> Right val
    Right Nothing -> Left "aborted"
    Left err -> Left err

-- | Run cmd with the given tracks.
runTracks :: [UiTest.TrackSpec] -> Cmd.CmdId a -> Result a
runTracks tracks = run (makeTracks tracks) defaultCmdState

runBlocks :: [UiTest.BlockSpec] -> Cmd.CmdId a -> Result a
runBlocks blocks = run (snd $ UiTest.run Ui.empty $ UiTest.mkblocks blocks)
    defaultCmdState

-- | Like 'run_tracks', but the ruler only extends to the end of the last
-- event.
runTracksRuler :: [UiTest.TrackSpec] -> Cmd.CmdId a -> Result a
runTracksRuler tracks = run (makeTracksRuler tracks) defaultCmdState

-- | Derive the tracks and then run the cmd with the performance available.
runTracksWithPerformance :: [UiTest.TrackSpec] -> Cmd.CmdT IO a
    -> IO (Result a)
runTracksWithPerformance tracks =
    runWithPerformance (makeTracks tracks) defaultCmdState

runWithPerformance :: Ui.State -> Cmd.State -> Cmd.CmdT IO a
    -> IO (Result a)
runWithPerformance ustate cstate cmd = do
    cstate <- updatePerformance Ui.empty ustate cstate mempty
    runIo ustate cstate cmd

makeTracks :: [UiTest.TrackSpec] -> Ui.State
makeTracks = snd . UiTest.runMkview

makeTracksRuler :: [UiTest.TrackSpec] -> Ui.State
makeTracksRuler = snd . make
    where
    make tracks = UiTest.run Ui.empty $
        UiTest.mkblockView (UiTest.defaultBlockName <> "=ruler", tracks)

-- | Run a cmd and return everything you could possibly be interested in.
run :: Ui.State -> Cmd.State -> Cmd.CmdId a -> Result a
run ustate1 cstate1 cmd = Result
    { resultVal = val
    , resultCmdState = cstate2
    , resultUiState = ustate2
    , resultUiDamage = update
    , resultLogs = logs
    , resultThru = midiMsgs
    }
    where
    (cstate2, midiMsgs, logs, result) = Cmd.runId ustate1 cstate1 cmd
    (val, ustate2, update) = case result of
        Left err -> (Left (pretty err), ustate1, mempty)
        Right (v, ustate2, update) -> (Right v, ustate2, update)

runIo :: Ui.State -> Cmd.State -> Cmd.CmdT IO a -> IO (Result a)
runIo ustate1 cstate1 cmd = do
    (cstate2, thru, logs, result) <-
        Cmd.run Nothing ustate1 cstate1 (Just <$> cmd)
    let (val, ustate2, update) = case result of
            Left err -> (Left (pretty err), ustate1, mempty)
            Right (v, ustate2, update) -> (Right v, ustate2, update)
    pure $ Result val cstate2 ustate2 update logs thru

runUi :: Ui.State -> Cmd.CmdId a -> Result a
runUi ustate = run ustate defaultCmdState

runUiIo :: Ui.State -> Cmd.CmdT IO a -> IO (Result a)
runUiIo ustate = runIo ustate defaultCmdState

-- | Run a Cmd and return just the value.
eval :: Ui.State -> Cmd.State -> Cmd.CmdId a -> a
eval ustate cstate cmd = case resultVal (run ustate cstate cmd) of
    Left err -> error $ "eval got StateError: " <> show err
    Right Nothing -> error "eval: cmd aborted"
    Right (Just val) -> val

-- | Like 'run', but with a selection on the given track at 0 and note
-- duration set to what will be a ScoreTime 1 with the ruler supplied by
-- UiTest.
runSel :: TrackNum -> [UiTest.TrackSpec] -> Cmd.CmdId a -> Result a
runSel tracknum trackSpecs cmd = runTracks trackSpecs $ do
    -- Add one because UiTest inserts a rule at track 0.
    setSel (tracknum+1) 0 (tracknum+1) 0
    cmd

runAgain :: Result a -> Cmd.CmdId b -> Result b
runAgain res = run (resultUiState res) (resultCmdState res)

-- | Update performances after running a Cmd and getting its Result.
updatePerf :: Ui.State -> Result val -> IO (Result val)
updatePerf uiFrom res = do
    cmdState <- updatePerformance uiFrom (resultUiState res)
        (resultCmdState res) (resultUiDamage res)
    pure $ res { resultCmdState = cmdState }

-- | Run a DeriveTest extractor on a CmdTest Result.
extractDerive :: (Score.Event -> a) -> Result x -> ([a], [Text])
extractDerive ex = DeriveTest.extract ex . extractDeriveResult

-- | Reconstruct a Derive.Result from the root performance, or throw an
-- exception if there is a problem getting it.
extractDeriveResult :: Result a -> Derive.Result
extractDeriveResult res =
    maybe (eval (resultUiState res) (resultCmdState res) mkres)
        (error . (msg<>) . untxt) (resultFailed res)
    where
    msg = "extract_derive_result: cmd failed so result is probably not right: "
    mkres = do
        Cmd.Performance
            { perfDeriveCache, perfEvents, perfLogs
            , perfTrackDynamic, perfIntegrated, perfWarps
            , perfTrackSignals
            } <- Perf.getRoot
        let stream = Stream.mergeLogs perfLogs $
                Stream.fromSortedEvents (Vector.toList perfEvents)
        pure $ Derive.Result
            { rEvents = stream
            , rCache = perfDeriveCache
            , rTrackWarps = perfWarps
            , rTrackSignals = perfTrackSignals
            , rTrackDynamic = perfTrackDynamic
            , rIntegrated = perfIntegrated
            , rState =
                error "can't fake a Derive.State for an extracted Result"
            }

-- | Update the performances based on the UI state change and updates.  This
-- manually runs that part of the responder, and is needed for tests that rely
-- on the performances.
updatePerformance :: Ui.State -> Ui.State -> Cmd.State
    -> Update.UiDamage -> IO Cmd.State
updatePerformance uiFrom uiTo cmdState uiDamage = do
    let (uiUpdates, _) = Diff.diff uiDamage uiFrom uiTo
    chan <- Chan.newChan
    let damage = Diff.deriveDiff uiFrom uiTo uiDamage uiUpdates
    cstate <- Performance.updatePerformance
        (\bid status -> Chan.writeChan chan (bid, status))
        uiTo (setImmediate cmdState) damage
    -- This will delay until the perform thread has derived enough of the
    -- performance to hand over.
    (blockId, perf) <- readPerf chan
    let insert st = st { Cmd.statePerformance = Map.insert blockId perf
            (Cmd.statePerformance st) }
    pure $ cstate { Cmd.statePlay = insert (Cmd.statePlay cstate) }
    where
    -- Get rid of the derive wait to avoid making tests slow.
    setImmediate state = state
        { Cmd.stateDeriveImmediately = Map.keysSet (Ui.stateBlocks uiTo)
        }
    readPerf chan = do
        (blockId, status) <- Chan.readChan chan
        case status of
            Msg.DeriveComplete perf _ -> pure (blockId, perf)
            _ -> readPerf chan

-- | Run several cmds, threading the state through.  The first cmd that fails
-- aborts the whole operation.
thread :: Ui.State -> Cmd.State -> [Cmd.CmdId a]
    -> Either Text (Ui.State, Cmd.State)
thread ustate cstate cmds = foldl f (Right (ustate, cstate)) cmds
    where
    f (Right (ustate, cstate)) cmd = case run ustate cstate cmd of
        Result (Right _) cstate2 ustate2 _ logs _ ->
            DeriveTest.traceLogs logs $ Right (ustate2, cstate2)
        Result (Left err) _ _ _ _ _ -> Left (showt err)
    f (Left err) _ = Left err

-- | Make some tracks and call 'thread'.
threadTracks :: [UiTest.TrackSpec] -> (Cmd.State -> Cmd.State)
    -> [Cmd.CmdId a] -> Either Text (Ui.State, Cmd.State)
threadTracks tracks modifyCmdState cmds =
    thread ustate (modifyCmdState defaultCmdState) cmds
    where (_, ustate) = UiTest.runMkview tracks

defaultCmdState :: Cmd.State
defaultCmdState = mkCmdState UiTest.defaultDb

mkCmdState :: Cmd.InstrumentDb -> Cmd.State
mkCmdState db = (Cmd.initialState (DeriveTest.cmdConfig db))
    { Cmd.stateFocusedView = Just UiTest.defaultViewId
    , Cmd.stateEdit = defaultEditState
    , Cmd.statePlay = defaultPlayState
    }

defaultPlayState :: Cmd.PlayState
defaultPlayState =
    Cmd.initialPlayState { Cmd.statePlayStep = UiTest.step1 }

defaultEditState :: Cmd.EditState
defaultEditState = Cmd.initialEditState
    { Cmd.stateTimeStep = UiTest.step1
    , Cmd.stateNoteDuration = UiTest.step1
    }

-- ** cmds

-- | This has a hack where if post start_pos and cur_pos are negative, use a
-- negative selection.
setSel :: Cmd.M m => Types.TrackNum -> ScoreTime -> Types.TrackNum
    -> ScoreTime -> m ()
setSel = setSelOn UiTest.defaultViewId

setSelOn :: Cmd.M m => ViewId -> Types.TrackNum -> ScoreTime
    -> Types.TrackNum -> ScoreTime -> m ()
setSelOn viewId startTrack startPos curTrack curPos = do
    Cmd.modify $ \st -> st { Cmd.stateFocusedView = Just viewId }
    Ui.setSelection viewId Config.insertSelnum (Just sel)
    where
    sel = Sel.Selection
        { startTrack = startTrack
        , startPos = abs startPos
        , curTrack = curTrack
        , curPos = abs curPos
        , orientation = if startPos >= 0 && curPos >= 0 then Sel.Positive
            else if startPos <= 0 && curPos <= 0 then Sel.Negative
            else error $ "both start_pos and cur_pos should be + or -: "
                <> show (startPos, curPos)
        }

setPointSel :: Ui.M m => Types.TrackNum -> ScoreTime -> m Cmd.Status
setPointSel tracknum pos = do
    setPointSelBlock UiTest.defaultBlockName tracknum pos
    pure Cmd.Done

-- | Set a point selection on the default view of the given block name.
setPointSelBlock :: Ui.M m => Text -> Types.TrackNum -> ScoreTime -> m ()
setPointSelBlock blockName tracknum pos =
    Ui.setSelection viewId Config.insertSelnum
        (Just (Sel.point tracknum pos Sel.Positive))
    where viewId = UiTest.mkVidName blockName

selectAll :: Cmd.M m => m ()
selectAll = do
    tracks <- Ui.trackCount UiTest.defaultBlockId
    end <- Ui.blockEnd UiTest.defaultBlockId
    setSelOn UiTest.defaultViewId 1 0 (tracks - 1) end


-- * extractors

-- | The output of the 'extract' family of functions:
-- Either error (val, [log])
type Extracted val = Either Text (val, [Text])

-- | Run this on either 'extract' or 'extract_state' when you don't care about
-- the logs.
traceLogs :: Extracted a -> Either Text a
traceLogs res = case res of
    Right (b, logs) -> (if null logs then id else trace logs) (Right b)
    Left a -> Left a
    where
    trace = Debug.traceStr . Text.strip . Text.unlines . ("\tlogged:":)

eLogs :: Result a -> [Text]
eLogs = map DeriveTest.showLog . DeriveTest.traceLowPrio . resultLogs

ePerformance :: BlockId -> Result a -> Maybe Cmd.Performance
ePerformance blockId = Map.lookup blockId . Cmd.statePerformance
    . Cmd.statePlay . resultCmdState

eEvents :: BlockId -> Result a -> [Score.Event]
eEvents blockId = maybe [] (Vector.toList . Cmd.perfEvents)
    . ePerformance blockId

eMidi :: Result a -> [Midi.Message]
eMidi result =
    [ Midi.wmsgMsg msg
    | Cmd.MidiThru (Interface.Midi msg) <- resultThru result
    ]


-- ** val

-- | Extract the value from a cmd.  This is meant to be used as the single
-- check on a cmd operation, so it also returns logs and whether the cmd
-- failed or not (the latter is mandatory since otherwise there is no value).
extract :: (val -> e) -> Result val -> Extracted (Maybe e)
extract f res = case resultVal res of
    Right val -> Right (fmap f val, eLogs res)
    Left err -> Left err

-- ** state

-- | Get something out of the Result from one of the states.  Like 'extract',
-- this is meant to be used as the single check on a cmd operation.
extractState :: (Ui.State -> Cmd.State -> e) -> Result val -> Extracted e
extractState f res = maybe
    (Right (f (resultUiState res) (resultCmdState res), eLogs res))
    Left (resultFailed res)

extractUiState :: (Ui.State -> e) -> Result val -> Extracted e
extractUiState f = extractState (\state _ -> f state)

eTracks :: Result a -> Extracted [UiTest.TrackSpec]
eTracks = extractUiState UiTest.extractTracks

ePitchTracks :: Result a -> Extracted [[UiTest.EventSpec]]
ePitchTracks = extractUiState $
    UiTest.toPitchSpec . UiTest.toNoteSpec . UiTest.extractTracks

extractUi :: Ui.StateId e -> Result v -> Extracted e
extractUi m = extractUiState $ \state -> UiTest.eval state m

-- * inst db

-- | Configure ustate and cstate with the given instruments.
setSynthsSimple :: [MidiInst.Synth] -> DeriveTest.SimpleAllocations
    -> Ui.State -> Cmd.State -> (Ui.State, Cmd.State)
setSynthsSimple synths simpleAllocs uiState cmdState =
    ( (Ui.config#UiConfig.allocations #= allocs) uiState
    , cmdState
        { Cmd.stateConfig = (Cmd.stateConfig cmdState)
            { Cmd.configInstrumentDb = db }
        }
    )
    where
    db = DeriveTest.synthsToDb synths
    allocs = DeriveTest.simpleAllocs simpleAllocs

-- * msg

emptyContext :: UiMsg.Context
emptyContext = UiMsg.Context Nothing Nothing False

makeKeyMods :: [Key.Modifier] -> UiMsg.KbdState -> Key.Key -> Msg.Msg
makeKeyMods mods state k = Msg.Ui
    (UiMsg.UiMsg emptyContext (UiMsg.MsgEvent (UiMsg.Kbd state mods k text)))
    where
    text = case (k, state) of
        (Key.Char c, UiMsg.KeyDown) -> Just c
        _ -> Nothing

makeKey :: UiMsg.KbdState -> Key.Key -> Msg.Msg
makeKey = makeKeyMods []

keypress :: Key.Key -> [Msg.Msg]
keypress k = [makeKey UiMsg.KeyDown k, makeKey UiMsg.KeyUp k]

keypresses :: [Key.Key] -> [Msg.Msg]
keypresses = concatMap keypress

keyDown :: Char -> Msg.Msg
keyDown = makeKey UiMsg.KeyDown . Key.Char

keyUp :: Char -> Msg.Msg
keyUp = makeKey UiMsg.KeyUp . Key.Char

backspace :: Msg.Msg
backspace = makeKey UiMsg.KeyDown Key.Backspace

mouse :: Bool -> Types.MouseButton -> TrackNum -> ScoreTime -> Msg.Msg
mouse down btn track pos = Msg.Ui $ UiMsg.UiMsg context $
    UiMsg.MsgEvent $ UiMsg.Mouse $ UiMsg.MouseEvent state [] (42, 2) 0 True
    where
    state = if down then UiMsg.MouseDown btn else UiMsg.MouseUp btn
    context = emptyContext { UiMsg.ctxTrack = Just (track, UiMsg.Track pos) }

drag :: Types.MouseButton -> TrackNum -> ScoreTime -> Msg.Msg
drag btn track pos = Msg.Ui $ UiMsg.UiMsg context $
    UiMsg.MsgEvent $ UiMsg.Mouse $
        UiMsg.MouseEvent (UiMsg.MouseDrag btn) [] (42, 2) 0 False
    where
    context = emptyContext { UiMsg.ctxTrack = Just (track, UiMsg.Track pos) }

makeMidi :: Midi.ChannelMessage -> Msg.Msg
makeMidi chanMsg = Msg.Midi $
    Midi.ReadMessage (Midi.readDevice "test") 0
        (Midi.ChannelMessage 0 chanMsg)

noteOn :: Int -> Pitch.Pitch -> InputNote.Input
noteOn noteId pitch = InputNote.NoteOn (InputNote.NoteId noteId)
    (Pitch.Input Pitch.AsciiKbd pitch 0) 1

noteOnNn :: Pitch.NoteNumber -> InputNote.Input
noteOnNn nn = InputNote.NoteOn noteId (InputNote.nnToInput nn) 1
    where noteId = InputNote.NoteId $ floor nn

noteOff :: Int -> InputNote.GenericInput a
noteOff noteId = InputNote.NoteOff (InputNote.NoteId noteId) 1

pitchChange :: Int -> Pitch.Pitch -> InputNote.Input
pitchChange noteId pitch = InputNote.PitchChange (InputNote.NoteId noteId)
    (Pitch.Input Pitch.AsciiKbd pitch 0)

pitchChangeNn :: Int -> Pitch.NoteNumber -> InputNote.Input
pitchChangeNn noteId nn =
    InputNote.PitchChange (InputNote.NoteId noteId) (InputNote.nnToInput nn)

control :: Int -> ScoreT.Control -> Signal.Y -> InputNote.GenericInput a
control noteId cont val =
    InputNote.Control (InputNote.NoteId noteId) cont val

mNoteOn :: Pitch.NoteNumber -> Msg.Msg
mNoteOn = Msg.InputNote . noteOnNn

mNoteOff :: Int -> Msg.Msg
mNoteOff = Msg.InputNote . noteOff

mControl :: Int -> ScoreT.Control -> Signal.Y -> Msg.Msg
mControl noteId cont val = Msg.InputNote (control noteId cont val)

mPitchChange :: Int -> Pitch.NoteNumber -> Msg.Msg
mPitchChange nid = Msg.InputNote . pitchChangeNn nid

-- * Pitch.Input

asciiKbd :: Pitch.Pitch -> Pitch.Input
asciiKbd pitch = Pitch.Input Pitch.AsciiKbd pitch 0

pianoKbd :: Pitch.Pitch -> Pitch.Input
pianoKbd pitch = Pitch.Input Pitch.PianoKbd pitch 0

pitch :: Pitch.Octave -> Pitch.PitchClass -> Pitch.Accidentals -> Pitch.Pitch
pitch oct pc accs = Pitch.Pitch oct (Pitch.Degree pc accs)

octPc :: Pitch.Octave -> Pitch.PitchClass -> Pitch.Pitch
octPc oct pc = pitch oct pc 0

-- * setup cmds

setScale :: Cmd.M m => BlockId -> BlockId -> TrackId -> Pitch.ScaleId -> m ()
setScale rootId blockId trackId scaleId = setEnv rootId blockId trackId
    [(EnvKey.scale, DeriveT.VStr (Expr.scaleIdToStr scaleId))]

-- | Fake up just enough Performance to have environ in it.
setEnv :: Cmd.M m => BlockId -> BlockId -> TrackId
    -> [(Env.Key, DeriveT.Val)] -> m ()
setEnv rootId blockId trackId environ =
    Cmd.modifyPlayState $ \st -> st
        { Cmd.statePerformanceThreads = Map.insert rootId
            (Cmd.Thread $ Unsafe.unsafePerformIO $ Async.async (pure ()))
            (Cmd.statePerformanceThreads st)
        , Cmd.statePerformance = Map.insert rootId perf
            (Cmd.statePerformance st)
        }
    where
    trackDyn = Map.singleton (blockId, trackId)
        (Derive.initialDynamic (Env.fromList environ))
    perf = emptyPerformance { Cmd.perfTrackDynamic = trackDyn }

emptyPerformance :: Msg.Performance
emptyPerformance = Cmd.Performance
    { perfDeriveCache = mempty
    , perfEvents = mempty
    , perfLogs = []
    , perfLogsWritten = True
    , perfTrackDynamic = mempty
    , perfIntegrated = []
    , perfDamage = mempty
    , perfWarps = []
    , perfTrackSignals = mempty
    , perfBlockDeps = mempty
    , perfTrackInstruments = mempty
    , perfUiState = Ui.empty
    }
