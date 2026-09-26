-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utility functions for derive tests.
module Derive.DeriveTest where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import           GHC.Stack (HasCallStack)
import           System.FilePath ((</>))
import qualified System.IO.Unsafe as Unsafe

import qualified Util.Debug as Debug
import qualified Util.Lists as Lists
import qualified Util.Log as Log
import qualified Util.Num as Num
import qualified Util.Ranges as Ranges
import qualified Util.Test.Testing as Testing

import qualified App.Config as Config
import qualified App.Path as Path
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Cmd.PlayUtil as PlayUtil
import qualified Cmd.SaveGitT as SaveGitT
import qualified Cmd.Simple as Simple

import qualified Derive.C.All as C.All
import qualified Derive.C.Prelude.Block as Prelude.Block
import qualified Derive.Controls as Controls
import           Derive.DDebug () -- just make sure it compiles
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Eval as Eval
import qualified Derive.Expr as Expr
import qualified Derive.LEvent as LEvent
import qualified Derive.PSignal as PSignal
import qualified Derive.Scale as Scale
import qualified Derive.Scale.All2 as Scale.All2
import qualified Derive.Scale.Scales as Scales
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Stack as Stack
import qualified Derive.Stream as Stream
import           Derive.TestInstances ()
import qualified Derive.Typecheck as Typecheck

import qualified Instrument.Common as Common
import qualified Instrument.Inst as Inst
import qualified Instrument.InstT as InstT

import qualified Midi.Midi as Midi
import qualified Midi.StubMidi as StubMidi
import qualified Perform.Im.Convert as Im.Convert
import qualified Perform.Im.Patch as Im.Patch
import qualified Perform.Midi.Convert as Convert
import qualified Perform.Midi.Patch as Patch
import qualified Perform.Midi.Perform as Perform
import qualified Perform.Midi.Types as Midi.Types
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import qualified Synth.Shared.Config as Shared.Config
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note

import qualified Ui.Block as Block
import qualified Ui.Color as Color
import qualified Ui.Id as Id
import qualified Ui.Ruler as Ruler
import qualified Ui.Skeleton as Skeleton
import qualified Ui.Track as Track
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig
import qualified Ui.UiTest as UiTest

import           Global
import           Types


-- | Simulate the linear interpolate call, to check deriver output that
-- involves that particular call.
--
-- Drops the first sample to make this match output from the derive call
-- interpolators.
signalInterpolate :: RealTime -> Signal.Y -> RealTime -> Signal.Y
    -> [(RealTime, Signal.Y)]
signalInterpolate x0 y0 x1 y1 =
    drop 1 [(x, project x) | x <- Lists.rangeEnd x0 x1 1]
    where
    project = Num.scale y0 y1 . Num.normalize (toY x0) (toY x1) . toY
    toY = RealTime.toSeconds

-- * run

run :: Ui.State -> Derive.Deriver a
    -> Either Text (a, Derive.State, [Log.Msg])
run uiState m = run_ uiState (Internal.withStackBlock bid m)
    where
    -- Make sure Derive.get_current_block_id, called by add_new_track_warp,
    -- doesn't throw.
    bid = UiTest.bid "derive-test.run-fakeblock"

-- | Run without a fake stack.
run_ :: Ui.State -> Derive.Deriver a
    -> Either Text (a, Derive.State, [Log.Msg])
run_ uiState m = case Derive.run deriveState m of
    (Left err, _, _logs) -> Left (pretty err)
    (Right val, state, logs) -> Right (val, state, logs)
    where
    deriveState = Derive.initialState
        (defaultConstant uiState mempty mempty) defaultDynamic

extractRun :: (a -> b) -> Either Text (a, Derive.State, [Log.Msg])
    -> Either Text b
extractRun _ (Left err) = Left err
extractRun extract (Right (val, _, msgs)) =
    Right $ traceLogs msgs (extract val)

runEvents :: (a -> b)
    -> Either Text ([LEvent.LEvent a], Derive.State, [Log.Msg])
    -> Either Text ([b], [Text])
runEvents extract = extractRun $ extractLevents extract

eval :: Ui.State -> Derive.Deriver a -> Either Text a
eval uiState m = extractRun id (run uiState m)

-- * perform

performBlock :: [UiTest.TrackSpec] -> ([Midi.WriteMessage], [Text])
performBlock tracks = performBlocks [(UiTest.defaultBlockName, tracks)]

performBlocks :: [UiTest.BlockSpec] -> ([Midi.WriteMessage], [Text])
performBlocks blocks = (mmsgs, map showLog (filter interestingLog logs))
    where
    ((_, mmsgs), logs) = perform defaultConvertLookup
        UiTest.defaultAllocations (Derive.rEvents result)
    result = deriveBlocks blocks

-- TODO this tosses the logs from the stream, shouldn't I collect them into
-- the output?
perform :: Lookup -> UiConfig.Allocations -> Stream.Stream Score.Event
    -> (([Midi.Types.Event], [Midi.WriteMessage]), [Log.Msg])
perform lookup allocs stream =
    ((fst (LEvent.partition perfEvents), mmsgs), logs)
    where
    (perfEvents, perf) = performStream lookup allocs stream
    (mmsgs, logs) = extractLogs perf

performDefaults :: Stream.Stream Score.Event
    -> (([Midi.Types.Event], [Midi.WriteMessage]), [Log.Msg])
performDefaults = perform defaultConvertLookup UiTest.defaultAllocations

performStream :: Lookup -> UiConfig.Allocations -> Stream.Stream Score.Event
    -> ([LEvent.LEvent Midi.Types.Event], [LEvent.LEvent Midi.WriteMessage])
performStream (lookupInst, lookup) allocs stream = (perfEvents, midi)
    where
    perfEvents = Convert.convert 1 lookup lookupInst (Stream.eventsOf stream)
    (midi, _) = Perform.perform Perform.initialState midiAllocs perfEvents
    midiAllocs = Perform.config <$> PlayUtil.midiConfigs allocs

-- | Perform events with the given instrument db.
performSynths :: UiConfig.Allocations -> [MidiInst.Synth]
    -> Stream.Stream Score.Event
    -> (([Midi.Types.Event], [Midi.WriteMessage]), [Log.Msg])
performSynths allocs synths =
    perform (synthsToConvertLookup allocs synths) allocs

performSynthsSimple :: SimpleAllocations -> [MidiInst.Synth]
    -> Stream.Stream Score.Event
    -> (([Midi.Types.Event], [Midi.WriteMessage]), [Log.Msg])
performSynthsSimple allocs_ synths =
    perform (makeConvertLookup allocs db) allocs
    where
    db = synthsToDb synths
    allocs = simpleAllocs allocs_

-- | Chain a 'perform' from a Derive.Result.
performResult :: (Stream.Stream Score.Event -> (a, [Log.Msg]))
    -> Derive.Result -> (a, [Text])
performResult perform result = (val, map showLog (deriveLogs ++ perfLogs))
    where
    (events, deriveLogs) = extractLogs $ Stream.toList $
        Derive.rEvents result
    (val, perfLogs) = perform (Stream.fromEvents events)

-- ** perform im

performImSynths :: SimpleAllocations -> [MidiInst.Synth]
    -> Stream.Stream Score.Event -> ([Note.Note], [Text])
performImSynths allocs synths =
    performIm (fst $ makeConvertLookup uiAllocs db)
    where
    db = synthsToDb synths
    uiAllocs = imAllocs allocs

-- | Unlike 'perform', this includes the logs from the stream in the output.
-- TODO change perform?
performIm :: (ScoreT.Instrument -> Maybe Cmd.ResolvedInstrument)
    -> Stream.Stream Score.Event -> ([Note.Note], [Text])
performIm lookupInst stream =
    (perfEvents, mapMaybe showInterestingLog $ deriveLogs ++ perfLogs)
    where
    (events, deriveLogs) = Stream.partition stream
    (perfEvents, perfLogs) = LEvent.partition $
        Im.Convert.convert UiTest.defaultBlockId lookupInst events

-- * derive

deriveTracks :: Text -> [UiTest.TrackSpec] -> Derive.Result
deriveTracks = deriveTracksSetup mempty

-- | Derive tracks but with a linear skeleton.  Good for testing note
-- transformers since the default skeleton parsing won't create those.
deriveTracksLinear :: Text -> [UiTest.TrackSpec] -> Derive.Result
deriveTracksLinear = deriveTracksSetup withLinear

-- | Variant that lets you run setup on various states.
deriveTracksSetup :: Setup -> Text -> [UiTest.TrackSpec] -> Derive.Result
deriveTracksSetup setup title tracks = deriveBlocksSetup setup
    [(UiTest.defaultBlockName <> " -- " <> title, tracks)]

-- ** derive block variations

-- These are all built on 'derive_block_standard', with various things
-- specialized.

-- | Create multiple blocks, and derive the first one.
deriveBlocks :: [UiTest.BlockSpec] -> Derive.Result
deriveBlocks = deriveBlocksSetup mempty

deriveBlock :: Ui.State -> BlockId -> Derive.Result
deriveBlock = deriveBlockSetup mempty

deriveBlocksSetup :: Setup -> [UiTest.BlockSpec] -> Derive.Result
deriveBlocksSetup setup blockTracks = deriveBlockSetup setup uiState bid
    where (bid : _, uiState) = UiTest.runMkblocks blockTracks

-- | Derive a block with the testing environ.
deriveBlockSetup :: Setup -> Ui.State -> BlockId -> Derive.Result
deriveBlockSetup setup =
    deriveBlockStandard (withEnviron defaultEnviron <> setup)
        defaultCmdState mempty mempty

-- | Like 'derive_block_setup', but exec the StateId.
deriveBlockSetupM :: Setup -> Ui.StateId a -> BlockId -> Derive.Result
deriveBlockSetupM setup create =
    deriveBlockStandard (withEnviron defaultEnviron <> setup)
        defaultCmdState mempty mempty (UiTest.exec Ui.empty create)

-- | Derive the results of a "Cmd.Repl.LDebug".dump_block.
deriveDump :: [MidiInst.Synth] -> Simple.State -> BlockId -> Derive.Result
deriveDump synths dump@(_, simpleAllocs, _) =
    deriveBlockSetup (withSynths allocs synths) state
    where
    allocs = Simple.allocations simpleAllocs
    state = UiTest.eval Ui.empty (Simple.loadState dump)

-- | Derive a block in the same way that the app does.
deriveBlockStandard :: Setup -> Cmd.State -> Derive.Cache
    -> Derive.ScoreDamage -> Ui.State -> BlockId -> Derive.Result
deriveBlockStandard setup cmdState cache damage uiState_ blockId =
    runCmd uiState (setupCmd setup cmdState) cmd
    where
    cmd = Derive.extractResult <$> PlayUtil.run cache damage deriver
    uiState = setupUi setup uiState_
    deriver = setupDeriver setup $ Prelude.Block.evalRootBlock blockId

performDump :: [MidiInst.Synth] -> Simple.State -> Derive.Result
    -> (([Midi.Types.Event], [Midi.WriteMessage]), [Log.Msg])
performDump synths (_, simpleAllocs, _) =
    perform lookup allocs . Derive.rEvents
    where
    db = synthsToDb synths
    lookup = makeConvertLookup allocs db
    allocs = Simple.allocations simpleAllocs

derive :: Ui.State -> Derive.NoteDeriver -> Derive.Result
derive uiState deriver = Derive.extractResult $
    Derive.derive (defaultConstant uiState mempty mempty)
        defaultDynamic deriver

-- * cmd

-- | CmdTest also has this, but I can't import it because it imports
-- DeriveTest.
runCmd :: HasCallStack => Ui.State -> Cmd.State -> Cmd.CmdId a -> a
runCmd uiState cmdState cmd = case result of
    Right (Just result, _, _) -> result
    Right (Nothing, _, _) -> error "DeriveTest.run_cmd: Cmd aborted"
    Left err -> error $ "DeriveTest.run_cmd: Cmd error: " <> show err
    where
    (_cstate, _midiMsgs, _logs, result) = Cmd.runId uiState cmdState cmd

-- * setup

type Setup = SetupA (Stream.Stream Score.Event)

-- | This file only ever uses 'Setup', but it loses polymorphism.  To reuse
-- @with_*@ functions in a polymorphic way, I can use SetupA and pull them back
-- out with setup_deriver.
data SetupA a = Setup
    { setupUi :: Ui.State -> Ui.State
    , setupCmd :: Cmd.State -> Cmd.State
    , setupDeriver :: Derive.Deriver a -> Derive.Deriver a
    }

instance Semigroup (SetupA a) where
    Setup ui1 cmd1 deriver1 <> Setup ui2 cmd2 deriver2 =
        Setup (ui1 . ui2) (cmd1 . cmd2) (deriver1 . deriver2)
instance Monoid (SetupA a) where
    mempty = Setup id id id
    mappend = (<>)

withUi :: (Ui.State -> Ui.State) -> Setup
withUi ui = mempty { setupUi = ui }

withCmd :: (Cmd.State -> Cmd.State) -> Setup
withCmd cmd = mempty { setupCmd = cmd }

withDeriver :: (Derive.Deriver a -> Derive.Deriver a) -> SetupA a
withDeriver deriver = mempty { setupDeriver = deriver }

-- ** setup_ui

-- | Set the skeleton of the tracks to be linear, i.e. each track is the child
-- of the one to the left.  This overrides the default behaviour of figuring
-- out a skeleton by making note tracks start their own branches.
withLinear :: Setup
withLinear = withLinearBlock UiTest.defaultBlockId

withLinearBlock :: BlockId -> Setup
withLinearBlock blockId =
    withUi $ \state -> setupUi (withSkelBlock blockId (skel state)) state
    where
    -- Start at 1 to exclude the ruler.
    skel state =
        [(x, y) | (x, Just y) <- Lists.zipNext [1 .. ntracks state  - 1]]
    ntracks state = length $ maybe [] Block.blockTracks $
        Map.lookup blockId (Ui.stateBlocks state)

withSkel :: [Skeleton.Edge] -> Setup
withSkel = withSkelBlock UiTest.defaultBlockId

withSkelBlock :: BlockId -> [Skeleton.Edge] -> Setup
withSkelBlock blockId skel = withUi $ \state -> UiTest.exec state $
    Ui.setSkeleton blockId (Skeleton.make skel)

withDefaultRuler :: Ruler.Ruler -> Setup
withDefaultRuler ruler = withUi $
    Ui.rulers %= Map.insert UiTest.defaultRulerId ruler

-- | Set the ruler on the given block.
withRuler :: BlockId -> Ruler.Ruler -> Setup
withRuler blockId ruler = withUi $ \state -> UiTest.exec state make
    where make = Create.newRuler blockId (Id.identName blockId) ruler

withTsigs :: [TrackId] -> Setup
withTsigs = withTsigSources . map (, Nothing)

withTsigTracknums :: [TrackNum] -> Setup
withTsigTracknums = withTsigs . map UiTest.mkTid

withTsigSources :: [(TrackId, Maybe Track.RenderSource)] -> Setup
withTsigSources trackIds = withUi $ Ui.tracks %= Map.mapWithKey enable
    where
    enable trackId track = case lookup trackId trackIds of
        Just source -> track { Track.trackRender =
            Track.RenderConfig (Track.Line source) Color.blue }
        Nothing -> track

withMidiConfig :: ScoreT.Instrument -> Text -> Common.Config -> Patch.Config
    -> Setup
withMidiConfig inst qualified commonConfig midiConfig = withUi $
    Ui.config#UiConfig.allocationsMap %= Map.insert inst
        (UiConfig.Allocation (InstT.parseQualified qualified)
            commonConfig (UiConfig.Midi midiConfig))

-- * setup_deriver

withKey :: Text -> SetupA a
withKey key = withDeriver $ Derive.withVal EnvKey.key key

withEnviron :: Env.Environ -> SetupA a
withEnviron env =
    withDeriver $ Internal.local $ \st -> st { Derive.stateEnviron = env }

-- | I'm not really supposed to do this, but should be *mostly* ok for tests.
-- It's not entirely ok because some values are baked in to e.g. lookup
-- functions.
modifyConstant :: (Derive.Constant -> Derive.Constant) -> SetupA a
modifyConstant f = withDeriver $ \deriver -> do
    Derive.modify $ \st ->
        st { Derive.stateConstant = f (Derive.stateConstant st) }
    deriver
    -- TODO replace with safe specific ones, e.g. with_damage

-- | Not supposed to do this in general, but it's ok for tests.
modifyDynamic :: (Derive.Dynamic -> Derive.Dynamic) -> Derive.Deriver ()
modifyDynamic f = Derive.modify $ \st ->
    st { Derive.stateDynamic = f (Derive.stateDynamic st) }

-- * setup multiple

withScale :: Scale.Scale -> Setup
withScale scale = modifyConstant $ \c ->
    c { Derive.stateLookupScale = Derive.LookupScale lookup }
    where
    lookup env scaleId
        | scaleId == Scale.scaleId scale = Just (Right scale)
        -- Fall back on the old lookup.  This is important because *twelve is
        -- the default scale so I want it to keep working.
        | otherwise = deflt env scaleId
        where Derive.LookupScale deflt = defaultLookupScale

-- | Derive with a bit of a real instrument db.  Useful for testing instrument
-- calls.
withSynths :: UiConfig.Allocations -> [MidiInst.Synth] -> Setup
withSynths allocs synths = withInstrumentDb allocs (synthsToDb synths)

withSynthsIm :: SimpleAllocations -> [MidiInst.Synth] -> Setup
withSynthsIm allocs synths =
    withInstrumentDb (imAllocs allocs) db
    where db = synthsToDb synths

withSynthsSimple :: SimpleAllocations -> [MidiInst.Synth] -> Setup
withSynthsSimple allocs synths =
    withInstrumentDb (simpleAllocs allocs) db
    where db = synthsToDb synths

-- | Add a single instrument with a MIDI patch.
withPatch :: (MidiInst.Patch -> MidiInst.Patch) -> Text -> Setup
withPatch configurePatch name =
    withSynthsSimple [(name, name <> "/")] [synth]
    where
    synth = UiTest.makeSynth name [patch]
    patch = configurePatch $ MidiInst.makePatch $ UiTest.makePatch ""

withInstrumentDb :: UiConfig.Allocations -> Cmd.InstrumentDb -> Setup
withInstrumentDb allocs db = withAllocations allocs <> withDb
    where
    withDb = withCmd $ setCmdConfig $ \state -> state
        { Cmd.configInstrumentDb = db }

-- | A further-simplified version of 'Simple.Allocations' for tests:
-- [(Instrument, Qualified)]
type SimpleAllocations = [(Text, Text)]

simpleAllocs :: SimpleAllocations -> UiConfig.Allocations
simpleAllocs allocs = Simple.allocations
    [ (inst, (qual, Simple.Midi [("wdev", chan)]))
    | (chan, (inst, qual)) <- zip [0..] allocs
    ]

imAllocs :: SimpleAllocations -> UiConfig.Allocations
imAllocs allocs =
    Simple.allocations [(inst, (qual, Simple.Im)) | (inst, qual) <- allocs]

lookupQualified :: Cmd.InstrumentDb -> InstT.Qualified
    -> Maybe Cmd.Inst
lookupQualified db qual = Inst.lookup qual db

withAllocations :: UiConfig.Allocations -> Setup
withAllocations allocs =
    withUi $ Ui.config#UiConfig.allocations %= (allocs <>)

setCmdConfig :: (Cmd.Config -> Cmd.Config) -> Cmd.State -> Cmd.State
setCmdConfig f state = state { Cmd.stateConfig = f (Cmd.stateConfig state) }

-- ** defaults

defaultCmdState :: Cmd.State
defaultCmdState = Cmd.initialState (cmdConfig UiTest.defaultDb)

-- | Config to initialize the Cmd.State.
cmdConfig :: Cmd.InstrumentDb -> Cmd.Config
cmdConfig instDb = Cmd.Config
    { configAppDir = Path.AppDir "."
    , configSaveDir = Path.makeCanonical "."
    , configMidiInterface = Unsafe.unsafePerformIO StubMidi.interface
    , configKyPaths = []
    , configRdevMap = mempty
    , configWdevMap = mempty
    , configInstrumentDb = instDb
    , configBuiltins = C.All.builtins
    , configHighlightColors = Config.highlightColors
    , configIm = Shared.Config.Config
        { imDir = Testing.tmpBaseDir </> "im"
        , synths = Map.fromList
            [ ("im-synth", Shared.Config.Synth
                { synthName = "test-synth"
                , binary = ""
                })
            ]
        }
    , configGitUser = SaveGitT.User "name" "email"
    , configImPlayDirect = False
    , configPlayCache = Nothing
    }

defaultImSynth :: Inst.SynthDecl Cmd.InstrumentCode
defaultImSynth = imSynth "im-synth"

imSynth :: InstT.SynthName -> Inst.SynthDecl Cmd.InstrumentCode
imSynth name = Inst.SynthDecl
    { synthdName = name
    , synthdDoc = name
    , synthdPatches = [(Patch.defaultName, inst)]
    , synthdWarns = []
    }
    where
    inst = Inst.Inst
        { instBackend = Inst.Im $ Im.Patch.patch
            { Im.Patch.patchControls = Map.fromList
                [ (Control.pitch, "pitch doc")
                , (Control.dynamic, "dyn doc")
                ]
            }
        , instCommon = Common.common Cmd.emptyCode
        }

defaultLookupScale :: Derive.LookupScale
defaultLookupScale = Cmd.lookupScale

defaultBuiltins :: Derive.Builtins
defaultBuiltins = C.All.builtins

defaultConstant :: Ui.State -> Derive.Cache -> Derive.ScoreDamage
    -> Derive.Constant
defaultConstant uiState cache damage = Derive.initialConstant uiState
    defaultBuiltins defaultLookupScale Scale.All2.scales
        (const (Left "DeriveTest.default_constant has no inst db")) cache
        damage

defaultDynamic :: Derive.Dynamic
defaultDynamic = Derive.initialDynamic defaultEnviron

-- | This will replace Monad.initial_dynamic state_environ entirely.
defaultEnviron :: Env.Environ
defaultEnviron = Env.fromList
    -- tests are easier to write and read with integral interpolation
    [ (EnvKey.srate, DeriveT.num 1)
    , (EnvKey.scale, DeriveT.VStr (Expr.scaleIdToStr Twelve.scaleId))
    , (EnvKey.attributes, DeriveT.VAttributes mempty)
    ] <> Derive.initialEnviron

-- *** instrument defaults

defaultConvertLookup :: Lookup
defaultConvertLookup =
    makeConvertLookup UiTest.defaultAllocations UiTest.defaultDb

synthsLookupQualified :: [MidiInst.Synth] -> InstT.Qualified
    -> Maybe Cmd.Inst
synthsLookupQualified synth = \qualified -> Inst.lookup qualified db
    where db = synthsToDb synth

synthsToConvertLookup :: UiConfig.Allocations -> [MidiInst.Synth] -> Lookup
synthsToConvertLookup allocs = makeConvertLookup allocs . synthsToDb

synthsToDb :: [MidiInst.Synth] -> Cmd.InstrumentDb
synthsToDb synths = traceLogs (map (Log.msg Log.Warn Nothing) warns) db
    where (db, warns) = Inst.db synths

type Lookup =
    (ScoreT.Instrument -> Maybe Cmd.ResolvedInstrument, Convert.MidiLookup)

-- | Make a Lookp for a single patch.
makeConvertLookupFor :: ScoreT.Instrument -> Patch.Config -> Patch.Patch
    -> Lookup
makeConvertLookupFor inst patchConfig patch =
    makeConvertLookup allocs instDb
    where
    allocs = UiConfig.midiAllocations
        [(inst, (InstT.Qualified "s" "1", patchConfig))]
    instDb = UiTest.makeDb [("s", [patch { Patch.patchName = "1" }])]

makeConvertLookup :: UiConfig.Allocations -> Cmd.InstrumentDb -> Lookup
makeConvertLookup allocs db =
    runCmd (setupUi setup Ui.empty) (setupCmd setup defaultCmdState) $ do
        lookupInst <- Cmd.getLookupInstrument
        pure (lookupInst, PlayUtil.makeMidiLookup lookupInst)
    where setup = withInstrumentDb allocs db

-- ** extract

-- *** log msgs

traceLogs :: [Log.Msg] -> a -> a
traceLogs = traceAllLogs . filter interestingLog

-- | Write log msgs with 'Debug.traceStr', for debugging.
traceAllLogs :: [Log.Msg] -> a -> a
traceAllLogs logs val
    | null logs = val
    | otherwise = Debug.traceStr
        (Text.stripEnd $ Text.unlines $ "\tlogged:" : map Log.formatMsg logs)
        val

-- | Filter out low-priority logs and trace them.
traceLowPrio :: [Log.Msg] -> [Log.Msg]
traceLowPrio msgs = traceAllLogs low high
    where (high, low) = List.partition interestingLog msgs

-- | Tests generally shouldn't depend on logs below a certain priority since
-- those don't indicate anything interesting.
interestingLog :: Log.Msg -> Bool
interestingLog = (>=Log.Warn) . Log.msgPriority

quietFilterLogs :: [Log.Msg] -> [Log.Msg]
quietFilterLogs = filter ((>=Log.Warn) . Log.msgPriority)

-- ** extract

extract :: (Score.Event -> a) -> Derive.Result -> ([a], [Text])
extract eEvent = extractLevents eEvent . Stream.toList . Derive.rEvents

filterEvents :: (Score.Event -> Bool) -> Derive.Result -> Derive.Result
filterEvents f result =
    result { Derive.rEvents = filt (Derive.rEvents result) }
    where
    filt = Stream.fromSortedList . filter (LEvent.logOr f) . Stream.toList

filterEventsRange :: RealTime -> RealTime -> Derive.Result -> Derive.Result
filterEventsRange start end = filterEvents $ \e ->
    start <= Score.eventStart e && Score.eventStart e < end

extractEvents :: (Score.Event -> a) -> Derive.Result -> [a]
extractEvents eEvent result = traceAllLogs logs (map eEvent events)
    where (events, logs) = rSplit result

extractLevents :: (a -> b) -> [LEvent.LEvent a] -> ([b], [Text])
extractLevents eEvent = bimap (map eEvent) (map showLog) . extractLogs

extractLogs :: [LEvent.LEvent a] -> ([a], [Log.Msg])
extractLogs = second (filter interestingLog) . LEvent.partition

extractStream :: (Score.Event -> a) -> Derive.Result -> [Either a Text]
extractStream eEvent = mapMaybe extract . Stream.toList . Derive.rEvents
    where
    extract (LEvent.Log log)
        | interestingLog log = Just $ Right $ showLog log
        | otherwise = Nothing
    extract (LEvent.Event e) = Just $ Left $ eEvent e

rSplit :: Derive.Result -> ([Score.Event], [Log.Msg])
rSplit = second (filter interestingLog) . Stream.partition . Derive.rEvents

rLogs :: Derive.Result -> [Log.Msg]
rLogs = snd . rSplit

stackToUi :: Stack.Stack -> [Text]
stackToUi = map Stack.unparseUiFrame . Stack.toUi

rLogStrings :: Derive.Result -> [Text]
rLogStrings = snd . extract id

eEvent :: Score.Event -> (RealTime, RealTime, Text)
eEvent e = (Score.eventStart e, Score.eventDuration e, Score.eventText e)

eStartDur :: Score.Event -> (RealTime, RealTime)
eStartDur e = (Score.eventStart e, Score.eventDuration e)

eInstrument :: Score.Event -> Text
eInstrument = ScoreT.instrumentName . Score.eventInstrument

eControl :: ScoreT.Control -> Score.Event -> [(RealTime, Signal.Y)]
eControl control = Lists.dropDups id
    . maybe [] (Signal.toPairs . ScoreT.valOf)
    . Score.eventControl control

eControls :: Score.Event -> [(ScoreT.Control, [(Signal.X, Signal.Y)])]
eControls = map (second (Signal.toPairs . ScoreT.valOf)) . Map.toList
    . Score.eventControls

lookupControl :: ScoreT.Control -> Derive.Dynamic -> Maybe ScoreT.TypedSignal
lookupControl (ScoreT.Control c) =
    toSignal <=< Env.lookup c . Derive.stateEnviron
    where
    toSignal val = case Typecheck.valToSignal val of
        Just (Right sig) -> Just sig
        _ -> Nothing

eControlVals :: ScoreT.Control -> Score.Event -> [Signal.Y]
eControlVals control = map snd . Lists.dropInitialDups fst . eControl control

eControlConstant :: ScoreT.Control -> Score.Event -> Maybe Signal.Y
eControlConstant control = Signal.constantVal . ScoreT.valOf
    <=< Map.lookup control . Score.eventControls

eInitialControl :: ScoreT.Control -> Score.Event -> Maybe Signal.Y
eInitialControl control event = ScoreT.valOf <$>
    Score.controlAt (Score.eventStart event) control event

eDyn :: Score.Event -> [(RealTime, Signal.Y)]
eDyn = Lists.dropDups id . eControl Controls.dynamic

eDynRounded :: Score.Event -> [(RealTime, Signal.Y)]
eDynRounded = Lists.dropDups id . map (second (Num.roundDigits 2))
    . eControl Controls.dynamic

-- | Like 'e_nns', but drop discontinuities.
--
-- TODO don't use this for new tests, this is just left over from pre-linear
-- segments so I don't have to update a million tests.
eNnsOld :: HasCallStack => Score.Event -> [(RealTime, Pitch.NoteNumber)]
eNnsOld e
    | not (null errs) = error $ "errors flattening signal: " <> show errs
    | otherwise = Lists.dropDups snd $ Lists.dropInitialDups fst sig
    where (sig, errs) = eNnsErrors e

-- | Like 'e_nns_errors', but throw an exception if there are errors.
eNns :: HasCallStack => Score.Event -> [(RealTime, Pitch.NoteNumber)]
eNns e
    | not (null errs) = error $ "errors flattening signal: " <> show errs
    | otherwise = Lists.dropDups id sig
    where (sig, errs) = eNnsErrors e

-- | Like 'e_nns', but round to cents to make comparison easier.
eNnsRounded :: Score.Event -> [(RealTime, Pitch.NoteNumber)]
eNnsRounded = map (second (Num.roundDigits 2)) . eNns

eNnRounded :: Score.Event -> Pitch.NoteNumber
eNnRounded = maybe 0 (Num.roundDigits 2) . Score.initialNn

-- | Extract pitch signal and any errors flattening it.
eNnsErrors :: Score.Event -> ([(RealTime, Pitch.NoteNumber)], [Text])
eNnsErrors =
    bimap (map (second Pitch.nn) . Signal.toPairs) (map pretty)
    . PSignal.toNn . Score.eventPitch . Score.normalize

flatSegments :: [(x, y)] -> [(x, y)]
flatSegments = concatMap make . Lists.zipNext
    where
    make ((x0, y), Just (x1, _)) = [(x0, y), (x1, y)]
    make (xy, Nothing) = [xy]

ePitch :: Score.Event -> Text
ePitch e = maybe "?" Pitch.noteText (Score.initialNote e)

-- | (start, dur, pitch), the melodic essentials of a note.
eNote :: Score.Event -> (RealTime, RealTime, Text)
eNote e = (Score.eventStart e, Score.eventDuration e, ePitch e)

eStartNote :: Score.Event -> (RealTime, Text)
eStartNote e = (Score.eventStart e, ePitch e)

eAttributes :: Score.Event -> Text
eAttributes = ShowVal.showVal . Score.eventAttributes

eEnviron :: Env.Key -> Score.Event -> Maybe Text
eEnviron name = fmap ShowVal.showVal . Env.lookup name . Score.eventEnviron

eEnvironLike :: (Text -> Bool) -> Score.Event -> [(Env.Key, Text)]
eEnvironLike f event =
    [ (k, ShowVal.showVal v)
    | (k, v) <- Env.toList (Score.eventEnviron event), f k
    ]

eEnvironVal :: Typecheck.Typecheck a => Env.Key -> Score.Event -> Maybe a
eEnvironVal name = Env.maybeVal name . Score.eventEnviron

eTsigs :: Derive.Result -> [((BlockId, TrackId), [(Signal.X, Signal.Y)])]
eTsigs =
    filter (not . null . snd) . Map.toList . Map.map tsig
        . Derive.rTrackSignals
    where tsig t = Signal.toPairs $ Track.tsSignal t

eTsigLogs :: Derive.Result -> [Text]
eTsigLogs = filter ("Track signal: " `Text.isPrefixOf`) . map showLog
    . Stream.logsOf . Derive.rEvents

-- ** extract log msgs

showLogStack :: Log.Msg -> Text
showLogStack msg = showStack (Log.msgStack msg) <> ": " <> showLog msg

-- | Show an abbreviated stack from the log.  This is meant to be similar to
-- how LogView does it, so that in tests I get an accurate idea of the actual
-- error.
showStack :: Maybe Stack.Stack -> Text
showStack =  showStack_ True

showStack_ :: Bool -> Maybe Stack.Stack -> Text
showStack_ _ Nothing = ""
showStack_ withCall (Just stack)
    | null ui = "<no stack>"
    -- This uses ': ' so 'x: *' works regardless of where in the stack x is.
    | otherwise = Text.intercalate ": " (map Stack.unparseUiFrame_ ui)
        <> maybe "" (": "<>) (lastCall stack)
    where
    ui = Stack.toUi stack
    lastCall
        | withCall = fmap (\c -> if Text.null c then "''" else c) . Lists.head
            . mapMaybe Stack.callOf . Stack.innermost
        | otherwise = const Nothing

showLog :: Log.Msg -> Text
showLog = Log.msgText

showInterestingLog :: Log.Msg -> Maybe Text
showInterestingLog msg
    | interestingLog msg = Just (Log.msgText msg)
    | otherwise = Nothing

-- ** extract midi msgs

noteOnVel :: [Midi.WriteMessage] -> [(Integer, Midi.Key, Midi.Velocity)]
noteOnVel msgs =
    [ (ts, nn, vel)
    | (ts, Midi.ChannelMessage _ (Midi.NoteOn nn vel)) <- extractMidi msgs
    ]

noteOn :: [Midi.WriteMessage] -> [Midi.Key]
noteOn msgs = [nn | (_, nn, _) <- noteOnVel msgs]

noteOnCc :: [Midi.WriteMessage]
    -> [Either Midi.Key (Midi.Control, Midi.ControlValue)]
noteOnCc = mapMaybe (extract . snd) . extractMidi
    where
    extract (Midi.ChannelMessage _ (Midi.NoteOn nn _)) = Just $ Left nn
    extract (Midi.ChannelMessage _ (Midi.ControlChange cc val)) =
        Just $ Right (cc, val)
    extract _ = Nothing

midiChannel :: [Midi.WriteMessage] -> [(Midi.Channel, Midi.ChannelMessage)]
midiChannel midi =
    [ (chan, msg)
    | Midi.ChannelMessage chan msg <- map Midi.wmsgMsg (interestingMidi midi)
    ]

extractMidi :: [Midi.WriteMessage] -> [(Integer, Midi.Message)]
extractMidi midi =
    [ (RealTime.toMilliseconds ts, msg)
    | Midi.WriteMessage _ ts msg <- interestingMidi midi
    ]

extractMidiMsg :: [Midi.WriteMessage] -> [Midi.Message]
extractMidiMsg = map snd . extractMidi

-- | Filter out boring msgs that I don't want tests to rely on.
interestingMidi :: [Midi.WriteMessage] -> [Midi.WriteMessage]
interestingMidi = filter $ \wmsg -> case Midi.wmsgMsg wmsg of
    Midi.ChannelMessage _ (Midi.PitchBend 0) -> False
    _ -> True

-- ** ui state

eState :: Derive.Result -> Ui.State
eState = Derive.stateUi . Derive.stateConstant . Derive.rState

-- * call

passedArgs :: Derive.CallName -> [DeriveT.Val] -> Derive.PassedArgs derived
passedArgs name vals = Derive.PassedArgs
    { Derive.passedVals = vals
    , Derive.passedCallName = name
    , Derive.passedCtx = Derive.dummyContext 0 1 "DeriveTest"
    }

cNote :: ScoreTime -> ScoreTime -> Derive.NoteDeriver
cNote sStart dur = do
    start <- Derive.real sStart
    end <- Derive.real (sStart + dur)
    inst <- Derive.getVal EnvKey.instrument
    environ <- Derive.getEnviron
    st <- Derive.gets Derive.stateDynamic
    cmap <- Derive.getControlMap
    pure $! Stream.fromEvent $! Score.emptyEvent
        { Score.eventStart = start
        , Score.eventDuration = end - start
        , Score.eventText = "evt"
        , Score.eventPitch = Derive.statePitch st
        , Score.eventInstrument = inst
        , Score.eventEnviron = environ <> Env.fromControls cmap
        }

-- * scale

mkscale :: Pitch.ScaleId -> [(Text, Pitch.NoteNumber)] -> Scale.Scale
mkscale scaleId notes =
    Scales.makeScale scaleId dmap "pattern" "simple test scale"
    where
    dmap = Scales.degreeMap 8 0 0 (map (Pitch.Note . fst) notes)
        (map snd notes)

-- * mkevents

-- | (start, dur, pitch12, controls, inst)
type EventSpec = (RealTime, RealTime, Text, Controls, ScoreT.Instrument)
type Controls = [(ScoreT.Control, [(RealTime, Signal.Y)])]
type ControlVals = [(ScoreT.Control, Signal.Y)]

mkevent :: HasCallStack => EventSpec -> Score.Event
mkevent = mkeventScale Twelve.scale

-- | Make an event with a non-twelve scale.
mkeventScale :: HasCallStack => Scale.Scale -> EventSpec -> Score.Event
mkeventScale scale = mkeventScaleKey scale Nothing

mkeventScaleKey :: HasCallStack => Scale.Scale -> Maybe Text
    -> EventSpec -> Score.Event
mkeventScaleKey scale key (start, dur, pitch, controls, inst) =
    maybe id (\k -> Score.modifyEnviron (Env.insertVal EnvKey.key k)) key $
    Score.emptyEvent
        { Score.eventStart = start
        , Score.eventDuration = dur
        , Score.eventText = ""
        , Score.eventEnviron = Env.fromControls $ mkcontrols controls
        , Score.eventPitch = PSignal.fromSample start (mkpitch scale pitch)
        , Score.eventStack = fakeStack
        , Score.eventInstrument = inst
        }

psignal :: [(RealTime, Text)] -> PSignal.PSignal
psignal = PSignal.fromPairs . map (second mkpitch12)

mkcontrols :: Controls -> ScoreT.ControlMap
mkcontrols cs = Map.fromList
    [(c, ScoreT.untyped (Signal.fromPairs sig)) | (c, sig) <- cs]

mkcontrolsConst :: ControlVals -> ScoreT.ControlMap
mkcontrolsConst cs = mkcontrols [(c, [(0, val)]) | (c, val) <- cs]

mkpitch12 :: Text -> PSignal.Pitch
mkpitch12 = mkpitch Twelve.scale

mkpitch :: HasCallStack => Scale.Scale -> Text -> PSignal.Pitch
mkpitch scale p = case eval Ui.empty deriver of
    Left err -> error $ "mkpitch " <> show p <> ": " <> untxt err
    Right pitch -> pitch
    where
    deriver = Derive.withScale scale $
        Eval.evalPitch 0 $ Expr.call0 (Expr.Symbol p)

defaultScale :: Scale.Scale
defaultScale = Twelve.scale

fakeStack :: Stack.Stack
fakeStack = Stack.fromOutermost
    [ Stack.Block (UiTest.bid "fakeblock")
    , Stack.Track (UiTest.tid "faketrack")
    , Stack.Region 42 43
    ]

-- * create misc

makeDamage :: Text -> TrackNum -> ScoreTime -> ScoreTime
    -> Derive.ScoreDamage
makeDamage block tracknum s e = Derive.ScoreDamage
    (Map.singleton (UiTest.mkTidName block tracknum) (Ranges.range s e))
    (Set.singleton (UiTest.bid block)) mempty
