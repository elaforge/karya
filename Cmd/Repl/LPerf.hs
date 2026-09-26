-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Cmds to deal with Cmd.Performance, derivation, and performance.
module Cmd.Repl.LPerf where
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as Exception
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Ratio as Ratio
import           Data.Ratio ((%))
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import qualified Util.Lists as Lists
import qualified Util.Log as Log
import qualified Util.Regex as Regex
import qualified Util.Texts as Texts

import qualified App.Config as Config
import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdUtil as CmdUtil
import qualified Cmd.Perf as Perf
import qualified Cmd.Performance as Performance
import qualified Cmd.PlayUtil as PlayUtil
import qualified Cmd.Repl.Util as Util
import qualified Cmd.Selection as Selection
import qualified Cmd.Simple as Simple

import qualified Derive.Cache as Cache
import qualified Derive.Derive as Derive
import qualified Derive.Env as Env
import qualified Derive.LEvent as LEvent
import qualified Derive.PSignal as PSignal
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.Stack as Stack
import qualified Derive.Stream as Stream
import qualified Derive.TrackWarp as TrackWarp
import qualified Derive.Warp as Warp

import qualified Midi.Midi as Midi
import qualified Midi.Synth as Synth
import qualified Perform.Im.Convert as Im.Convert
import qualified Perform.Midi.Convert as Midi.Convert
import qualified Perform.Midi.MSignal as MSignal
import qualified Perform.Midi.Perform as Perform
import qualified Perform.Midi.Types as Types
import qualified Perform.Pitch as Pitch
import qualified Perform.Sc.Convert as Sc.Convert
import qualified Perform.Sc.Note as Sc.Note
import qualified Perform.Signal as Signal
import qualified Perform.Transport as Transport

import qualified Synth.Shared.Config as Shared.Config
import qualified Synth.Shared.Note as Shared.Note
import qualified Ui.Id as Id
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig

import           Global
import           Types


-- | Whether the events come from the selected block as toplevel, or from the
-- root block's derivation, assuming the local block is called from the root.
data Source = Local | Root deriving (Show)

getRoot :: Cmd.M m => m Cmd.Performance
getRoot = Perf.getRoot

get :: Cmd.M m => BlockId -> m Cmd.Performance
get = Perf.get

getCurrent :: Cmd.M m => BlockId -> m Cmd.Performance
getCurrent blockId = Cmd.require ("no performance for " <> pretty blockId)
    =<< Map.lookup blockId <$>
        Cmd.gets (Cmd.stateCurrentPerformance . Cmd.statePlay)

pollThreads :: Cmd.CmdT IO
    (Map BlockId (Maybe (Either Exception.SomeException ())))
pollThreads = do
    threads <- Cmd.gets (Cmd.statePerformanceThreads  . Cmd.statePlay)
    liftIO $ traverse Async.poll $ fmap (\(Cmd.Thread t) -> t) threads

-- * info

environ :: Cmd.M m => m (Maybe Env.Environ)
environ = Perf.lookupEnviron =<< Selection.track

-- | Controls in scope at the insert point.
controls :: Cmd.M m => Source -> m ScoreT.ControlMap
controls source = do
    dyn <- dynamic source
    PlayUtil.evalWithDynamic dyn Derive.getControlMap

-- | The control vals at the insertion point, taking the control functions into
-- account.
controlVals :: Cmd.M m => Source -> m ScoreT.ControlValMap
controlVals source = do
    dyn <- dynamic source
    pos <- getRealtime source
    -- Dynamic doesn't record 'Derive.state_event_serial', so the randomization
    -- will likely be different.
    PlayUtil.evalWithDynamic dyn (Derive.controlsAt pos)

-- | Like 'control_vals', but without control functions.
rawControlVals :: Cmd.M m => Source -> m ScoreT.TypedControlValMap
rawControlVals source = do
    dyn <- dynamic source
    pos <- getRealtime source
    cmap <- PlayUtil.evalWithDynamic dyn Derive.getControlMap
    pure $ fmap (fmap (`Signal.at` pos)) cmap

aliases :: Cmd.M m => m (Map ScoreT.Instrument ScoreT.Instrument)
aliases = Derive.stateInstrumentAliases <$> dynamic Root

warp :: Cmd.M m => Source -> m Warp.Warp
warp source = Derive.stateWarp <$> dynamic source

dynamic :: Cmd.M m => Source -> m Derive.Dynamic
dynamic source = do
    track <- Selection.track
    Cmd.require ("no dynamic for track " <> pretty track) =<< case source of
        Root -> Perf.lookupRootDynamic track
        Local -> Perf.lookupDynamic (fst track) track

selToReal :: Cmd.M m => m [RealTime]
selToReal = do
    (blockId, _, trackId, pos) <- Selection.getInsert
    tempo <- Cmd.perfTempo <$> get blockId
    pure $ tempo blockId trackId pos

getRealtime :: Cmd.M m => Source -> m RealTime
getRealtime source = do
    (blockId, _, trackId, pos) <- Selection.getInsert
    perf <- case source of
        Root -> getRoot
        Local -> get blockId
    Perf.getRealtime perf blockId (Just trackId) pos

-- * analysis

type ControlVals =
    Map ScoreT.Control ((RealTime, Signal.Y), (RealTime, Signal.Y))

-- | Get the first and last values for each instrument.  This can be used to
-- show which controls the each instrument uses, which in turn can be used
-- to set its 'Instrument.config_controls', to make sure it is always
-- initialized consistently.
instControls :: Cmd.M m => BlockId -> m (Map ScoreT.Instrument ControlVals)
instControls blockId =
    foldl' merge mempty . LEvent.eventsOf <$> blockMidiEvents blockId
    where
    merge insts event =
        Map.insertWith (Map.unionWith merge1) (eventInst event)
            (controlVals (Types.eventControls event))
            insts
    controlVals = Map.mapMaybe $ \sig ->
        case (MSignal.head sig, MSignal.last sig) of
            (Just a, Just b) -> Just (a, b)
            _ -> Nothing
    merge1 (start1, end1) (start2, end2) =
        (Lists.minOn fst start1 start2, Lists.maxOn fst end1 end2)
    eventInst = Types.patchName . Types.eventPatch

-- * derive

-- These are mostly for testing, to find problems in performer output.

-- | Clear out all caches and rederive from scratch.
rederive :: Cmd.CmdT IO ()
rederive = do
    Cmd.invalidatePerformances
    Cmd.clearImCache =<< Cmd.getFocusedBlock

compareCachedEvents :: Cmd.M m => BlockId
    -> m [Either Simple.ScoreEvent Simple.ScoreEvent]
compareCachedEvents blockId = do
    uncached <- PlayUtil.uncachedDerive blockId
    cached <- PlayUtil.cachedDerive blockId
    pure $ diff (Stream.eventsOf (Derive.rEvents cached))
        (Stream.eventsOf (Derive.rEvents uncached))
    where
    diff e1 e2 = Lists.diffEither (==)
        (map Simple.scoreEvent e1) (map Simple.scoreEvent e2)

derive :: Cmd.M m => BlockId -> m Derive.Result
derive = PlayUtil.cachedDerive

uncachedDerive :: Cmd.M m => BlockId -> m Derive.Result
uncachedDerive = PlayUtil.uncachedDerive

inverseTempoFunc :: Cmd.M m => RealTime
    -> m [(BlockId, [(TrackId, ScoreTime)])]
inverseTempoFunc time = do
    perf <- get =<< Cmd.getFocusedBlock
    pure $ TrackWarp.inverseTempoFunc (Cmd.perfWarps perf)
        Transport.StopAtEnd time

-- * block

-- | Get this block's performance from the cache.
blockEvents :: Cmd.M m => BlockId -> m [LEvent.LEvent Score.Event]
blockEvents = fmap normalizeEvents . blockEventsUnnormalized

-- | 'normalize_events' is important for display, but I can't do it if I'm
-- going to pass to 'convert', because convert should do the normalization
-- itself.
blockEventsUnnormalized :: Cmd.M m => BlockId -> m [LEvent.LEvent Score.Event]
blockEventsUnnormalized blockId =
    Stream.toList . Derive.rEvents <$> PlayUtil.cachedDerive blockId

blockUncachedEvents :: Cmd.M m => BlockId -> m [LEvent.LEvent Score.Event]
blockUncachedEvents blockId = normalizeEvents . Stream.toList
    . Derive.rEvents <$> uncachedDerive blockId

-- | Apply 'Score.normalize' to the events, so that they have their final
-- control positions and pitches.  Normally 'convert' does this, but if you
-- display it before convert it's nice to see the \"cooked\" versions.
normalizeEvents :: [LEvent.LEvent Score.Event] -> [LEvent.LEvent Score.Event]
normalizeEvents = map (fmap Score.normalize)

-- | Get 'block_events' from the cache and convert to MIDI performer events.
blockMidiEvents :: Cmd.M m => BlockId -> m [LEvent.LEvent Types.Event]
blockMidiEvents blockId =
    midiConvert . LEvent.eventsOf =<< blockEventsUnnormalized blockId

-- | Derive all the way to MIDI.  This uses the cache.
blockMidi :: Cmd.M m => BlockId -> m Perform.MidiEvents
blockMidi blockId = do
    state <- Ui.get
    perf <- Performance.performance state <$> PlayUtil.cachedDerive blockId
    PlayUtil.performRaw $ Vector.toList $ Cmd.perfEvents perf

-- * selection

-- | Derive the current block from the cache and return events that fall within
-- the current selection.
selEvents :: Cmd.M m => m [Score.Event]
selEvents = getSelEvents Local blockEvents

selMidiEvents :: Cmd.M m => m [LEvent.LEvent Types.Event]
selMidiEvents =
    midiConvert =<< getSelEvents Local blockEventsUnnormalized

selImEvents :: Cmd.M m => m [LEvent.LEvent Shared.Note.Note]
selImEvents = do
    blockId <- Cmd.getFocusedBlock
    imConvert blockId =<< getSelEvents Local blockEventsUnnormalized

selScEvents :: Cmd.M m => m [LEvent.LEvent Sc.Note.Note]
selScEvents = scConvert =<< getSelEvents Local blockEventsUnnormalized

-- | Show the low level events as seen by the sampler backend.
selSamplerEvents :: Source -> Cmd.CmdT IO Text
selSamplerEvents source = do
    (blockId, trackIds, start, end) <- getSelRanges source
    imDir <- Cmd.gets $ Shared.Config.imDir . Cmd.configIm . Cmd.stateConfig
    scorePath <- Cmd.gets Cmd.scorePath
    let notesFile = Shared.Config.notesFilename imDir scorePath blockId
            Shared.Config.sampler
    let args =
            [ "dump"
            , "--range", show start <> "," <> show end
            , "--tracks", untxt $
                Text.intercalate "," (map Id.identText trackIds)
            , notesFile
            ]
    out <- CmdUtil.readProcess "build/opt/sampler-im" args
    pure $ Text.unwords ("%" : "build/opt/sampler-im" : map txt args) <> "\n"
        <> out

-- | Like 'sel_events' but take the root derivation.
rootSelEvents :: Cmd.M m => m [Score.Event]
rootSelEvents = getSelEvents Root blockEvents

rootSelMidiEvents :: Cmd.M m => m [LEvent.LEvent Types.Event]
rootSelMidiEvents =
    midiConvert =<< getSelEvents Root blockEventsUnnormalized

rootSelImEvents :: Cmd.M m => m [LEvent.LEvent Shared.Note.Note]
rootSelImEvents = do
    blockId <- Ui.getRootId
    imConvert blockId =<< getSelEvents Root blockEventsUnnormalized

-- ** extract

-- | Get logged events with the given tag and instruments.
extractInsts :: Cmd.M m => BlockId -> Text -> [Text]
    -> m [(Text, [Score.Event])]
extractInsts blockId tag insts =
    map (fmap (withInsts insts . stripStack)) <$> extractDebug blockId tag

-- | Extract events logged via the @debug@ call.
extractDebug :: Cmd.M m => BlockId -> Text -> m [(Text, [Score.Event])]
    -- ^ logged events by the stack where they were logged
extractDebug blockId tag = do
    logs <- LEvent.logsOf <$> blockEvents blockId
    pure
        [ (stack log, events)
        | (log, Just events) <- zip logs (map (Log.lookupDyn tag) logs)
        ]
    where
    stack = maybe "" Stack.prettyUi . Log.msgStack

control :: ScoreT.Control -> Score.Event -> Maybe (ScoreT.Typed Signal.Y)
control c e = Score.controlAt (Score.eventStart e) c e

eventControls :: Score.Event -> ScoreT.ControlValMap
eventControls e = Score.eventControlsAt (Score.eventStart e) e

onlyKeys :: [Env.Key] -> [LEvent.LEvent Score.Event] -> [Score.Event]
onlyKeys keys = map strip . LEvent.eventsOf
    where
    strip = Score.modifyEnviron $
        Env.fromMap . flip Map.restrictKeys (Set.fromList keys) . Env.toMap

withInsts :: [Text] -> [Score.Event] -> [Score.Event]
withInsts instruments = filter ((`elem` is) . Score.eventInstrument)
    where is = map Util.instrument instruments

stripStack :: [Score.Event] -> [Score.Event]
stripStack = map $ \event -> event { Score.eventStack = Stack.empty }

stripEnv :: [Score.Event] -> [Score.Event]
stripEnv = map $ \event -> event { Score.eventEnviron = mempty }

-- | Pretty-print events, presumably from 'sel_events'.  Extract the given
-- fields, and format them in columns.
--
-- There are a set of e_* functions designed for this.  E.g.
--
-- > LPerf.sel_events $> LPerf.e [LPerf.e_sp, LPerf.e_attr]
e :: [Fmt] -> [Score.Event] -> Text
e extract = Text.unlines . Texts.columns 1
    . map (\event -> map ($event) (concat extract))

-- | LPerf.en LPerf.sel_events
en :: Functor f => f [Score.Event] -> f Text
en = fmap (e [eNote])

type Fmt = [Score.Event -> Text]

-- Start, dur, text.
eSdt, eSd, eSp, eS :: Fmt
eSdt = eSd ++ [pretty . Score.eventText]
eSd = [pretty . Score.eventStart, pretty . Score.eventDuration]
eSp = eS ++ ePitch
eS = [pretty . Score.eventStart]

eNote :: Fmt
eNote = eSd ++ ePitch ++ eAttr

ePitch, eAttr, eInst, eEnv :: Fmt
ePitch = [maybe "?" PSignal.symbolicPitch . Score.initialPitch]
eAttr = [pretty . Score.eventAttributes]
eInst = [pretty . Score.eventInstrument]
eEnv = [pretty . Score.eventEnviron]

eEnvLike, eEnvK :: Text -> Fmt
eEnvLike key = [pretty . filter ((key `Text.isInfixOf`) . fst) . Env.toList
    . Score.eventEnviron]
eEnvK key = [pretty . Env.lookup key . Score.eventEnviron]

-- * play from

eventsFrom :: Cmd.M m => m (Vector.Vector Score.Event)
eventsFrom = do
    (blockId, _, trackId, pos) <- Selection.getInsert
    perf <- Cmd.getPerformance blockId
    start <- Perf.getRealtime perf blockId (Just trackId) pos
    pure $ snd $ PlayUtil.eventsFrom mempty start (Cmd.perfEvents perf)

performFrom :: Cmd.M m => m (Perform.MidiEvents, [LEvent.LEvent Sc.Note.Note])
performFrom = do
    (blockId, _, trackId, pos) <- Selection.getInsert
    perf <- Cmd.getPerformance blockId
    start <- Perf.getRealtime perf blockId (Just trackId) pos
    PlayUtil.performFrom start (Cmd.perfEvents perf)

-- ** implementation

-- | Like 'get_sel_events_logs', but filter out the LEvent.Logs.
getSelEvents :: Cmd.M m => Source
    -> (BlockId -> m [LEvent.LEvent Score.Event]) -> m [Score.Event]
getSelEvents source =
    LEvent.writeLogs <=< getSel Score.eventStart Score.eventStack source


-- | Get events derived in the selected range.
getSelEventsLogs :: Cmd.M m => Source
    -> (BlockId -> m [LEvent.LEvent Score.Event]) -- ^ derive events in the
    -- given block, e.g. via 'block_events' or 'block_events_unnormalized'
    -> m [LEvent.LEvent Score.Event]
getSelEventsLogs = getSel Score.eventStart Score.eventStack

getSel :: Cmd.M m => (d -> RealTime) -> (d -> Stack.Stack)
    -> Source -> (BlockId -> m [LEvent.LEvent d]) -> m [LEvent.LEvent d]
getSel eventStart eventStack source deriveEvents = do
    (blockId, trackIds, start, end) <- getSelRanges source
    events <- deriveEvents blockId
    pure $ inTracks eventStack trackIds $
        inRange eventStart start end events

getSelRanges :: Cmd.M m => Source
    -> m (BlockId, [TrackId], RealTime, RealTime)
getSelRanges source = do
    (blockId, start, end) <- case source of
        Root -> Selection.realtime
        Local -> Selection.localRealtime
    trackIds <- Selection.trackIds
    pure (blockId, trackIds, start, end)

scoreInSelection :: [TrackId] -> RealTime -> RealTime
    -> [LEvent.LEvent Score.Event] -> [LEvent.LEvent Score.Event]
scoreInSelection trackIds start end =
    inTracks Score.eventStack trackIds
    . inRange Score.eventStart start end

inTracks :: (d -> Stack.Stack) -> [TrackId] -> [LEvent.LEvent d]
    -> [LEvent.LEvent d]
inTracks eventStack trackIds =
    filter $ LEvent.logOr (has . tracksOf . eventStack)
    where
    tracksOf = mapMaybe Stack.trackOf . Stack.innermost
    has tids = any (`elem` tids) trackIds

inRange :: (a -> RealTime) -> RealTime -> RealTime -> [LEvent.LEvent a]
    -> [LEvent.LEvent a]
inRange startOf start end =
    LEvent.takeWhile ((<end) . startOf)
    . LEvent.dropWhile (before start . startOf)
    where
    before start ts
        -- I can't put a selection before 0, so assume that means I want
        -- everything before 0 too.
        | start == 0 = False
        | otherwise = ts < start

-- | Like 'in_range', but use the stack to check for ScoreTime range.
inScoreRange :: (a -> Stack.Stack) -> [BlockId] -> [TrackId]
    -> ScoreTime -> ScoreTime -> [LEvent.LEvent a] -> [LEvent.LEvent a]
inScoreRange stackOf blockIds trackIds start end = filter $ LEvent.logOr $
    stackInScoreRange blockIds trackIds start end . stackOf

stackInScoreRange :: [BlockId] -> [TrackId] -> ScoreTime -> ScoreTime
    -> Stack.Stack -> Bool
stackInScoreRange blockIds trackIds start end = any match . Stack.toUi
    where
    match (Just blockId, Just trackId, Just (s, e)) =
        blockId `elem` blockIds && trackId `elem` trackIds
        && not (end <= s || start >= e)
    match _ = False

-- * Midi.Types.Event

midiConvert :: Cmd.M m => [Score.Event] -> m [LEvent.LEvent Types.Event]
midiConvert events = do
    lookupInst <- Cmd.getLookupInstrument
    let lookup = PlayUtil.makeMidiLookup lookupInst
    pure $ Midi.Convert.convert Midi.Convert.defaultSrate lookup lookupInst
        events

imConvert :: Cmd.M m => BlockId -> [Score.Event]
    -> m [LEvent.LEvent Shared.Note.Note]
imConvert blockId events = do
    lookupInst <- Cmd.getLookupInstrument
    pure $ Im.Convert.convert blockId lookupInst events

scConvert :: Cmd.M m => [Score.Event] -> m [LEvent.LEvent Sc.Note.Note]
scConvert events = do
    lookupInst <- Cmd.getLookupInstrument
    pure $ Sc.Convert.convert Sc.Convert.defaultSrate lookupInst events

-- | Filter on events with a certain instrument.
midiEventInst :: Types.Event -> Text
midiEventInst = ScoreT.instrumentName . Types.patchName . Types.eventPatch

-- * midi

performEvents :: Cmd.M m => [LEvent.LEvent Score.Event] -> m Perform.MidiEvents
performEvents = PlayUtil.performRaw . LEvent.eventsOf

performMidiEvents :: Ui.M m => [LEvent.LEvent Types.Event]
    -> m Perform.MidiEvents
performMidiEvents events = do
    allocs <- Ui.gets $ UiConfig.configAllocations . Ui.stateConfig
    let midiAllocs = Perform.config <$> PlayUtil.midiConfigs allocs
    pure $ fst $ Perform.perform Perform.initialState midiAllocs events

-- | This is the local block's performance, and the events are filtered to the
-- selection range, and the filtering is done post-derivation, so they reflect
-- what would actually be played.  I can't filter by selected track because
-- MIDI events don't retain the stack.
selMidi :: Cmd.M m => m Perform.MidiEvents
selMidi = getSelMidi Local

-- | Analyze a section of MIDI for debugging.
analyzeMidi :: [LEvent.LEvent Midi.WriteMessage] -> Text
analyzeMidi =
    Synth.prettyState . Synth.run Synth.emptyState . LEvent.eventsOf

rootSelMidi :: Cmd.M m => m Perform.MidiEvents
rootSelMidi = getSelMidi Root

getSelMidi :: Cmd.M m => Source -> m Perform.MidiEvents
getSelMidi source = do
    (blockId, start, end) <- case source of
        Root -> Selection.realtime
        Local -> Selection.localRealtime
    events <- blockMidi blockId
    pure $ inRange Midi.wmsgTs start end events

-- | Get all logs whose 'Log.msg_text' matches a regex.
logsLike :: Cmd.M m => BlockId -> String -> m [Log.Msg]
logsLike blockId regex = do
    logs <- LEvent.logsOf <$> blockMidi blockId
    let reg = Regex.compileUnsafe regex
    pure $ filter (Regex.matches reg . Log.msgText) logs

-- | Get logs that include a stack frame that matches the given block, tracks,
-- and range.
logsMatching :: Cmd.M m => BlockId -> BlockId -> [TrackId] -> TrackTime
    -> TrackTime -> m [Log.Msg]
logsMatching perfBlock blockId trackIds start end = do
    logs <- LEvent.logsOf <$> blockMidi perfBlock
    let pattern = (Just blockId, Just (Set.fromList trackIds),
            Just (start, end))
        match = maybe False (Stack.match pattern) . Log.msgStack
    pure $ filter match logs

playMidi :: Cmd.M m => Perform.MidiEvents -> m ()
playMidi msgs = Cmd.modify $ \st -> st { Cmd.stateReplStatus = status }
    where
    toZero msgs = PlayUtil.shiftMidi 1 (PlayUtil.firstTime msgs) msgs
    status = Cmd.Play $ Cmd.PlayArgs
        { playSync = Nothing
        , playName = "repl"
        , playMidi = toZero msgs
        , playSc = Sc.Note.PlayNotes { shift = 0, stretch = 1, notes =  [] }
        , playInvTempo = Nothing
        , playRepeatAt = Nothing
        , playImEnd = Nothing
        , playImDirect = Nothing
        }

-- ** extract

withChans :: [Midi.Channel] -> [Midi.WriteMessage] -> [Midi.WriteMessage]
withChans chans = filter $ (`elem` map Just chans) . Midi.messageChannel
    . Midi.wmsgMsg

-- | Reduce MIDI to an easier to read form.
simpleMidi :: [Midi.WriteMessage] -> [(RealTime, Midi.Message)]
simpleMidi = map $ \wmsg -> (Midi.wmsgTs wmsg, Midi.wmsgMsg wmsg)


-- ** cache contents

getCache :: Cmd.M m => BlockId -> m (Map Derive.CacheKey Derive.Cached)
getCache blockId = do
    Derive.Cache cache <- Cmd.perfDeriveCache <$>
        Cmd.getPerformance blockId
    pure cache

getCacheEvents :: (Cache.Cacheable d, Cmd.M m) => BlockId
    -> m (Map Derive.CacheKey [LEvent.LEvent d])
getCacheEvents blockId = do
    cache <- getCache blockId
    pure $ fmap Stream.toList $ Map.mapMaybe get cache
    where
    get Derive.Invalid = Nothing
    get (Derive.Cached c) = case Cache.fromCacheEntry c of
        Nothing -> Nothing
        Just (Derive.CallType _ events) -> Just events

showCache :: Cmd.M m => BlockId -> m Text
showCache blockId = do
    perf <- Cmd.getPerformance blockId
    pure $ Cache.prettyCache (Cmd.perfDeriveCache perf)

entryEvents :: Derive.CacheEntry -> Int
entryEvents entry = case entry of
    Derive.CachedEvents (Derive.CallType _ events) -> Stream.length events
    Derive.CachedControl (Derive.CallType _ events) -> Stream.length events
    Derive.CachedPitch (Derive.CallType _ events) -> Stream.length events


-- * pitches

type Ratio = Ratio.Rational

-- | A hook for 'Cmd.hooks_selection'.
chordHook :: Cmd.M m => [(ViewId, Maybe Cmd.TrackSelection)] -> m ()
chordHook = mapM_ (uncurry setChordStatus)

-- | Show chord ratios at current selection.
chord :: Cmd.M m => m Text
chord = do
    (viewId, sel) <- Selection.getViewSel
    blockId <- Ui.blockIdOf viewId
    maybeTrackId <- Ui.eventTrackAt blockId (Selection.selPointTrack sel)
    showChord <$> chordAt blockId maybeTrackId (Selection.selPoint sel)

showChord :: [(Pitch.NoteNumber, Pitch.Note, Ratio)] -> Text
showChord = Text.intercalate ", " . map pretty
    where
    pretty (_, note, ratio) = Pitch.noteText note <> ":" <> showRatio ratio

showRatio :: Ratio -> Text
showRatio = go 0
    where
    go oct ratio
        | ratio >= 2 = go (oct+1) (ratio/2)
        | ratio <= 1%2 = go (oct-1) (ratio*2)
        | oct == 0 = pretty ratio
        | otherwise = showt oct <> "+" <> pretty ratio
    pretty ratio =
        showt (Ratio.numerator ratio) <> "/" <> showt (Ratio.denominator ratio)

setChordStatus :: Cmd.M m => ViewId -> Maybe Cmd.TrackSelection -> m ()
setChordStatus viewId maybeSel = case maybeSel of
    Nothing -> set Nothing
    Just (sel, blockId, maybeTrackId) ->
        set . Just . showChord
            =<< chordAt blockId maybeTrackId (Selection.selPoint sel)
    where set = Cmd.setViewStatus viewId Config.statusChord

-- | Show the ratios of the frequencies of the notes at the time of the current
-- selection.  They are sorted by their track-order, and the track with the
-- selection is considered unity.
chordAt :: Cmd.M m => BlockId -> Maybe TrackId -> ScoreTime
    -> m [(Pitch.NoteNumber, Pitch.Note, Ratio)]
chordAt blockId maybeTrackId pos = do
    perf <- Perf.getRoot
    pos <- Perf.getRealtime perf blockId maybeTrackId pos
    events <- PlayUtil.overlappingEvents pos . Cmd.perfEvents <$> get blockId
    events <- sortByTrack blockId events
    let selected = do
            trackId <- maybeTrackId
            List.find (onTrack trackId) events
    let nns = mapMaybe (Score.nnAt pos) events
        notes = mapMaybe (Score.noteAt pos) events
    pure $ List.zip3 nns notes (nnRatios (Score.nnAt pos =<< selected) nns)
    where
    onTrack trackId = (== Just trackId) . fmap snd . Stack.blockTrackOf
        . Score.eventStack

-- | Sort events by the tracknum of the tracks they fall on.  Filter out
-- events that don't directly originate from a track on the given block.
--
-- TODO I should look for the block anywhere in the stack, and sort it by the
-- corresponding track.
sortByTrack :: Ui.M m => BlockId -> [Score.Event] -> m [Score.Event]
sortByTrack blockId events = do
    let byTrack = Lists.keyOnJust
            (fmap snd . Stack.blockTrackOf . Score.eventStack) events
    tracknums <- mapM (Ui.tracknumOf blockId . fst) byTrack
    let byTracknum = [(tracknum, event)
            | (Just tracknum, event) <- zip tracknums (map snd byTrack)]
    pure $ map snd $ Lists.sortOn fst byTracknum

nnRatios :: Maybe Pitch.NoteNumber -> [Pitch.NoteNumber] -> [Ratio]
nnRatios unity nns = case (unity, nns) of
    (Just unity, nns) -> ratios unity nns
    (Nothing, nn : nns) -> ratios nn nns
    (_, []) -> []
    where
    ratios unity = map (flip Ratio.approxRational 0.01 . (/ hz unity) . hz)
    hz = Pitch.nnToHz

overlappingEvents :: Cmd.M m => Source -> m (RealTime, [Score.Event])
overlappingEvents source = do
    (blockId, start, _) <- case source of
        Root -> Selection.realtime
        Local -> Selection.localRealtime
    events <- PlayUtil.overlappingEvents start . Cmd.perfEvents <$>
        get blockId
    pure (start, events)
