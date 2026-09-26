-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Functions to do with performance.  This is split off from "Cmd.Play",
-- which contains play Cmds and their direct support.
module Cmd.PlayUtil (
    initialEnviron
    , cachedDerive, uncachedDerive
    , deriveBlock, run
    , evalWithDynamic, runWithConstant
    , isScoreDamageLog
    , getConstant, initialConstant, initialDynamic
    -- * perform
    , performFrom
    , performRaw
    , shiftMidi
    , firstTime
    , eventsFrom, overlappingEvents
    , makeMidiLookup
    , midiConfigs
    -- * mute and solo
    , getMutedTracks, mutedInstruments
) where
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import qualified Util.Log as Log
import qualified Util.Trees as Trees
import qualified Util.Vector

import qualified App.Config as Config
import qualified Cmd.Cmd as Cmd
import qualified Derive.C.Prelude.Block as Prelude.Block
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.LEvent as LEvent
import qualified Derive.Scale.All2 as Scale.All2
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.Stack as Stack

import qualified Instrument.Common as Common
import qualified Midi.Midi as Midi
import qualified Perform.Midi.Convert as Convert
import qualified Perform.Midi.Patch as Patch
import qualified Perform.Midi.Perform as Perform
import qualified Perform.RealTime as RealTime
import qualified Perform.Sc.Convert as Sc.Convert
import qualified Perform.Sc.Note as Sc.Note

import qualified Ui.Block as Block
import qualified Ui.TrackTree as TrackTree
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig

import           Global
import           Types


-- | There are a few environ values that almost everything relies on.
initialEnviron :: Env.Environ
initialEnviron = Env.fromList
    -- Control interpolators rely on this.
    -- TODO 66+2/3, change to a whole number when I'm ready to update verify
    [ (EnvKey.srate, DeriveT.num (1/0.015))
    -- Looking up any val call relies on having a scale in scope.
    , (EnvKey.scale, DeriveT.str Config.defaultScaleId)
    , (EnvKey.attributes, DeriveT.VAttributes mempty)
    , (EnvKey.seed, DeriveT.num 0)
    ]

-- | Derive with the cache.
cachedDerive :: Cmd.M m => BlockId -> m Derive.Result
cachedDerive blockId = do
    maybePerf <- Cmd.lookupPerformance blockId
    case maybePerf of
        Nothing -> uncachedDerive blockId
        Just perf -> deriveBlock (Cmd.perfDeriveCache perf)
            (Cmd.perfDamage perf) blockId

uncachedDerive :: Cmd.M m => BlockId -> m Derive.Result
uncachedDerive = deriveBlock mempty mempty

-- | Derive the contents of the given block to score events.
deriveBlock :: Cmd.M m => Derive.Cache -> Derive.ScoreDamage -> BlockId
    -> m Derive.Result
deriveBlock cache damage blockId =
    fmap Derive.extractResult $ run cache damage $ do
        unless (damage == mempty) $
            Log.debug $ "score damage for " <> showt blockId <> ": "
                <> pretty damage
        Prelude.Block.evalRootBlock blockId

isScoreDamageLog :: Log.Msg -> Bool
isScoreDamageLog = ("score damage for " `Text.isPrefixOf`) . Log.msgText

run :: Cmd.M m => Derive.Cache -> Derive.ScoreDamage -> Derive.Deriver a
    -> m (Derive.RunResult a)
run cache damage deriver = do
    uiState <- Ui.get
    (constant, aliases) <- getConstant uiState cache damage
    pure $ Derive.derive constant (initialDynamic aliases) deriver

-- | 'run_with_dynamic' but extract just the value.
evalWithDynamic :: Cmd.M m => Derive.Dynamic -> Derive.Deriver a -> m a
evalWithDynamic dynamic deriver = do
    (errA, _, logs) <- runWithDynamic dynamic deriver
    mapM_ Log.write logs
    Cmd.requireRight pretty errA

-- | Run a derivation when you already know the Dynamic.  This is the case when
-- deriving at a certain point in the score via the TrackDynamic.
runWithDynamic :: Cmd.M m => Derive.Dynamic -> Derive.Deriver a
    -> m (Derive.RunResult a)
runWithDynamic dynamic deriver = do
    uiState <- Ui.get
    -- Trust they already put the ky aliases in.
    (constant, _aliases) <- getConstant uiState mempty mempty
    pure $ runWithConstant constant dynamic deriver

runWithConstant :: Derive.Constant -> Derive.Dynamic
    -> Derive.Deriver a -> Derive.RunResult a
runWithConstant constant dynamic = Derive.run state
    where
    state = Derive.State
        { stateThreaded = Derive.initialThreaded
        , stateDynamic = dynamic
        , stateCollect = mempty
        , stateConstant = constant
        }

-- | Create deriver configuration.  This is the main place where Cmd level
-- configuration is adapted to the deriver.
getConstant :: Cmd.M m => Ui.State -> Derive.Cache -> Derive.ScoreDamage
    -> m (Derive.Constant, Derive.InstrumentAliases)
getConstant uiState cache damage = do
    cmdState <- Cmd.get
    (builtins, aliases) <- case Cmd.stateKyCache cmdState of
        Nothing -> pure (mempty, mempty)
        Just kyCache -> Cmd.requireRight id $ kyBuiltins kyCache
    pure
        ( initialConstant uiState (Cmd.stateConfig cmdState)
            builtins cache damage
        , aliases
        )

initialConstant :: Ui.State -> Cmd.Config -> Derive.Builtins -> Derive.Cache
    -> Derive.ScoreDamage -> Derive.Constant
initialConstant uiState cmdConfig builtins cache damage =
    Derive.initialConstant uiState
        (builtins <> configBuiltins) Cmd.lookupScale Scale.All2.scales
        (fmap Cmd.makeDeriveInstrument . lookupInst) cache damage
    where
    lookupInst = Cmd.memoizedInstrument
        (UiConfig.configAllocations (Ui.stateConfig uiState))
        (Cmd.configInstrumentDb cmdConfig)
    configBuiltins = Cmd.configBuiltins cmdConfig

kyBuiltins :: Cmd.KyCache
    -> Either Cmd.Error (Derive.Builtins, Derive.InstrumentAliases)
kyBuiltins = \case
    Cmd.KyCache (Left err) _ -> Left $ "parsing ky: " <> err
    Cmd.KyCache (Right builtins) _ -> Right builtins
    Cmd.PermanentKy builtins -> Right builtins

initialDynamic :: Derive.InstrumentAliases -> Derive.Dynamic
initialDynamic aliases = (Derive.initialDynamic initialEnviron)
    { Derive.stateInstrumentAliases = aliases }

performFrom :: Cmd.M m => RealTime -> Vector.Vector Score.Event
    -> m (Perform.MidiEvents, Sc.Note.Notes)
performFrom start events = do
    allocs <- Ui.gets $ UiConfig.configAllocations . Ui.stateConfig
    lookupInst <- Cmd.getLookupInstrument
    let insts = Map.keys $ UiConfig.unallocations allocs
    let resumeInsts = Set.fromList $
            filter (hasFlag lookupInst Patch.ResumePlay) insts
    -- TODO no ResumePlay for SC, but why not?
    (extra, events) <- pure $ eventsFrom resumeInsts start events
    muted <- getMutedTracks
    -- Performance should be lazy, so converting to a list here means I can
    -- avoid doing work for the notes that never get played.
    let eventsList = filterTrackMuted muted $
            filterInstrumentMuted allocs $
            extra ++ Vector.toList events
    pure
        ( performMidi lookupInst allocs eventsList
        , performSc lookupInst allocs eventsList
        )
    where
    hasFlag lookupInst flag inst =
        case Cmd.midiPatch =<< lookupInst inst of
            Nothing -> False
            Just (_, config) -> Patch.hasFlag config flag

-- | Perform midi only, from time 0, without mutes.
performRaw :: Cmd.M m => [Score.Event] -> m Perform.MidiEvents
performRaw events = do
    allocs <- Ui.gets $ UiConfig.configAllocations . Ui.stateConfig
    lookupInst <- Cmd.getLookupInstrument
    pure $ performMidi lookupInst allocs events

performMidi :: (ScoreT.Instrument -> Maybe Cmd.ResolvedInstrument)
    -> UiConfig.Allocations -> [Score.Event] -> Perform.MidiEvents
performMidi lookupInst allocs events
    | UiConfig.hasMidi allocs =
        fst $ Perform.perform Perform.initialState configs $
            Convert.convert Convert.defaultSrate midiLookup lookupInst events
    | otherwise = mempty
    where
    configs = Perform.config <$> midiConfigs allocs
    midiLookup = makeMidiLookup lookupInst

shiftMidi :: RealTime -> RealTime -> Perform.MidiEvents
    -> Perform.MidiEvents
shiftMidi multiplier start events = shift start events
    where
    shift offset = map $ fmap $
        Midi.modifyTimestamp ((* multiplier) . subtract offset)

performSc :: (ScoreT.Instrument -> Maybe Cmd.ResolvedInstrument)
    -> UiConfig.Allocations -> [Score.Event] -> Sc.Note.Notes
performSc lookupInst allocs
    | UiConfig.hasSc allocs =
        Sc.Convert.convert Sc.Convert.defaultSrate lookupInst
    | otherwise = mempty

-- | The first timestamp from the msgs.
firstTime :: [LEvent.LEvent Midi.WriteMessage] -> RealTime
firstTime msgs = case LEvent.eventsOf msgs of
    event : _ -> Midi.wmsgTs event
    [] -> 0

-- | As a special case, a start <= 0 will get all events, including negative
-- ones.  This is so notes pushed before 0 won't be clipped on a play from 0.
eventsFrom :: Set ScoreT.Instrument -- ^ scan back for starts of these
    -> RealTime -> Vector.Vector Score.Event
    -> ([Score.Event], Vector.Vector Score.Event)
    -- ^ (extra events before start, events from start)
eventsFrom resumeInsts start events
    | start <= 0 = ([], events)
    | otherwise = (starts, Vector.drop index events)
    where
    index = Util.Vector.lowestIndex Score.eventStart (start - RealTime.eta)
        events
    starts = scanForStarts defaultScanBack resumeInsts events start index

-- | Look back from the play start time by this much.  This is convenient
-- because randomization can move events back from the ruler mark, and it's
-- annoying when play misses them.
defaultScanBack :: RealTime
defaultScanBack = 0.075

-- | Starting from the index, look back for overlapping events in the given set.
scanForStarts :: RealTime -> Set ScoreT.Instrument
    -> Vector.Vector Score.Event -> RealTime -> Int -> [Score.Event]
scanForStarts scanBack resumeInsts events pos index =
    reverse $ mapMaybe (setStart scanBack pos) $
        scan (resumeInsts `Set.difference` presentHere) back
    where
    here = Vector.takeWhile ((==pos) . Score.eventStart) $
        Vector.drop index events
    presentHere = Vector.foldl' (\s e -> Set.insert (inst e) s) mempty here
    back = Util.Vector.toReverseList $ Vector.take index events
    scan _ [] = []
    scan !insts (e:es)
        | Score.eventStart e >= until = e : scan insts2 es
        | Set.null insts = []
        | inst e `Set.member` insts = e : scan insts2 es
        | otherwise = scan insts es
        where insts2 = Set.delete (inst e) insts
    until = pos - scanBack
    inst = Score.eventInstrument

setStart :: RealTime -> RealTime -> Score.Event -> Maybe Score.Event
setStart scanBack pos event
    | Score.eventStart event >= pos - scanBack = Just event
    | dur <= 0 = Nothing
    | otherwise =
        Just $ event { Score.eventStart = pos, Score.eventDuration = dur }
    where dur = Score.eventEnd event - pos

-- | How to know how far back to go?  Impossible to know!  Well, I could look
-- up overlapping ui events, then map the earliest time to RealTime, and start
-- searching there.  But for now scanning from the beginning should be fast
-- enough.
overlappingEvents :: RealTime -> Vector.Vector Score.Event -> [Score.Event]
overlappingEvents pos = Vector.foldl' collect []
    where
    collect overlap event
        | Score.eventEnd event <= pos || Score.eventStart event > pos =
            overlap
        | otherwise = event : overlap

-- | Similar to the Solo and Mute track flags, individual instruments can be
-- soloed or muted.
filterInstrumentMuted :: UiConfig.Allocations -> [Score.Event]
    -> [Score.Event]
filterInstrumentMuted allocs =
    filter ((`Set.notMember` muted) . Score.eventInstrument)
    where muted = mutedInstruments allocs

mutedInstruments :: UiConfig.Allocations -> Set ScoreT.Instrument
mutedInstruments (UiConfig.Allocations allocs)
    | not (null soloed) = instruments Set.\\ Set.fromList soloed
    | otherwise = Set.fromList muted
    where
    configs = map (second UiConfig.allocConfig) (Map.toList allocs)
    instruments = Set.fromList $ map fst configs
    soloed = map fst $ filter (Common.configSolo . snd) configs
    muted = map fst $ filter (Common.configMute . snd) configs

-- | Filter events according to the Solo and Mute flags in the tracks of the
-- given blocks.
--
-- Solo only applies to the block on which the track is soloed.  So if you solo
-- a track on one block, other blocks will still play.
--
-- Solo takes priority over Mute.
filterTrackMuted :: Set TrackId -> [Score.Event] -> [Score.Event]
filterTrackMuted muted
    | Set.null muted = id
    | otherwise = filter (not . stackContains muted)
    where
    stackContains trackIds = any (`Set.member` trackIds) . stackTracks
    stackTracks = mapMaybe Stack.trackOf . Stack.innermost . Score.eventStack

getMutedTracks :: Ui.M m => m (Set TrackId)
getMutedTracks = do
    blocks <- Ui.gets (Map.toList . Ui.stateBlocks)
    tree <- concat <$> mapM (TrackTree.trackTreeOf . fst) blocks
    pure $ mutedTracks tree blocks

mutedTracks :: TrackTree.TrackTree -> [(BlockId, Block.Block)] -> Set TrackId
mutedTracks tree blocks
    | not (Set.null soloed) = soloToMute tree blocks soloed
    | otherwise = muted
    where
    soloed = withFlag Block.Solo
    muted = withFlag Block.Mute
    withFlag flag = Set.fromList
        [ trackId
        | (_, block) <- blocks
        , track <- Block.blockTracks block
        , Just trackId <- [Block.trackId track]
        , flag `Set.member` Block.trackFlags track
        ]

-- | Solo is surprisingly tricky.  Solo means non soloed-tracks are muted,
-- unless there is no solo on the block, or the track is the parent or child of
-- a soloed track.
--
-- I've already rewritten this a bunch of times, hopefully this is the last
-- time.
soloToMute :: TrackTree.TrackTree -- ^ All the trees of the whole score,
    -- concatenated.  This is because I just need to know who is a child of
    -- who, and I don't care what block they're in.
    -> [(BlockId, Block.Block)] -> Set TrackId -> Set TrackId
soloToMute tree blocks soloed = Set.fromList
    [ trackId
    | (blockId, block) <- blocks
    , track <- Block.blockTracks block
    , Just trackId <- [Block.trackId track]
    , trackId `Set.notMember` soloed
    , blockId `Set.member` soloedBlocks
    , trackId `Set.notMember` hasSoloedRelatives
    ]
    where
    hasSoloedRelatives = Set.fromList (mapMaybe get (Trees.flatPaths tree))
        where
        get (track, parents, children)
            | any (`Set.member` soloed) (map Ui.trackId children)
                    || any (`Set.member` soloed) (map Ui.trackId parents) =
                Just (Ui.trackId track)
            | otherwise = Nothing
    soloedBlocks = Set.fromList
        [ blockId
        | (blockId, block) <- blocks
        , any ((Block.Solo `Set.member`) . Block.trackFlags)
            (Block.blockTracks block)
        ]

midiConfigs :: UiConfig.Allocations -> Map ScoreT.Instrument Patch.Config
midiConfigs (UiConfig.Allocations allocs) = Map.fromAscList
    [ (inst, config)
    | (inst, alloc) <- Map.toAscList allocs
    , Just config <- [midiConfig (UiConfig.allocBackend alloc)]
    ]

midiConfig :: UiConfig.Backend -> Maybe Patch.Config
midiConfig (UiConfig.Midi a) = Just a
midiConfig _ = Nothing

makeMidiLookup :: (ScoreT.Instrument -> Maybe Cmd.ResolvedInstrument)
    -> Convert.MidiLookup
makeMidiLookup lookupInst = Convert.MidiLookup
    { lookupScale = Cmd.lookupScale
    , lookupControlDefaults = \inst -> fromMaybe mempty $ do
        inst <- lookupInst inst
        defaults <- case Cmd.instBackend inst of
            Cmd.Midi _ config -> Patch.configControlDefaults $
                Patch.configSettings config
            _ -> Nothing
        pure defaults
    }
