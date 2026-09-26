-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Simple Events are supposed to be easy to read, and easy to serialize to
    text and load back again.  Functions here convert them to and from text
    form, stashing converted simple blocks in the clipboard.
-}
module Cmd.Simple where
import qualified Data.Map as Map
import qualified Data.Tree as Tree

import qualified Util.Pretty as Pretty
import qualified App.Config as Config
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Selection as Selection
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.Stack as Stack

import qualified Instrument.InstT as InstT
import qualified Midi.Midi as Midi
import qualified Perform.Midi.MSignal as MSignal
import qualified Perform.Midi.Patch as Patch
import qualified Perform.Midi.Types as Midi.Types
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Id as Id
import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.Skeleton as Skeleton
import qualified Ui.Track as Track
import qualified Ui.TrackTree as TrackTree
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig

import           Global
import           Types


-- | Dump a score, or part of a score, to paste into a test.
-- (global_transform, allocations, blocks)
type State = (Text, Allocations, [Block])

-- | (id_name, title, tracks, skeleton)
type Block = (Text, Text, [Maybe Track], [Skeleton.Edge])

-- | (id_name, title, events)
type Track = (Text, Text, [Event])

-- | (start, duration, text)
type Event = (Double, Double, Text)

-- | (start, duration, text, initial_nn)
type ScoreEvent = (Double, Double, String, Maybe Pitch.NoteNumber)

-- | (inst, start, duration, initial_nn)
type PerfEvent = (String, Double, Double, Pitch.NoteNumber)

-- | (instrument, (qualified, [(device, chan)]))
--
-- [] chans means it's UiConfig.Dummy.
--
-- This doesn't include 'Patch.config_settings', so it's assumed they're the
-- same as 'Patch.patch_defaults'.
type Allocations = [(Instrument, (Qualified, Allocation))]
type Instrument = Text
type Qualified = Text
type WriteDevice = Text

data Allocation = Midi [(WriteDevice, Midi.Channel)] | Dummy | Im | Sc
    deriving (Eq, Show)

instance Pretty Allocation where
    format = \case
        Midi allocs -> Pretty.constructor "Midi" [Pretty.format allocs]
        Dummy -> "Dummy"
        Im -> "Im"
        Sc -> "Sc"

fromScore :: ScoreTime -> Double
fromScore = ScoreTime.toDouble

fromReal :: RealTime -> Double
fromReal = RealTime.toSeconds

event :: Event.Event -> Event
event e =
    (fromScore (Event.start e), fromScore (Event.duration e), Event.text e)

scoreEvent :: Score.Event -> ScoreEvent
scoreEvent evt =
    ( fromReal (Score.eventStart evt)
    , fromReal (Score.eventDuration evt)
    , untxt $ Score.eventText evt
    , Score.initialNn evt
    )

perfEvent :: Midi.Types.Event -> PerfEvent
perfEvent evt =
    ( untxt $ ScoreT.instrumentName $ Midi.Types.patchName $
        Midi.Types.eventPatch evt
    , fromReal start
    , fromReal (Midi.Types.eventDuration evt)
    , Pitch.nn (MSignal.at start (Midi.Types.eventPitch evt))
    )
    where start = Midi.Types.eventStart evt

-- * state

dumpState :: Ui.M m => m State
dumpState = do
    state <- Ui.get
    blocks <- mapM dumpBlock (Map.keys (Ui.stateBlocks state))
    pure
        ( Ui.config#UiConfig.ky #$ state
        , dumpAllocations $ Ui.config#UiConfig.allocations #$ state
        , blocks
        )

loadState :: Ui.M m => State -> m Ui.State
loadState (ky, allocs, blocks) =
    Ui.execRethrow "convert state" Ui.empty $ do
        mapM_ makeBlock blocks
        Ui.modify $ (Ui.config#UiConfig.ky #= ky)
            . (Ui.config#UiConfig.allocations #= allocations allocs)

-- * block

dumpBlock :: Ui.M m => BlockId -> m Block
dumpBlock blockId = do
    block <- Ui.getBlock blockId
    tracks <- mapM dumpTracklike (Block.blockTracklikeIds block)
    tree <- TrackTree.trackTreeOf blockId
    pure (Id.identText blockId, Block.blockTitle block, tracks,
        toSkel tree)
    where
    toSkel = concatMap go
        where
        go (Tree.Node track subs) =
            [(num track, num (Tree.rootLabel sub)) | sub <- subs]
            ++ toSkel subs
    num = Ui.trackTracknum

loadBlock :: Cmd.M m => Block -> m Ui.State
loadBlock block = Ui.execRethrow "convert block" Ui.empty $
    makeBlock block

readBlock :: FilePath -> Cmd.CmdT IO Ui.State
readBlock fn = do
    simpleBlock <- liftIO (readIO =<< readFile fn :: IO Block)
    loadBlock simpleBlock

makeBlock :: Ui.M m => Block -> m BlockId
makeBlock (idName, title, tracks, skel) = do
    tracks <- mapM loadTracklike tracks
    blockId <- Ui.createBlock (Id.readId idName) title tracks
    Ui.setSkeleton blockId (Skeleton.make skel)
    pure blockId

dumpTracklike :: Ui.M m => Block.TracklikeId -> m (Maybe Track)
dumpTracklike =
    maybe (pure Nothing) (fmap Just . dumpTrack) . Block.trackIdOf

loadTracklike :: Ui.M m => Maybe Track -> m Block.Track
loadTracklike Nothing = pure $ Block.track (Block.RId Ui.noRuler) 0
loadTracklike (Just track) = loadTrack track

-- * track

dumpTrack :: Ui.M m => TrackId -> m Track
dumpTrack trackId = do
    track <- Ui.getTrack trackId
    pure (simplifyTrack trackId track)

simplifyTrack :: TrackId -> Track.Track -> Track
simplifyTrack trackId track =
    (Id.identText trackId, Track.trackTitle track, map event events)
    where events = Events.ascending (Track.trackEvents track)

loadTrack :: Ui.M m => Track -> m Block.Track
loadTrack (idName, title, events) = do
    trackId <- Ui.createTrack (Id.readId idName) $
        Track.track title (Events.fromList (map loadEvent events))
    pure $ Block.track (Block.TId trackId Ui.noRuler) Config.trackWidth

loadEvent :: Event -> Event.Event
loadEvent (start, dur, text) =
    Event.event (ScoreTime.fromDouble start) (ScoreTime.fromDouble dur) text

dumpSelection :: Cmd.CmdL [(TrackId, [Event])]
dumpSelection = map (second (map event)) <$> Selection.events

-- * allocations

dumpAllocations :: UiConfig.Allocations -> Allocations
dumpAllocations (UiConfig.Allocations allocs) = do
    (inst, alloc) <- Map.toList allocs
    let simpleAlloc = case UiConfig.allocBackend alloc of
            UiConfig.Midi config -> Midi $ addrsOf config
            UiConfig.Im -> Im
            UiConfig.Dummy {} -> Dummy
            UiConfig.Sc -> Sc
    let qualified = InstT.showQualified $ UiConfig.allocQualified alloc
    pure (ScoreT.instrumentName inst, (qualified, simpleAlloc))
    where
    addrsOf config =
        [ (Midi.writeDeviceText dev, chan)
        | (dev, chan) <- Patch.configAddrs config
        ]

allocations :: Allocations -> UiConfig.Allocations
allocations = UiConfig.Allocations . Map.fromList . map allocation

allocation :: (Instrument, (Qualified, Allocation))
    -> (ScoreT.Instrument, UiConfig.Allocation)
allocation (inst, (qual, simpleAlloc)) =
    (ScoreT.Instrument inst, UiConfig.allocation qualified backend)
    where
    qualified = InstT.parseQualified qual
    backend = case simpleAlloc of
        Dummy -> UiConfig.Dummy ""
        Im -> UiConfig.Im
        Sc -> UiConfig.Sc
        Midi addrs -> UiConfig.Midi $ Patch.config
            [ ((Midi.writeDevice dev, chan), Nothing)
            | (dev,chan) <- addrs
            ]


-- * ExactPerfEvent

-- | Like 'PerfEvent', but is meant to recreate a 'Midi.Types.Event' exactly.
type ExactPerfEvent =
    ( Text, RealTime, RealTime, [(Text, [(RealTime, Signal.Y)])]
    , [(RealTime, Signal.Y)], (Signal.Y, Signal.Y), Stack.Stack
    )

dumpExactPerfEvent :: Midi.Types.Event -> ExactPerfEvent
dumpExactPerfEvent (Midi.Types.Event start dur patch controls pitch svel evel
        stack) =
    ( ScoreT.instrumentName (Midi.Types.patchName patch)
    , start, dur
    , map (bimap ScoreT.controlName MSignal.toPairs) (Map.toList controls)
    , MSignal.toPairs pitch
    , (svel, evel)
    , stack
    )

loadExactPerfEvent :: (InstT.Qualified -> Maybe Midi.Types.Patch)
    -> ExactPerfEvent -> Maybe Midi.Types.Event
loadExactPerfEvent lookupPatch (inst, start, dur, controls, pitch,
        (svel, evel), stack) = do
    patch <- lookupPatch (InstT.parseQualified inst)
    pure $ Midi.Types.Event
        { eventPatch = patch
        , eventStart = start
        , eventDuration = dur
        , eventControls = controlMap controls
        , eventPitch = MSignal.fromPairs pitch
        , eventStartVelocity = svel
        , eventEndVelocity = evel
        , eventStack = stack
        }

controlMap :: [(Text, [(RealTime, Signal.Y)])]
    -> Map ScoreT.Control MSignal.Signal
controlMap kvs = Map.fromList
    [(ScoreT.Control k, MSignal.fromPairs vs) | (k, vs) <- kvs]
