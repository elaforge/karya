-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{- | Utilities for "Derive.Call.Block".

    Derivation of a block is much more complicated than it might first appear.
    This can cause score code to be evaluated more times than you think it
    should be.

    One culprit is evaluating tracks for control signals for signal render.
    Under some circumstances, the track's normal control output can be directly
    reused as the signal render, but in many cases it has to be evaluated
    again.  This is further complicated by the presence of inversion and
    orphans.
-}
module Derive.Call.BlockUtil (
    noteDeriver, controlDeriver
    , captureNullControl
    , deriveTracks
    , hasTopTempoTrack

#ifdef TESTING
    , deriveTree
#endif
) where
import qualified Data.Text as Text
import qualified Data.Tree as Tree

import qualified Util.Lists as Lists
import qualified Util.Log as Log
import qualified Util.Trees as Trees

import qualified Derive.Cache as Cache
import qualified Derive.Control as Control
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.EnvKey as EnvKey
import qualified Derive.EvalTrack as EvalTrack
import qualified Derive.Expr as Expr
import qualified Derive.LEvent as LEvent
import qualified Derive.Note as Note
import qualified Derive.PSignal as PSignal
import qualified Derive.Parse as Parse
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Stack as Stack
import qualified Derive.Stream as Stream
import qualified Derive.Tempo as Tempo

import qualified Perform.Signal as Signal
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.TrackTree as TrackTree
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig

import           Global
import           Types


noteDeriver :: BlockId -> Derive.NoteDeriver
noteDeriver blockId = do
    (tree, blockRange) <- Derive.evalUi $
        (,) <$> TrackTree.blockEventsTree blockId
            <*> Ui.blockLogicalRange blockId
    withPerBlockState (snd blockRange) $ deriveTree blockRange tree

-- | Reset Dynamic state for a new block.
withPerBlockState :: TrackTime -> Derive.Deriver a -> Derive.Deriver a
withPerBlockState end = clear . Derive.withVal EnvKey.blockEnd end
    where
    clear = Internal.local $ \state -> state
        { Derive.stateNoteTrack = Nothing
        , Derive.statePitchMap = Nothing -- set by 'with_pitch_map' below
        }

-- * control deriver

-- | Control blocks are very restricted: they should consist of a single
-- branch ending in a track with a @%@ title, which is the default control,
-- which should have been set by the calling track.  If the requirements are
-- met, a fake note track will be appended to make this a valid note block,
-- with a single note event whose only job is to collect the the default
-- control.
controlDeriver :: BlockId -> Ui.StateId Derive.ControlDeriver
controlDeriver blockId = do
    tree <- TrackTree.blockEventsTree blockId
    blockRange <- Ui.blockLogicalRange blockId
    case checkControlTree (snd blockRange) tree of
        Left err -> Ui.throw $ "control block skeleton malformed: " <> err
        Right tree -> pure $ withPerBlockState (snd blockRange) $
            deriveControlTree blockRange tree

-- | Name of the call for the control deriver hack.
captureNullControl :: Expr.Symbol
captureNullControl = "capture-null-control"

-- | Ensure the tree meets the requirements documented by 'control_deriver'
-- and append the fake note track if it does.
checkControlTree :: ScoreTime -> TrackTree.EventsTree
    -> Either Text TrackTree.EventsTree
checkControlTree blockEnd forest = case forest of
    [] -> Left "empty block"
    [Tree.Node track []]
        | TrackTree.trackTitle track == "%" ->
            Right [Tree.Node track [Tree.Node captureTrack []]]
        | otherwise -> Left $ "skeleton must end in % track, ends with "
            <> showt (TrackTree.trackTitle track)
    [Tree.Node track subs] -> do
        subs <- checkControlTree blockEnd subs
        pure [Tree.Node track subs]
    tracks -> Left $ "skeleton must have only a single branch, "
        <> "but there are multiple children: "
        <> showt (map (TrackTree.trackTitle . Tree.rootLabel) tracks)
    where
    events = Events.singleton $
        Event.event 0 blockEnd (Expr.unsym captureNullControl)
    captureTrack = TrackTree.makeTrack ">" events blockEnd

deriveControlTree :: (ScoreTime, ScoreTime) -> TrackTree.EventsTree
    -> Derive.ControlDeriver
deriveControlTree blockRange tree = do
    -- There are an awful lot of things that can go wrong.  I guess that's why
    -- this is a hack.
    events <- deriveTree blockRange tree
    case Stream.partition events of
        ([event], logs) -> case Score.eventControl Controls.null event of
            Nothing -> Derive.throw "control call didn't emit Controls.null"
            Just signal -> pure $ Stream.fromSortedList $
                -- The calling control itself will be providing the type since
                -- types are at the level of the signal as a whole.
                LEvent.Event (ScoreT.valOf signal) : map LEvent.Log logs
        (events, logs) -> do
            msg <- complain events
            pure $ Stream.fromLogs $ msg : logs
    where
    -- Or I could throw, but this way any other logs the block emitted will
    -- also be visible, and they might have something interesting.
    complain events = Derive.initializeLogMsg $ Log.msg Log.Warn Nothing $
        "control call should have emitted a single call to "
        <> ShowVal.showVal captureNullControl
        <> " which produces a single event, but got events: " <> showt events

-- ** implementation

deriveTree :: (ScoreTime, ScoreTime) -> TrackTree.EventsTree
    -> Derive.NoteDeriver
deriveTree blockRange tree
    | Just node <- hasTopTempoTrack tree = deriveTrack True node
    | otherwise = do
        -- Every block must have a tempo track as the topmost track.  This is
        -- because Tempo.with_tempo sets up some stuff that every block needs,
        -- and because I need a TrackWarp for the tracks below.
        tempo <- getTempo
        Tempo.withTempo True (Just blockRange) Nothing (Signal.constant tempo)
            (deriveTracks tree)
    where
    getTempo = Derive.lookupVal EnvKey.tempo >>= \case
        Nothing -> Derive.getUiConfig $
            UiConfig.defaultTempo . UiConfig.configDefault
        Just tempo -> pure tempo

-- | Derive an EventsTree.
deriveTracks :: TrackTree.EventsTree -> Derive.NoteDeriver
deriveTracks = mconcatMap (deriveTrack False)

-- | Derive a single track node and any tracks below it.
deriveTrack :: Bool -- ^ True if this is the single topmost track and is
    -- a tempo track.  Ultimately this flag gets threaded all the way down to
    -- "Derive.Tempo".
    -> TrackTree.EventsNode -> Derive.NoteDeriver
deriveTrack toplevel node@(Tree.Node track subs)
    | Parse.comment `Text.isPrefixOf` TrackTree.trackTitle track = mempty
    | ParseTitle.isNoteTrack (TrackTree.trackTitle track) =
        withStack $ Cache.track track (TrackTree.trackChildren node) $ do
            events <- withVoice track $ withPitchMap track subs $
                maybe id withNoteTrack (TrackTree.trackId track) $
                Note.dNoteTrack deriveTracks node
            when (TrackTree.trackSliced track == TrackTree.NotSliced)
                defragment
            mapM_ (Note.stashSignalIfWanted events)
                (noteSignalTracks track subs)
            pure events
    | otherwise = withStack $
        Control.dControlTrack (Control.Config toplevel True) track
            (deriveTracks subs)
    where
    withVoice = maybe id (Derive.withVal EnvKey.trackVoice)
        . TrackTree.trackVoice
    defragment = do
        warp <- Internal.getWarp
        Internal.modifyCollect $ EvalTrack.defragmentTrackSignals warp
    withStack = maybe id Internal.withStackTrack (TrackTree.trackId track)

withNoteTrack :: TrackId -> Derive.Deriver a -> Derive.Deriver a
withNoteTrack trackId deriver = do
    maybeBlockId <- msum . map Stack.blockOf . Stack.innermost <$>
        Internal.getStack
    maybe id with maybeBlockId deriver
    where
    with blockId = Internal.local $ \state -> state
        { Derive.stateNoteTrack = Just (blockId, trackId) }

-- | Extract tracks that might want to stash a signal.
--
-- A note track can display a signal, extracted from the events it generates.
-- But if the track has orphan sub-tracks, its events will be evaluated
-- separately, either directly as orphans, or indirectly as the children of the
-- non-orphan sections.  At the moment it seems simpler to collect all the
-- events of the whole set of tracks and consider them the output of all of
-- them.
noteSignalTracks :: TrackTree.Track -> TrackTree.EventsTree
    -> [TrackTree.Track]
noteSignalTracks track subs = case TrackTree.trackSliced track of
    TrackTree.NotSliced -> track : filter isNote (concatMap Tree.flatten subs)
    _ -> []
    where isNote = ParseTitle.isNoteTrack . TrackTree.trackTitle

-- | The tempo track, if the top level track is a tempo track.
hasTopTempoTrack :: [Tree.Tree TrackTree.Track]
    -> Maybe (Tree.Tree TrackTree.Track)
hasTopTempoTrack tree = case tree of
    [node@(Tree.Node track _)] | isTempo track -> Just node
    _ -> Nothing
    where
    isTempo = ParseTitle.isTempoTrack . TrackTree.trackTitle

-- * track pitch map

-- | Given an event track, look for a pitch track below it, and derive it
-- standalone.  If there is none, then take 'Derive.state_pitch'.  Put this
-- into 'Derive.state_pitch_map'.
withPitchMap :: TrackTree.Track -> TrackTree.EventsTree -> Derive.Deriver a
    -> Derive.Deriver a
withPitchMap track subs deriver = case TrackTree.trackSliced track of
    TrackTree.NotSliced -> do
        pmap <- getPitchMap subs
        Internal.local (\state -> state { Derive.statePitchMap = Just pmap })
            deriver
    _ -> deriver

getPitchMap :: TrackTree.EventsTree
    -> Derive.Deriver (Maybe PSignal.PSignal, [Log.Msg])
getPitchMap subs = case pitchMapTrack subs of
    Just pitchTrack -> do
        state <- Derive.get
        pure $ derivePitchMap state pitchTrack
    Nothing -> do
        sig <- Internal.getDynamic Derive.statePitch
        pure (Just sig, [])

-- | Derive the given pitch track lazily.
--
-- It uses a hack similar to control blocks, e.g. 'derive_control_tree'.
derivePitchMap :: Derive.State -> TrackTree.Track
    -> (Maybe PSignal.PSignal, [Log.Msg])
derivePitchMap state pitchTrack = case result of
    Right sig -> (Just sig, logs)
    Left err -> (Nothing, Derive.errorToWarn err : logs)
    where
    (result, _, logs) = Derive.run stripped (derive pitchTrack)
    stripped = state
        { Derive.stateDynamic = (Derive.stateDynamic state)
            -- As documented by 'Derive.state_pitch_map'.
            { Derive.statePitchMap = Nothing }
        }
    derive pitchTrack = do
        (events, logs) <- Stream.partition <$> Internal.inRealTime
            (Control.dControlTrack config pitchTrack capture)
        mapM_ Log.write logs
        Derive.require "get_pitch_map: no event" $
            Score.eventPitch <$> Lists.head events
    config = Control.Config
        { configToplevelTempo = False
        , configUseCache = False
        }
    capture :: Derive.NoteDeriver
    capture = do
        pitch <- Internal.getDynamic Derive.statePitch
        pure $ Stream.fromEvent $
            Score.emptyEvent { Score.eventPitch = pitch }

pitchMapTrack :: TrackTree.EventsTree -> Maybe TrackTree.Track
pitchMapTrack = fmap Tree.rootLabel . Trees.find isPitch
    where isPitch = ParseTitle.isPitchTrack . TrackTree.trackTitle
