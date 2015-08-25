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
    note_deriver, control_deriver
    , capture_null_control
    , derive_tracks
    , has_top_tempo_track

#ifdef TESTING
    , derive_tree
#endif
) where
import qualified Data.Tree as Tree

import qualified Util.Log as Log
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.State as State
import qualified Ui.TrackTree as TrackTree

import qualified Derive.Cache as Cache
import qualified Derive.Control as Control
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.EnvKey as EnvKey
import qualified Derive.EvalTrack as EvalTrack
import qualified Derive.LEvent as LEvent
import qualified Derive.Note as Note
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Stack as Stack
import qualified Derive.Tempo as Tempo
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Signal as Signal
import Global
import Types


note_deriver :: BlockId -> Derive.NoteDeriver
note_deriver block_id = do
    (tree, block_range) <- Derive.eval_ui ("note_deriver " <> showt block_id) $
        (,) <$> TrackTree.block_events_tree block_id
            <*> State.block_logical_range block_id
    Derive.with_val EnvKey.block_end (snd block_range) $
        Internal.local (\state -> state { Derive.state_note_track = Nothing }) $
        derive_tree block_range tree

-- * control deriver

-- | Control blocks are very restricted: they should consist of a single
-- branch ending in a track with a @%@ title, which is the default control,
-- which should have been set by the calling track.  If the requirements are
-- met, a fake note track will be appended to make this a valid note block,
-- with a single note event whose only job is to collect the the default
-- control.
control_deriver :: BlockId -> State.StateId Derive.ControlDeriver
control_deriver block_id = do
    tree <- TrackTree.block_events_tree block_id
    block_range <- State.block_logical_range block_id
    case check_control_tree (snd block_range) tree of
        Left err -> State.throw $ "control block skeleton malformed: " <> err
        Right tree -> return $
            Derive.with_val EnvKey.block_end (snd block_range) $
                derive_control_tree block_range tree

-- | Name of the call for the control deriver hack.
capture_null_control :: TrackLang.CallId
capture_null_control = "capture-null-control"

-- | Ensure the tree meets the requirements documented by 'control_deriver'
-- and append the fake note track if it does.
check_control_tree :: ScoreTime -> TrackTree.EventsTree
    -> Either Text TrackTree.EventsTree
check_control_tree block_end forest = case forest of
    [] -> Left "empty block"
    [Tree.Node track []]
        | TrackTree.track_title track == "%" ->
            Right [Tree.Node track [Tree.Node capture_track []]]
        | otherwise -> Left $ "skeleton must end in % track, ends with "
            <> showt (TrackTree.track_title track)
    [Tree.Node track subs] -> do
        subs <- check_control_tree block_end subs
        return [Tree.Node track subs]
    tracks -> Left $ "skeleton must have only a single branch, "
        <> "but there are multiple children: "
        <> showt (map (TrackTree.track_title . Tree.rootLabel) tracks)
    where
    events = Events.singleton $
        Event.event 0 block_end (TrackLang.unsym capture_null_control)
    capture_track = TrackTree.make_track ">" events block_end

derive_control_tree :: (ScoreTime, ScoreTime) -> TrackTree.EventsTree
    -> Derive.ControlDeriver
derive_control_tree block_range tree = do
    -- There are an awful lot of things that can go wrong.  I guess that's why
    -- this is a hack.
    events <- derive_tree block_range tree
    case LEvent.partition events of
        ([event], logs) -> case Score.event_control Controls.null event of
            Nothing -> Derive.throw "control call didn't emit Controls.null"
            Just signal -> return $
                -- The calling control itself will be providing the type since
                -- types are at the level of the signal as a whole.
                LEvent.Event (Score.typed_val signal) : map LEvent.Log logs
        (events, logs) -> do
            msg <- complain events
            return $ LEvent.log msg : map LEvent.Log logs
    where
    -- Or I could throw, but this way any other logs the block emitted will
    -- also be visible, and they might have something interesting.
    complain events = Log.initialized_msg Log.Warn $
        "control call should have emitted a single call to "
        <> ShowVal.show_val capture_null_control
        <> " which produces a single event, but got events: " <> showt events

-- ** implementation

derive_tree :: (ScoreTime, ScoreTime) -> TrackTree.EventsTree
    -> Derive.NoteDeriver
derive_tree block_range tree
    | Just node <- has_top_tempo_track tree = derive_track True node
    | otherwise = do
        -- Every block must have a tempo track as the topmost track.  This is
        -- because Tempo.with_tempo sets up some stuff that every block needs,
        -- and because I need a TrackWarp for the tracks below.
        tempo <- get_tempo
        Tempo.with_tempo True (Just block_range) Nothing (Signal.constant tempo)
            (derive_tracks tree)
    where
    get_tempo = Derive.lookup_val EnvKey.tempo >>= \x -> case x of
        Nothing -> Derive.get_ui_config
            (State.default_tempo . State.config_default)
        Just tempo -> return tempo

-- | Derive an EventsTree.
derive_tracks :: TrackTree.EventsTree -> Derive.NoteDeriver
derive_tracks = mconcatMap (derive_track False)

-- | Derive a single track node and any tracks below it.
derive_track :: Bool -> TrackTree.EventsNode -> Derive.NoteDeriver
derive_track toplevel node@(Tree.Node track subs)
    | ParseTitle.is_note_track (TrackTree.track_title track) =
        with_stack $ Cache.track track (TrackTree.track_children node) $ do
            events <- Internal.track_setup track $ with_voice track $
                maybe id with_note_track (TrackTree.track_id track) $
                Note.d_note_track derive_tracks node
            unless (TrackTree.track_sliced track) defragment
            mapM_ (Note.stash_signal_if_wanted events)
                (note_signal_tracks track subs)
            return events
    -- I'd like to call track_setup up here, but tempo tracks are treated
    -- differently, so it goes inside d_control_track.
    | otherwise = with_stack $
        Control.d_control_track toplevel node (derive_tracks subs)
    where
    with_voice = maybe id (Derive.with_val EnvKey.track_voice)
        . TrackTree.track_voice
    defragment = do
        warp <- Internal.get_warp
        Internal.modify_collect $ EvalTrack.defragment_track_signals warp
    with_stack = maybe id Internal.with_stack_track (TrackTree.track_id track)

with_note_track :: TrackId -> Derive.Deriver a -> Derive.Deriver a
with_note_track track_id deriver = do
    maybe_block_id <- msum . map Stack.block_of . Stack.innermost <$>
        Internal.get_stack
    maybe id with maybe_block_id deriver
    where
    with block_id = Internal.local $ \state -> state
        { Derive.state_note_track = Just (block_id, track_id) }

-- | Extract tracks that might want to stash a signal.
--
-- A note track can display a signal, extracted from the events it generates.
-- But if the track has orphan sub-tracks, its events will be evaluated
-- separately, either directly as orphans, or indirectly as the children of the
-- non-orphan sections.  At the moment it seems simpler to collect all the
-- events of the whole set of tracks and consider them the output of all of
-- them.
note_signal_tracks :: TrackTree.Track -> TrackTree.EventsTree
    -> [TrackTree.Track]
note_signal_tracks track subs
    | TrackTree.track_sliced track = []
    | otherwise = track : filter is_note (concatMap Tree.flatten subs)
    where is_note = ParseTitle.is_note_track . TrackTree.track_title

-- | The tempo track, if the top level track is a tempo track.
has_top_tempo_track :: [Tree.Tree TrackTree.Track]
    -> Maybe (Tree.Tree TrackTree.Track)
has_top_tempo_track tree = case tree of
    [node@(Tree.Node track _)] | is_tempo track -> Just node
    _ -> Nothing
    where
    is_tempo = ParseTitle.is_tempo_track . TrackTree.track_title
