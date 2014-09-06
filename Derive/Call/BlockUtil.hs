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
    , has_nontempo_track

#ifdef TESTING
    , derive_tree
#endif
) where
import qualified Data.Map as Map
import qualified Data.Tree as Tree

import Util.Control
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
import qualified Derive.Environ as Environ
import qualified Derive.EvalTrack as EvalTrack
import qualified Derive.LEvent as LEvent
import qualified Derive.Note as Note
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Tempo as Tempo
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Signal as Signal
import Types


note_deriver :: BlockId -> Derive.NoteDeriver
note_deriver block_id = do
    (tree, block_end) <- Derive.eval_ui ("note_deriver " <> show block_id) $
        get_tree block_id
    derive_tree block_end (Control.split_control_tracks tree)

-- * control deriver

-- | Control blocks are very restricted: they should consist of a single
-- branch ending in a track with a @%@ title, which is the default control,
-- which should have been set by the calling track.  If the requirements are
-- met, a fake note track will be appended to make this a valid note block,
-- with a single note event whose only job is to collect the the default
-- control.
control_deriver :: BlockId -> State.StateId Derive.ControlDeriver
control_deriver block_id = do
    (tree, block_end) <- get_tree block_id
    case check_control_tree block_end tree of
        Left err -> State.throw $ "control block skeleton malformed: " ++ err
        Right tree -> return $ derive_control_tree block_end tree

-- | Name of the call for the control deriver hack.
capture_null_control :: TrackLang.CallId
capture_null_control = "capture-null-control"

-- | Ensure the tree meets the requirements documented by 'control_deriver'
-- and append the fake note track if it does.
check_control_tree :: ScoreTime -> TrackTree.EventsTree
    -> Either String TrackTree.EventsTree
check_control_tree block_end forest = case forest of
    [] -> Left "empty block"
    [Tree.Node track []]
        | TrackTree.track_title track == "%" ->
            Right [Tree.Node track [Tree.Node capture_track []]]
        | otherwise -> Left $ "skeleton must end in % track, ends with "
            ++ show (TrackTree.track_title track)
    [Tree.Node track subs] -> do
        subs <- check_control_tree block_end subs
        return [Tree.Node track subs]
    tracks -> Left $ "skeleton must have only a single branch, "
        ++ "but there are multiple children: "
        ++ show (map (TrackTree.track_title . Tree.rootLabel) tracks)
    where
    events = Events.singleton $
        Event.event 0 block_end (TrackLang.unsym capture_null_control)
    capture_track = TrackTree.make_track ">" events block_end

derive_control_tree :: ScoreTime -> TrackTree.EventsTree
    -> Derive.ControlDeriver
derive_control_tree block_end tree = do
    -- There are an awful lot of things that can go wrong.  I guess that's why
    -- this is a hack.
    events <- derive_tree block_end tree
    let lookup_control = Map.lookup Controls.null . Score.event_controls
    case LEvent.partition events of
        ([event], logs) -> case lookup_control event of
            Nothing -> Derive.throw "control call didn't emit Controls.null"
            Just signal -> return $
                -- The calling control itself will be providing the type since
                -- types are at the level of the signal as a whole.
                LEvent.Event (Score.typed_val signal) : map LEvent.Log logs
        (events, logs) -> do
            msg <- complain events
            return $ LEvent.Log msg : map LEvent.Log logs
    where
    -- Or I could throw, but this way any other logs the block emitted will
    -- also be visible, and they might have something interesting.
    complain events = Log.initialized_msg Log.Warn $
        "control call should have emitted a single call to "
        <> ShowVal.show_val capture_null_control
        <> " which produces a single event, but got events: " <> showt events

-- ** implementation

get_tree :: State.M m => BlockId -> m (TrackTree.EventsTree, ScoreTime)
get_tree block_id = do
    info_tree <- TrackTree.strip_disabled_tracks block_id
        =<< TrackTree.track_tree_of block_id
    ruler_end <- State.block_ruler_end block_id
    tree <- TrackTree.events_tree block_id ruler_end info_tree
    return (tree, ruler_end)

derive_tree :: ScoreTime -> TrackTree.EventsTree -> Derive.NoteDeriver
derive_tree block_end tree = with_default_tempo (derive_tracks tree)
    where
    -- d_tempo sets up some stuff that every block needs, so add one if a block
    -- doesn't have at least one top level tempo.
    with_default_tempo deriver
        -- To ensure that every track is associated with a TrackWarp, I can't
        -- have tracks that don't have a tempo track above them.  Those tracks
        -- implicitly have an id warp, so this just makes that explicit.
        | has_nontempo_track tree = do
            tempo <- Derive.lookup_val Environ.tempo >>= \x -> case x of
                Nothing -> Derive.get_ui_config
                    (State.default_tempo . State.config_default)
                Just tempo -> return tempo
            Tempo.with_tempo block_end Nothing (Signal.constant tempo) deriver
        | otherwise = deriver

-- | Derive an EventsTree.
derive_tracks :: TrackTree.EventsTree -> Derive.NoteDeriver
derive_tracks = mconcatMap derive_track

-- | Derive a single track node and any tracks below it.
derive_track :: TrackTree.EventsNode -> Derive.NoteDeriver
derive_track node@(Tree.Node track subs)
    | ParseTitle.is_note_track (TrackTree.track_title track) =
        with_stack $ Cache.track track (TrackTree.track_children node) $ do
            events <- Internal.track_setup track $
                with_voice track $
                Note.d_note_track derive_tracks node
            unless (TrackTree.track_sliced track) defragment
            mapM_ (Note.stash_signal_if_wanted events)
                (note_signal_tracks track subs)
            return events
    -- I'd like to call track_setup up here, but tempo tracks are treated
    -- differently, so it goes inside d_control_track.
    | otherwise = with_stack $ Control.d_control_track node (derive_tracks subs)
    where
    with_voice = maybe id (Derive.with_val Environ.track_voice)
        . TrackTree.track_voice
    defragment = do
        warp <- Internal.get_dynamic Derive.state_warp
        Internal.modify_collect $ EvalTrack.defragment_track_signals warp
    with_stack = maybe id Internal.with_stack_track
        (TrackTree.track_id track)

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

-- | Does this tree have any non-tempo tracks at the top level?
has_nontempo_track :: TrackTree.EventsTree -> Bool
has_nontempo_track = any $ \(Tree.Node track _) ->
    not $ ParseTitle.is_tempo_track (TrackTree.track_title track)
