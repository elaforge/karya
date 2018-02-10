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
import qualified Util.Seq as Seq
import qualified Util.Tree

import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.TrackTree as TrackTree
import qualified Ui.Ui as Ui

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
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Stack as Stack
import qualified Derive.Stream as Stream
import qualified Derive.Tempo as Tempo

import qualified Perform.Signal as Signal
import Global
import Types


note_deriver :: BlockId -> Derive.NoteDeriver
note_deriver block_id = do
    (tree, block_range) <- Derive.eval_ui $
        (,) <$> TrackTree.block_events_tree block_id
            <*> Ui.block_logical_range block_id
    with_per_block_state (snd block_range) $ derive_tree block_range tree

-- | Reset Dynamic state for a new block.
with_per_block_state :: TrackTime -> Derive.Deriver a -> Derive.Deriver a
with_per_block_state end = clear . Derive.with_val EnvKey.block_end end
    where
    clear = Internal.local $ \state -> state
        { Derive.state_note_track = Nothing
        , Derive.state_pitch_map = Nothing -- set by 'with_pitch_map' below
        }

-- * control deriver

-- | Control blocks are very restricted: they should consist of a single
-- branch ending in a track with a @%@ title, which is the default control,
-- which should have been set by the calling track.  If the requirements are
-- met, a fake note track will be appended to make this a valid note block,
-- with a single note event whose only job is to collect the the default
-- control.
control_deriver :: BlockId -> Ui.StateId Derive.ControlDeriver
control_deriver block_id = do
    tree <- TrackTree.block_events_tree block_id
    block_range <- Ui.block_logical_range block_id
    case check_control_tree (snd block_range) tree of
        Left err -> Ui.throw $ "control block skeleton malformed: " <> err
        Right tree -> return $ with_per_block_state (snd block_range) $
            derive_control_tree block_range tree

-- | Name of the call for the control deriver hack.
capture_null_control :: Expr.Symbol
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
        Event.event 0 block_end (Expr.unsym capture_null_control)
    capture_track = TrackTree.make_track ">" events block_end

derive_control_tree :: (ScoreTime, ScoreTime) -> TrackTree.EventsTree
    -> Derive.ControlDeriver
derive_control_tree block_range tree = do
    -- There are an awful lot of things that can go wrong.  I guess that's why
    -- this is a hack.
    events <- derive_tree block_range tree
    case Stream.partition events of
        ([event], logs) -> case Score.event_control Controls.null event of
            Nothing -> Derive.throw "control call didn't emit Controls.null"
            Just signal -> return $ Stream.from_sorted_list $
                -- The calling control itself will be providing the type since
                -- types are at the level of the signal as a whole.
                LEvent.Event (Score.typed_val signal) : map LEvent.Log logs
        (events, logs) -> do
            msg <- complain events
            return $ Stream.from_logs $ msg : logs
    where
    -- Or I could throw, but this way any other logs the block emitted will
    -- also be visible, and they might have something interesting.
    complain events = Derive.initialize_log_msg $ Log.msg Log.Warn Nothing $
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
        Nothing -> Derive.get_ui_config (Ui.default_tempo . Ui.config_default)
        Just tempo -> return tempo

-- | Derive an EventsTree.
derive_tracks :: TrackTree.EventsTree -> Derive.NoteDeriver
derive_tracks = mconcatMap (derive_track False)

-- | Derive a single track node and any tracks below it.
derive_track :: Bool -- ^ True if this is the single topmost track and is
    -- a tempo track.  Ultimately this flag gets threaded all the way down to
    -- "Derive.Tempo".
    -> TrackTree.EventsNode -> Derive.NoteDeriver
derive_track toplevel node@(Tree.Node track subs)
    | ParseTitle.is_note_track (TrackTree.track_title track) =
        with_stack $ Cache.track track (TrackTree.track_children node) $ do
            events <- with_voice track $ with_pitch_map track subs $
                maybe id with_note_track (TrackTree.track_id track) $
                Note.d_note_track derive_tracks node
            when (TrackTree.track_sliced track == TrackTree.NotSliced)
                defragment
            mapM_ (Note.stash_signal_if_wanted events)
                (note_signal_tracks track subs)
            return events
    | otherwise = with_stack $
        Control.d_control_track (Control.Config toplevel True) track
            (derive_tracks subs)
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
note_signal_tracks track subs = case TrackTree.track_sliced track of
    TrackTree.NotSliced -> track : filter is_note (concatMap Tree.flatten subs)
    _ -> []
    where is_note = ParseTitle.is_note_track . TrackTree.track_title

-- | The tempo track, if the top level track is a tempo track.
has_top_tempo_track :: [Tree.Tree TrackTree.Track]
    -> Maybe (Tree.Tree TrackTree.Track)
has_top_tempo_track tree = case tree of
    [node@(Tree.Node track _)] | is_tempo track -> Just node
    _ -> Nothing
    where
    is_tempo = ParseTitle.is_tempo_track . TrackTree.track_title

-- * track pitch map

-- | Given an event track, look for a pitch track below it, and derive it
-- standalone.  If there is none, then take 'Derive.state_pitch'.  Put this
-- into 'Derive.state_pitch_map'.
with_pitch_map :: TrackTree.Track -> TrackTree.EventsTree -> Derive.Deriver a
    -> Derive.Deriver a
with_pitch_map track subs deriver = case TrackTree.track_sliced track of
    TrackTree.NotSliced -> do
        pmap <- get_pitch_map subs
        Internal.local (\state -> state { Derive.state_pitch_map = Just pmap })
            deriver
    _ -> deriver

get_pitch_map :: TrackTree.EventsTree
    -> Derive.Deriver (Maybe PSignal.PSignal, [Log.Msg])
get_pitch_map subs = case pitch_map_track subs of
    Just pitch_track -> do
        state <- Derive.get
        return $ derive_pitch_map state pitch_track
    Nothing -> do
        sig <- Internal.get_dynamic Derive.state_pitch
        return (Just sig, [])

-- | Derive the given pitch track lazily.
--
-- It uses a hack similar to control blocks, e.g. 'derive_control_tree'.
derive_pitch_map :: Derive.State -> TrackTree.Track
    -> (Maybe PSignal.PSignal, [Log.Msg])
derive_pitch_map state pitch_track = case result of
    Right sig -> (Just sig, logs)
    Left err -> (Nothing, Derive.error_to_warn err : logs)
    where
    (result, _, logs) = Derive.run stripped (derive pitch_track)
    stripped = state
        { Derive.state_dynamic = (Derive.state_dynamic state)
            -- As documented by 'Derive.state_pitch_map'.
            { Derive.state_pitch_map = Nothing }
        }
    derive pitch_track = do
        (events, logs) <- Stream.partition <$>
            Control.d_control_track config pitch_track capture
        mapM_ Log.write logs
        Derive.require "get_pitch_map: no event" $
            Score.event_pitch <$> (Seq.head events)
    config = Control.Config
        { config_toplevel_tempo = False
        , config_use_cache = False
        }
    capture :: Derive.NoteDeriver
    capture = do
        pitch <- Internal.get_dynamic Derive.state_pitch
        return $ Stream.from_event $
            Score.empty_event { Score.event_pitch = pitch }

pitch_map_track :: TrackTree.EventsTree -> Maybe TrackTree.Track
pitch_map_track = fmap Tree.rootLabel . Util.Tree.find is_pitch
    where is_pitch = ParseTitle.is_pitch_track . TrackTree.track_title
