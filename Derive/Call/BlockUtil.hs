{-# LANGUAGE CPP #-}
{- | Utilities for "Derive.Call.Block".
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

import qualified Derive.Control as Control
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.LEvent as LEvent
import qualified Derive.Note as Note
import qualified Derive.Score as Score
import qualified Derive.Slice as Slice
import qualified Derive.TrackInfo as TrackInfo

import qualified Perform.Signal as Signal
import Types


note_deriver :: BlockId -> State.StateId Derive.EventDeriver
note_deriver block_id = do
    (tree, block_end) <- get_tree block_id
    return $ derive_tree block_end tree

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
capture_null_control :: String
capture_null_control = "capture-null-control"

-- | Ensure the tree meets the requirements documented by 'control_deriver'
-- and append the fake note track if it does.
check_control_tree :: ScoreTime -> State.EventsTree
    -> Either String State.EventsTree
check_control_tree block_end forest = case forest of
    [] -> Left "empty block"
    [Tree.Node track []]
        | State.tevents_title track == "%" ->
            Right [Tree.Node track [Tree.Node capture_track []]]
        | otherwise -> Left $ "skeleton must end in % track, ends with "
            ++ show (State.tevents_title track)
    [Tree.Node track subs] -> do
        subs <- check_control_tree block_end subs
        return [Tree.Node track subs]
    tracks -> Left $ "skeleton must have only a single branch, "
        ++ "but there are multiple children: "
        ++ show (map (State.tevents_title . Tree.rootLabel) tracks)
    where
    events = Events.singleton (Event.event 0 block_end capture_null_control)
    capture_track = (State.track_events ">" events block_end)

derive_control_tree :: ScoreTime -> State.EventsTree -> Derive.ControlDeriver
derive_control_tree block_end tree = do
    -- There are an awful lot of things that can go wrong.  I guess that's why
    -- this is a hack.
    events <- derive_tree block_end tree
    let lookup_control = Map.lookup Score.c_null . Score.event_controls
    case LEvent.partition events of
        ([event], logs) -> case lookup_control event of
            Nothing -> Derive.throw "control call didn't emit Score.c_null"
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
        ++ show capture_null_control ++ " which produces a single event, but "
        ++ "got events: " ++ show events

-- ** implementation

get_tree :: (State.M m) => BlockId -> m (State.EventsTree, ScoreTime)
get_tree block_id = do
    info_tree <- State.get_track_tree block_id
    block_end <- State.block_event_end block_id
    tree <- State.events_tree block_end info_tree
    block <- State.get_block block_id
    let mutes_tree = State.track_tree_mutes
            (State.muted_tracknums block info_tree) info_tree
    return (strip_mutes mutes_tree tree, block_end)

-- | Strip the events out of muted tracks.  If the tracks themselves were
-- stripped out it looks like there are orphans.  This way they are just tracks
-- that produce nothing.
--
-- It's ugly how the two trees are zipped up, but otherwise I have yet another
-- type for EventTreeMutes or hairy parameterization just for this one
-- function.
strip_mutes :: State.TrackTreeMutes -> State.EventsTree -> State.EventsTree
strip_mutes mutes tree = zipWith mute_node mutes tree
    where
    mute_node (Tree.Node (_, muted) ms) (Tree.Node track ts) =
        Tree.Node (if muted then mute track else track) (strip_mutes ms ts)
    mute track = track { State.tevents_events = Events.empty }

derive_tree :: ScoreTime -> State.EventsTree -> Derive.EventDeriver
derive_tree block_end tree = with_default_tempo (derive_tracks tree)
    where
    -- d_tempo sets up some stuff that every block needs, so add one if a block
    -- doesn't have at least one top level tempo.
    with_default_tempo deriver
        -- To ensure that every track is associated with a TrackWarp, I can't
        -- have tracks that don't have a tempo track above them.  Those tracks
        -- implicitly have an id warp, so this just makes that explicit.
        | has_nontempo_track tree = do
            tempo <- Derive.get_ui_config
                (State.default_tempo . State.config_default)
            Internal.d_tempo block_end Nothing (Signal.constant tempo) deriver
        | otherwise = deriver

-- | Derive an EventsTree.
derive_tracks :: State.EventsTree -> Derive.EventDeriver
derive_tracks = mconcat . map derive_track

-- | Derive a single track node and any tracks below it.
derive_track :: State.EventsNode -> Derive.EventDeriver
derive_track node@(Tree.Node track subs)
    | TrackInfo.is_note_track (State.tevents_title track) =
        with_stack $ derive_orphans (State.tevents_title track)
            (Slice.extract_orphans track subs)
            (Internal.track_setup track (Note.d_note_track node))
    -- I'd like track_setup up here, but tempo tracks are treated differently,
    -- so it goes inside d_control_track.
    | otherwise = with_stack $ Control.d_control_track node (derive_tracks subs)
    where
    derive_orphans title orphans
        -- If d_merge could tell when an EventDeriver was mempty and not
        -- evaluate it I wouldn't need this little optimization.
        | null orphans = id
        -- The orphans still get evaluated under the track title, otherwise
        -- they might miss the instrument.
        | otherwise = (<> Note.with_title title (derive_tracks orphans))
    with_stack = maybe id Internal.with_stack_track
        (State.tevents_track_id track)

-- | Does this tree have any non-tempo tracks at the top level?
has_nontempo_track :: State.EventsTree -> Bool
has_nontempo_track = any $ \(Tree.Node track _) ->
    not $ TrackInfo.is_tempo_track (State.tevents_title track)
