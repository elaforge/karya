module Derive.Call.Integrate (
    note_calls, unwarp, has_nontempo_track
    -- * track integrate
    , get_integrated_tracks
    , integrate_track
) where
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Tree as Tree

import Util.Control
import qualified Ui.State as State
import qualified Derive.Call as Call
import qualified Derive.Call.Util as Util
import qualified Derive.CallSig as CallSig
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack
import qualified Derive.TrackInfo as TrackInfo
import qualified Derive.TrackLang as TrackLang

import qualified Perform.RealTime as RealTime
import Types


note_calls :: Derive.NoteCallMap
note_calls = Derive.make_calls
    [ ("<<", c_block_integrate)
    -- The call 'integrate_track' is actually called directly from BlockUtil,
    -- so this just passes the events through.
    , (TrackLang.symbol_string integrate_track_symbol,
        Derive.transformer "track-integrate" $ \_ deriver -> deriver)
    ]

integrate_track_symbol :: TrackLang.CallId
integrate_track_symbol = TrackLang.Symbol "<"

-- * block integrate

c_block_integrate :: Derive.NoteCall
c_block_integrate = Derive.transformer "block-integrate" $ \args deriver ->
    CallSig.call0 args $ do
        events <- deriver
        block_integrate events
        return events

block_integrate :: Derive.Events -> Derive.Deriver ()
block_integrate events = do
    -- Only collect an integration if this is the top level block.  Otherwise
    -- I can get integrating blocks called from many places and who knows which
    -- one is supposed to be integrated.
    maybe_block_id <- is_root_block
    when_just maybe_block_id $ \block_id -> do
        events <- Derive.eval_ui "c_block_integrate" $ unwarp block_id events
        key <- Util.lookup_key
        let integrated = Derive.Integrated (Left block_id) [] events key
        Internal.merge_collect $ mempty
            { Derive.collect_integrated = [integrated] }

-- | If the block uses a default tempo, it will get applied once during
-- integration, and again when it's played.  I should avoid applying the
-- default tempo at all for integration, but that's too much bother.  Instead,
-- unwarp the events if the default tempo was applied.
--
-- TODO Getting rid of the default tempo entirely is also an option.
unwarp :: (State.M m) => BlockId -> Derive.Events -> m Derive.Events
unwarp block_id events = ifM (uses_default_tempo block_id)
    (do tempo <- State.get_default State.default_tempo
        return $ move (RealTime.seconds tempo) events)
    (return events)
    where
    move tempo = map $ fmap $ Score.move (*tempo) . Score.duration (*tempo)

uses_default_tempo :: (State.M m) => BlockId -> m Bool
uses_default_tempo block_id =
    has_nontempo_track <$> State.events_tree_of block_id

is_root_block :: Derive.Deriver (Maybe BlockId)
is_root_block = do
    stack <- Internal.get_stack
    return $ msum $ map Stack.block_of (Stack.innermost stack)

-- | Does this tree have any non-tempo tracks at the top level?
has_nontempo_track :: State.EventsTree -> Bool
has_nontempo_track = any $ \(Tree.Node track _) ->
    not $ TrackInfo.is_tempo_track (State.tevents_title track)

-- * track integrate

-- | Get the tracks which should be integrated and have damage.
get_integrated_tracks :: State.EventsTree
    -> Derive.Deriver [(State.EventsNode, [TrackLang.Term])]
get_integrated_tracks tracks = do
    maybe_block_id <- is_root_block
    case maybe_block_id of
        Nothing -> return []
        Just block_id -> do
            damage <- Internal.get_constant Derive.state_score_damage
            return [ (track, args)
                | (track, Just args) <- zip tracks (map is_integrated tracks)
                , has_damage block_id damage track
                ]

-- | Just args if a integrate call was found in the track title.
is_integrated :: State.EventsNode -> Maybe [TrackLang.Term]
is_integrated (Tree.Node track _)
    | TrackInfo.is_note_track title = case TrackInfo.parse_note title of
        Right (_ : TrackLang.Call call args : _)
            | call == integrate_track_symbol -> Just args
        _ -> Nothing
    | otherwise = case TrackInfo.parse_control_expr title of
        Right (_, TrackLang.Call call args : _)
            | call == integrate_track_symbol -> Just args
        _ -> Nothing
    where title = State.tevents_title track

-- | True if this track or any of its children have score damage.
has_damage :: BlockId -> Derive.ScoreDamage -> State.EventsNode -> Bool
has_damage block_id damage node =
    block_id `Set.member` Derive.sdamage_blocks damage || is_damaged node
    where
    is_damaged (Tree.Node track subs) =
        maybe False (`Map.member` track_damage) (State.tevents_track_id track)
        || any is_damaged subs
    track_damage = Derive.sdamage_tracks damage

integrate_track :: TrackId -> [TrackId] -> [TrackLang.Term]
    -> Derive.EventDeriver -> Derive.Deriver ()
integrate_track track_id children args deriver = do
    vals <- mapM Call.eval args
    -- Fake up a passed args so I can use CallSig.  TODO ugh, surely there's
    -- some way to get rid of dummy_call_info?
    let passed = Derive.PassedArgs vals integrate_track_symbol
            (Derive.dummy_call_info 0 1 "")
    -- All that work making args and it doesn't want any... well maybe later it
    -- will.
    CallSig.call0 passed $ do
        events <- deriver
        integrate_events track_id children events

integrate_events :: TrackId -> [TrackId] -> Derive.Events -> Derive.Deriver ()
integrate_events track_id children events = do
    -- If it wasn't a root block this should never have been called, because of
    -- 'get_integrated_tracks', but it's sort of a pain to pass down here, so
    -- let's get it again.
    maybe_block_id <- is_root_block
    when_just maybe_block_id $ \block_id -> do
        key <- Util.lookup_key
        events <- Derive.eval_ui "c_block_integrate" $ unwarp block_id events
        let integrated = Derive.Integrated (Right track_id) children events key
        Internal.merge_collect $ mempty
            { Derive.collect_integrated = [integrated] }
