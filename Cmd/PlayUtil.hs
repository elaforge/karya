-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Functions to do with performance.  This is split off from "Cmd.Play",
-- which contains play Cmds and their direct support.
module Cmd.PlayUtil (
    initial_environ
    , cached_derive, uncached_derive
    , derive_block, run, run_with_dynamic
    , is_score_damage_log
    , get_constant, initial_dynamic
    -- * perform
    , perform_from, shift_messages, first_time
    , events_from, overlapping_events
    , perform_events, get_convert_lookup
    , midi_configs
) where
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import qualified Util.Log as Log
import qualified Util.Tree as Tree
import qualified Util.Vector

import qualified Midi.Midi as Midi
import qualified Ui.Block as Block
import qualified Ui.State as State
import qualified Ui.StateConfig as StateConfig
import qualified Ui.TrackTree as TrackTree

import qualified Cmd.Cmd as Cmd
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Call.Prelude.Block as Prelude.Block
import qualified Derive.Derive as Derive
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack

import qualified Perform.Midi.Convert as Convert
import qualified Perform.Midi.Patch as Patch
import qualified Perform.Midi.Perform as Perform
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import qualified Instrument.Common as Common
import qualified App.Config as Config
import Global
import Types


-- | There are a few environ values that almost everything relies on.
initial_environ :: Env.Environ
initial_environ = Env.from_list
    -- Control interpolators rely on this.
    [ (EnvKey.srate, BaseTypes.num 0.015)
    -- Looking up any val call relies on having a scale in scope.
    , (EnvKey.scale, BaseTypes.VSymbol
        (BaseTypes.Symbol Config.default_scale_id))
    , (EnvKey.attributes, BaseTypes.VAttributes mempty)
    , (EnvKey.seed, BaseTypes.num 0)
    ]

-- | Derive with the cache.
cached_derive :: Cmd.M m => BlockId -> m Derive.Result
cached_derive block_id = do
    maybe_perf <- Cmd.lookup_performance block_id
    case maybe_perf of
        Nothing -> uncached_derive block_id
        Just perf -> derive_block (Cmd.perf_derive_cache perf)
            (Cmd.perf_damage perf) block_id

uncached_derive :: Cmd.M m => BlockId -> m Derive.Result
uncached_derive = derive_block mempty mempty

-- | Derive the contents of the given block to score events.
derive_block :: Cmd.M m => Derive.Cache -> Derive.ScoreDamage -> BlockId
    -> m Derive.Result
derive_block cache damage block_id = do
    global_transform <- State.config#State.global_transform <#> State.get
    fmap Derive.extract_result $ run cache damage $ do
        unless (damage == mempty) $
            Log.debug $ "score damage for " <> showt block_id <> ": "
                <> pretty damage
        Prelude.Block.eval_root_block global_transform block_id

is_score_damage_log :: Log.Msg -> Bool
is_score_damage_log = ("score damage for " `Text.isPrefixOf`) . Log.msg_text

run :: Cmd.M m => Derive.Cache -> Derive.ScoreDamage -> Derive.Deriver a
    -> m (Derive.RunResult a)
run cache damage deriver = do
    constant <- get_constant cache damage
    return $ Derive.derive constant initial_dynamic deriver

-- | Run a derivation when you already know the Dynamic.  This is the case when
-- deriving at a certain point in the score via the TrackDynamic.
run_with_dynamic :: Cmd.M m => Derive.Dynamic -> Derive.Deriver a
    -> m (Derive.RunResult a)
run_with_dynamic dynamic deriver = do
    constant <- get_constant mempty mempty
    let state = Derive.State
            { state_threaded = Derive.initial_threaded
            , state_dynamic = dynamic
            , state_collect = mempty
            , state_constant = constant
            }
    return $ Derive.run state deriver

-- | Create deriver configuration.  This is the main place where Cmd level
-- configuration is adapted to the deriver.
get_constant :: Cmd.M m => Derive.Cache -> Derive.ScoreDamage
    -> m Derive.Constant
get_constant cache damage = do
    ui_state <- State.get
    lookup_scale <- Cmd.gets $ Cmd.config_lookup_scale . Cmd.state_config
    lookup_inst <- Cmd.get_lookup_instrument
    library <- Cmd.gets $ Cmd.config_library . Cmd.state_config
    defs_library <- get_library
    let allocs = State.config#State.allocations_map #$ ui_state
    return $ Derive.initial_constant ui_state (defs_library <> library)
        lookup_scale (adapt allocs lookup_inst) cache damage
    where
    adapt allocs lookup_inst = \inst -> case lookup_inst inst of
        Just (patch, _qualified) -> Just $
            Cmd.make_derive_instrument (lookup_controls inst allocs) patch
        Nothing -> Nothing
    lookup_controls inst allocs =
        maybe mempty (Common.config_controls . StateConfig.alloc_config)
            (Map.lookup inst allocs)

-- | Get Library from the cache.
get_library :: Cmd.M m => m Derive.Library
get_library = do
    cache <- Cmd.gets Cmd.state_ky_cache
    case cache of
        Nothing -> return mempty
        Just (Cmd.KyCache (Left err) _) -> Cmd.throw $ "get_library: " <> err
        Just (Cmd.KyCache (Right library) _) -> return library

initial_dynamic :: Derive.Dynamic
initial_dynamic = Derive.initial_dynamic initial_environ

perform_from :: Cmd.M m => RealTime -> Cmd.Performance -> m Perform.MidiEvents
perform_from start = perform_events . events_from start . Cmd.perf_events

shift_messages :: RealTime -> RealTime -> Perform.MidiEvents
    -> Perform.MidiEvents
shift_messages multiplier start events = shift start events
    where
    shift offset = map $ fmap $
        Midi.modify_timestamp ((* multiplier) . subtract offset)

-- | The first timestamp from the msgs.
first_time :: [LEvent.LEvent Midi.WriteMessage] -> RealTime
first_time msgs = case LEvent.events_of msgs of
    event : _ -> Midi.wmsg_ts event
    [] -> 0

-- | As a special case, a start <= 0 will get all events, including negative
-- ones.  This is so notes pushed before 0 won't be clipped on a play from 0.
events_from :: RealTime -> Vector.Vector Score.Event
    -> Vector.Vector Score.Event
events_from start events
    | start <= 0 = events
    | otherwise = Vector.drop i events
    where
    i = Util.Vector.lowest_index Score.event_start (start - RealTime.eta) events

-- | How to know how far back to go?  Impossible to know!  Well, I could look
-- up overlapping ui events, then map the earliest time to RealTime, and start
-- searching there.  But for now scanning from the beginning should be fast
-- enough.
overlapping_events :: RealTime -> Vector.Vector Score.Event -> [Score.Event]
overlapping_events pos = Vector.foldl' collect []
    where
    collect overlap event
        | Score.event_end event <= pos || Score.event_start event > pos =
            overlap
        | otherwise = event : overlap

-- | Filter events according to the Solo and Mute flags in the tracks of the
-- given blocks.
--
-- Solo only applies to the block on which the track is soloed.  So if you solo
-- a track on one block, other blocks will still play.
--
-- Solo takes priority over Mute.
filter_track_muted :: TrackTree.TrackTree -> [(BlockId, Block.Block)]
    -> [Score.Event] -> [Score.Event]
filter_track_muted tree blocks
    | not (Set.null soloed) = filter (not . stack_contains solo_muted)
    | not (Set.null muted) = filter (not . stack_contains muted)
    | otherwise = id
    where
    stack_contains track_ids = any (`Set.member` track_ids) . stack_tracks
    stack_tracks = mapMaybe Stack.track_of . Stack.innermost . Score.event_stack
    soloed = with_flag Block.Solo
    muted = with_flag Block.Mute
    solo_muted = solo_to_mute tree blocks soloed
    with_flag flag = Set.fromList
        [ track_id
        | (_, block) <- blocks
        , track <- Block.block_tracks block
        , Just track_id <- [Block.track_id track]
        , flag `Set.member` Block.track_flags track
        ]

-- | Solo is surprisingly tricky.  Solo means non soloed-tracks are muted,
-- unless there is no solo on the block, or the track is the parent or child of
-- a soloed track.
--
-- I've already rewritten this a bunch of times, hopefully this is the last
-- time.
solo_to_mute :: TrackTree.TrackTree -- ^ All the trees of the whole score,
    -- concatenated.  This is because I just need to know who is a child of
    -- who, and I don't care what block they're in.
    -> [(BlockId, Block.Block)]
    -> Set.Set TrackId -> Set.Set TrackId
solo_to_mute tree blocks soloed = Set.fromList
    [ track_id
    | (block_id, block) <- blocks
    , track <- Block.block_tracks block
    , Just track_id <- [Block.track_id track]
    , track_id `Set.notMember` soloed
    , block_id `Set.member` soloed_blocks
    , track_id `Set.notMember` has_soloed_relatives
    ]
    where
    has_soloed_relatives = Set.fromList (mapMaybe get (Tree.flat_paths tree))
        where
        get (track, parents, children)
            | any (`Set.member` soloed) (map State.track_id children)
                    || any (`Set.member` soloed) (map State.track_id parents) =
                Just (State.track_id track)
            | otherwise = Nothing
    soloed_blocks = Set.fromList
        [ block_id
        | (block_id, block) <- blocks
        , any ((Block.Solo `Set.member`) . Block.track_flags)
            (Block.block_tracks block)
        ]

-- | Similar to the Solo and Mute track flags, individual instruments can be
-- soloed or muted.
filter_instrument_muted :: StateConfig.Allocations -> [Score.Event]
    -> [Score.Event]
filter_instrument_muted (StateConfig.Allocations allocs)
    | not (Set.null soloed) = filter $
        (`Set.member` soloed) . Score.event_instrument
    | not (Set.null muted) = filter $
        (`Set.notMember` muted) . Score.event_instrument
    | otherwise = id
    where
    configs = map (second StateConfig.alloc_config) (Map.toList allocs)
    soloed = Set.fromList $ map fst $ filter (Common.config_solo . snd) configs
    muted = Set.fromList $ map fst $ filter (Common.config_mute . snd) configs

perform_events :: Cmd.M m => Vector.Vector Score.Event -> m Perform.MidiEvents
perform_events events = do
    allocs <- State.gets $ State.config_allocations . State.state_config
    lookup <- get_convert_lookup
    lookup_inst <- Cmd.get_lookup_instrument
    blocks <- State.gets (Map.toList . State.state_blocks)
    tree <- concat <$> mapM (TrackTree.track_tree_of . fst) blocks
    let alloc = Patch.config_allocation <$> midi_configs allocs
    return $ fst $ Perform.perform Perform.initial_state alloc $
        Convert.convert lookup lookup_inst $
        filter_track_muted tree blocks $ filter_instrument_muted allocs $
        -- Performance should be lazy, so converting to a list here means I can
        -- avoid doing work for the notes that never get played.
        Vector.toList events

midi_configs :: StateConfig.Allocations -> Map.Map Score.Instrument Patch.Config
midi_configs (StateConfig.Allocations allocs) = Map.fromAscList
    [ (inst, config)
    | (inst, alloc) <- Map.toAscList allocs
    , Just config <- [midi_config (StateConfig.alloc_backend alloc)]
    ]

midi_config :: StateConfig.Backend -> Maybe Patch.Config
midi_config (StateConfig.Midi a) = Just a
midi_config _ = Nothing

get_convert_lookup :: Cmd.M m => m Convert.Lookup
get_convert_lookup = do
    lookup_scale <- Cmd.gets $ Cmd.config_lookup_scale . Cmd.state_config
    allocs <- State.config#State.allocations_map <#> State.get
    return $ Convert.Lookup
        { lookup_scale = lookup_scale
        , lookup_control_defaults = \inst -> case lookup_config inst allocs of
            Just config -> Score.untyped . Signal.constant <$>
                Patch.config_control_defaults config
            _ -> mempty
        }
    where
    lookup_config inst allocs =
        midi_config . StateConfig.alloc_backend =<< Map.lookup inst allocs
