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
    -- * mute and solo
    , get_muted_tracks, muted_instruments
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
import qualified Ui.TrackTree as TrackTree
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig

import qualified Cmd.Cmd as Cmd
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.C.Prelude.Block as Prelude.Block
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
    , (EnvKey.scale, BaseTypes.str Config.default_scale_id)
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
derive_block cache damage block_id =
    fmap Derive.extract_result $ run cache damage $ do
        unless (damage == mempty) $
            Log.debug $ "score damage for " <> showt block_id <> ": "
                <> pretty damage
        Prelude.Block.eval_root_block block_id

is_score_damage_log :: Log.Msg -> Bool
is_score_damage_log = ("score damage for " `Text.isPrefixOf`) . Log.msg_text

run :: Cmd.M m => Derive.Cache -> Derive.ScoreDamage -> Derive.Deriver a
    -> m (Derive.RunResult a)
run cache damage deriver = do
    ui_state <- Ui.get
    (constant, aliases) <- get_constant ui_state cache damage
    return $ Derive.derive constant (initial_dynamic aliases) deriver

-- | Run a derivation when you already know the Dynamic.  This is the case when
-- deriving at a certain point in the score via the TrackDynamic.
run_with_dynamic :: Cmd.M m => Derive.Dynamic -> Derive.Deriver a
    -> m (Derive.RunResult a)
run_with_dynamic dynamic deriver = do
    ui_state <- Ui.get
    -- Trust they already put the ky aliases in.
    (constant, _aliases) <- get_constant ui_state mempty mempty
    let state = Derive.State
            { state_threaded = Derive.initial_threaded
            , state_dynamic = dynamic
            , state_collect = mempty
            , state_constant = constant
            }
    return $ Derive.run state deriver

-- | Create deriver configuration.  This is the main place where Cmd level
-- configuration is adapted to the deriver.
get_constant :: Cmd.M m => Ui.State -> Derive.Cache -> Derive.ScoreDamage
    -> m (Derive.Constant, Derive.InstrumentAliases)
get_constant ui_state cache damage = do
    cmd_state <- Cmd.get
    let lookup_inst = Cmd.state_resolve_instrument ui_state cmd_state
    config_builtins <- Cmd.gets (Cmd.config_builtins . Cmd.state_config)
    (defs_builtins, aliases) <- get_builtins
    return $ (,aliases) $ Derive.initial_constant ui_state
        (defs_builtins <> config_builtins) Cmd.lookup_scale
        (fmap Cmd.make_derive_instrument . lookup_inst) cache damage

-- | Get Builtins from the cache.
get_builtins :: Cmd.M m => m (Derive.Builtins, Derive.InstrumentAliases)
get_builtins = do
    cache <- Cmd.gets Cmd.state_ky_cache
    case cache of
        Nothing -> return mempty
        Just (Cmd.KyCache (Left err) _) -> Cmd.throw $ "parsing ky: " <> err
        Just (Cmd.KyCache (Right builtins) _) -> return builtins
        Just (Cmd.PermanentKy builtins) -> return builtins

initial_dynamic :: Derive.InstrumentAliases -> Derive.Dynamic
initial_dynamic aliases = (Derive.initial_dynamic initial_environ)
    { Derive.state_instrument_aliases = aliases }

perform_from :: Cmd.M m => RealTime -> Cmd.Performance -> m Perform.MidiEvents
perform_from start perf = do
    insts <- Map.keys <$> (Ui.config#Ui.allocations_map <#> Ui.get)
    resume_insts <- Set.fromList <$> filterM (has_flag Patch.ResumePlay) insts
    let (extra, events) = events_from resume_insts start $ Cmd.perf_events perf
    perform_events_list (extra ++ Vector.toList events)

has_flag :: Cmd.M m => Patch.Flag -> Score.Instrument -> m Bool
has_flag flag inst =
    maybe False (`Patch.has_flag` flag) <$> Cmd.lookup_midi_config inst

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
events_from :: Set Score.Instrument -- ^ scan back for starts of these
    -> RealTime -> Vector.Vector Score.Event
    -> ([Score.Event], Vector.Vector Score.Event)
    -- ^ (extra events before start, events from start)
events_from resume_insts start events
    | start <= 0 = ([], events)
    | otherwise = (starts, Vector.drop index events)
    where
    index = Util.Vector.lowest_index Score.event_start (start - RealTime.eta)
        events
    starts = scan_for_starts default_scan_back resume_insts events start index

-- | Look back from the play start time by this much.  This is convenient
-- because randomization can move events back from the ruler mark, and it's
-- annoying when play misses them.
default_scan_back :: RealTime
default_scan_back = 0.075

-- | Starting from the index, look back for overlapping events in the given set.
scan_for_starts :: RealTime -> Set Score.Instrument -> Vector.Vector Score.Event
    -> RealTime -> Int -> [Score.Event]
scan_for_starts scan_back resume_insts events pos index =
    reverse $ mapMaybe (set_start scan_back pos) $
        scan (resume_insts `Set.difference` present_here) back
    where
    here = Vector.takeWhile ((==pos) . Score.event_start) $
        Vector.drop index events
    present_here = Vector.foldl' (\s e -> Set.insert (inst e) s) mempty here
    back = Util.Vector.to_reverse_list $ Vector.take index events
    scan _ [] = []
    scan !insts (e:es)
        | Score.event_start e >= until = e : scan insts2 es
        | Set.null insts = []
        | inst e `Set.member` insts = e : scan insts2 es
        | otherwise = scan insts es
        where insts2 = Set.delete (inst e) insts
    until = pos - scan_back
    inst = Score.event_instrument

set_start :: RealTime -> RealTime -> Score.Event -> Maybe Score.Event
set_start scan_back pos event
    | Score.event_start event >= pos - scan_back = Just event
    | dur <= 0 = Nothing
    | otherwise =
        Just $ event { Score.event_start = pos, Score.event_duration = dur }
    where dur = Score.event_end event - pos

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

perform_events :: Cmd.M m => Vector.Vector Score.Event -> m Perform.MidiEvents
perform_events = perform_events_list . Vector.toList
    -- Performance should be lazy, so converting to a list here means I can
    -- avoid doing work for the notes that never get played.

perform_events_list :: Cmd.M m => [Score.Event] -> m Perform.MidiEvents
perform_events_list events = do
    allocs <- Ui.gets $ Ui.config_allocations . Ui.state_config
    lookup <- get_convert_lookup
    lookup_inst <- Cmd.get_lookup_instrument
    muted <- get_muted_tracks
    let alloc = Perform.config <$> midi_configs allocs
    return $ fst $ Perform.perform Perform.initial_state alloc $
        Convert.convert lookup lookup_inst $
        filter_track_muted muted $ filter_instrument_muted allocs events

-- | Similar to the Solo and Mute track flags, individual instruments can be
-- soloed or muted.
filter_instrument_muted :: UiConfig.Allocations -> [Score.Event]
    -> [Score.Event]
filter_instrument_muted allocs =
    filter ((`Set.notMember` muted) . Score.event_instrument)
    where muted = muted_instruments allocs

muted_instruments :: UiConfig.Allocations -> Set Score.Instrument
muted_instruments (UiConfig.Allocations allocs)
    | not (null soloed) = instruments Set.\\ Set.fromList soloed
    | otherwise = Set.fromList muted
    where
    configs = map (second UiConfig.alloc_config) (Map.toList allocs)
    instruments = Set.fromList $ map fst configs
    soloed = map fst $ filter (Common.config_solo . snd) configs
    muted = map fst $ filter (Common.config_mute . snd) configs

-- | Filter events according to the Solo and Mute flags in the tracks of the
-- given blocks.
--
-- Solo only applies to the block on which the track is soloed.  So if you solo
-- a track on one block, other blocks will still play.
--
-- Solo takes priority over Mute.
filter_track_muted :: Set TrackId -> [Score.Event] -> [Score.Event]
filter_track_muted muted
    | Set.null muted = id
    | otherwise = filter (not . stack_contains muted)
    where
    stack_contains track_ids = any (`Set.member` track_ids) . stack_tracks
    stack_tracks = mapMaybe Stack.track_of . Stack.innermost . Score.event_stack

get_muted_tracks :: Ui.M m => m (Set TrackId)
get_muted_tracks = do
    blocks <- Ui.gets (Map.toList . Ui.state_blocks)
    tree <- concat <$> mapM (TrackTree.track_tree_of . fst) blocks
    return $ muted_tracks tree blocks

muted_tracks :: TrackTree.TrackTree -> [(BlockId, Block.Block)] -> Set TrackId
muted_tracks tree blocks
    | not (Set.null soloed) = solo_to_mute tree blocks soloed
    | otherwise = muted
    where
    soloed = with_flag Block.Solo
    muted = with_flag Block.Mute
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
    -> [(BlockId, Block.Block)] -> Set TrackId -> Set TrackId
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
            | any (`Set.member` soloed) (map Ui.track_id children)
                    || any (`Set.member` soloed) (map Ui.track_id parents) =
                Just (Ui.track_id track)
            | otherwise = Nothing
    soloed_blocks = Set.fromList
        [ block_id
        | (block_id, block) <- blocks
        , any ((Block.Solo `Set.member`) . Block.track_flags)
            (Block.block_tracks block)
        ]

midi_configs :: UiConfig.Allocations -> Map Score.Instrument Patch.Config
midi_configs (UiConfig.Allocations allocs) = Map.fromAscList
    [ (inst, config)
    | (inst, alloc) <- Map.toAscList allocs
    , Just config <- [midi_config (UiConfig.alloc_backend alloc)]
    ]

midi_config :: UiConfig.Backend -> Maybe Patch.Config
midi_config (UiConfig.Midi a) = Just a
midi_config _ = Nothing

get_convert_lookup :: Cmd.M m => m Convert.Lookup
get_convert_lookup = do
    allocs <- Ui.config#Ui.allocations_map <#> Ui.get
    return $ Convert.Lookup
        { lookup_scale = Cmd.lookup_scale
        , lookup_control_defaults = \inst -> case lookup_config inst allocs of
            Just config -> Score.untyped . Signal.constant <$>
                Patch.config_control_defaults config
            _ -> mempty
        }
    where
    lookup_config inst allocs =
        midi_config . UiConfig.alloc_backend =<< Map.lookup inst allocs
