-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{- | Derivers for control tracks.  That means tempo, control, and pitch.

    Control and pitch tracks can have a 'Derive.Merger'.  If no merger is
    given, they are combined with 'Derive.get_default_merger'.  @set@ will
    replace the signal.  So two tracks named @c@ will multiply, same as if the
    second were @mul c@.  If you want to override @c@ then @set c@ will do
    that.

    Tempo tracks don't support mergers because they are converted into
    a warp, which is then combined via composition.  Pitch tracks normally
    replace each other because adding together absolute pitches is undefined.
    Relative pitches can be added or multiplied, and this is expressed via
    normal controls using transposition signals like 'Controls.chromatic'.
-}
module Derive.Control (
    Config(..)
    , d_control_track
    , track_info
    -- * TrackSignal
    , stash_signal, render_of
#ifdef TESTING
    , derive_control
#endif
) where
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import qualified Util.Log as Log
import qualified Util.Maps as Maps
import qualified Derive.Cache as Cache
import qualified Derive.Call as Call
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Eval as Eval
import qualified Derive.EvalTrack as EvalTrack
import qualified Derive.Expr as Expr
import qualified Derive.LEvent as LEvent
import qualified Derive.PSignal as PSignal
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.Scale as Scale
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Stream as Stream
import qualified Derive.Tempo as Tempo

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import qualified Ui.Block as Block
import qualified Ui.Track as Track
import qualified Ui.TrackTree as TrackTree
import qualified Ui.Types as Types

import           Global
import           Types


data Config = Config {
    -- | True if this is the single topmost track and is a tempo track.
    -- Ultimately it'is used by "Derive.Tempo".
    config_toplevel_tempo :: !Bool
    , config_use_cache :: !Bool
    } deriving (Show)

-- | Top level deriver for control tracks.
d_control_track :: Config -> TrackTree.Track -> Derive.NoteDeriver
    -> Derive.NoteDeriver
d_control_track config track deriver = do
    let title = TrackTree.track_title track
    if Text.all Char.isSpace title then deriver else do
        (ctype, expr) <- either (\err -> Derive.throw $ "track title: " <> err)
            return (ParseTitle.parse_control_title title)
        eval_track config track expr ctype deriver

-- * eval_track

eval_track :: Config -> TrackTree.Track -> [DeriveT.Call]
    -> ParseTitle.ControlType -> Derive.NoteDeriver -> Derive.NoteDeriver
eval_track config track expr ctype deriver = case ctype of
    ParseTitle.Tempo maybe_sym -> do
        is_ly <- Derive.is_lilypond_mode
        let sig_deriver
                | is_ly = return (Signal.constant 1, [])
                | otherwise = with_control_env
                    Controls.tempo (Derive.Merger "compose" (const id) 0)
                    (in_normal_mode $ derive_control True track transform)
        tempo_call config maybe_sym track sig_deriver deriver
    ParseTitle.Control (Right typed_control) maybe_merge -> do
        let control = ScoreT.typed_val typed_control
        merger <- get_merger control maybe_merge
        let sig_deriver = with_control_env control merger
                (derive_control False track transform)
        control_call track typed_control merger sig_deriver deriver
    ParseTitle.Control (Left tcall) maybe_merge -> do
        (typed_control, sig) <- track_call tcall track
        merger <- get_merger (ScoreT.typed_val typed_control) maybe_merge
        control_call track typed_control merger (return (sig, [])) deriver
    ParseTitle.Pitch scale_id pcontrol_tcall -> case pcontrol_tcall of
        Right pcontrol -> pitch_call config track pcontrol scale_id
            transform deriver
        -- TODO have to refactor pitch_call
        Left _tcall -> Derive.throw "unimplemented"
    where
    transform :: Derive.CallableExpr d => Derive.Deriver (Stream.Stream d)
        -> Derive.Deriver (Stream.Stream d)
    transform = Eval.eval_transformers ctx expr
    ctx = Derive.dummy_context 0 (TrackTree.track_end track) $ case ctype of
        ParseTitle.Tempo {} -> "tempo track"
        ParseTitle.Control {} -> "control track"
        ParseTitle.Pitch {} -> "pitch track"

track_call :: Derive.CallableExpr d => Expr.Symbol -> TrackTree.Track
    -> Derive.Deriver (ScoreT.Typed ScoreT.Control, d)
track_call sym track = do
    call <- Eval.get_track_call sym
    Derive.tcall_func call track

-- | Switch 'Derive.RealDurationQuery' to 'Derive.Normal'.  A RealDurationQuery
-- needs to evaluate until the tempo track of the callee block.  But if I leave
-- it in RealDurationQuery mode, the calls on the tempo signal think I am
-- asking about their durations.
in_normal_mode :: Derive.Deriver a -> Derive.Deriver a
in_normal_mode deriver = Derive.get_mode >>= \mode -> case mode of
    Derive.RealDurationQuery -> Internal.local
        (\st -> st { Derive.state_mode = Derive.Normal }) deriver
    _ -> deriver

-- | Get the combining operator for this track.
--
-- 'Controls.null' is used by control calls, and uses 'Derive.Set' by default.
-- Since the control call emits signal which then goes in a control track,
-- a merge operator would wind up being applied twice.
get_merger :: ScoreT.Control -> Maybe Expr.Symbol
    -> Derive.Deriver Derive.Merger
get_merger control merge = case merge of
    Nothing
        | control == Controls.null -> return Derive.Set
        | otherwise -> Derive.get_default_merger control
    Just sym -> Derive.get_control_merge sym

-- | A tempo track is derived like other signals, but in absolute time.
-- Otherwise it would wind up being composed with the environmental warp twice.
tempo_call :: Config -> Maybe Expr.Symbol -> TrackTree.Track
    -> Derive.Deriver (TrackResults Signal.Control)
    -> Derive.NoteDeriver -> Derive.NoteDeriver
tempo_call config sym track sig_deriver deriver = do
    (signal, logs) <- Internal.in_real_time $ do
        (signal, logs) <- Derive.with_val EnvKey.control_gt_0 True sig_deriver
        -- Do this in real time, so 'stash_if_wanted' knows it can directly
        -- reuse the signal.
        stash_if_wanted track signal
        return (signal, logs)

    -- The range is used by the tempo calls to normalize the block to 0--1.
    -- This should only happen for the tempo track at the top of the block.
    range <- maybe (return Nothing) (fmap Just . Internal.block_logical_range) $
        TrackTree.track_block_id track
    -- 'with_damage' must be applied *inside* 'Tempo.with_tempo'.  If it were
    -- outside, it would get the wrong RealTimes when it tried to create the
    -- ControlDamage.
    merge_logs logs $ dispatch_tempo config sym range
        maybe_track_id (Signal.coerce signal) (with_damage deriver)
    where
    maybe_block_track_id = TrackTree.block_track_id track
    maybe_track_id = snd <$> maybe_block_track_id
    with_damage = maybe id get_damage maybe_block_track_id
    get_damage (block_id, track_id) deriver = do
        damage <- Cache.get_tempo_damage block_id track_id
            (TrackTree.track_end track)
            (TrackTree.track_events track)
        Internal.with_control_damage damage deriver

dispatch_tempo :: Monoid a => Config -> Maybe Expr.Symbol
    -> Maybe (ScoreTime, ScoreTime) -> Maybe TrackId -> Signal.Tempo
    -> Derive.Deriver a -> Derive.Deriver a
dispatch_tempo config sym block_range maybe_track_id signal deriver =
    case sym of
        Nothing -> Tempo.with_tempo toplevel block_range maybe_track_id signal
            deriver
        Just sym
            | sym == "hybrid" -> Tempo.with_hybrid toplevel block_range
                maybe_track_id signal deriver
            | sym == "abs" -> Tempo.with_absolute toplevel block_range
                maybe_track_id signal deriver
            | otherwise -> Derive.throw $
                "unknown tempo modifier: " <> ShowVal.show_val sym
    where toplevel = config_toplevel_tempo config

control_call :: TrackTree.Track -> ScoreT.Typed ScoreT.Control -> Derive.Merger
    -- TODO doesn't need to be a Deriver
    -> Derive.Deriver (TrackResults Signal.Control)
    -> Derive.NoteDeriver -> Derive.NoteDeriver
control_call track control merger sig_deriver deriver = do
    (signal, logs) <- sig_deriver
    stash_if_wanted track signal
    -- Apply and strip any control modifications made during the above derive.
    end <- Derive.real $ TrackTree.track_end track
    Derive.eval_control_mods end $ merge_logs logs $ with_damage $
        with_merger control merger signal deriver
    -- I think this forces sequentialness because 'deriver' runs in the state
    -- from the end of 'sig_deriver'.  To make these parallelize, I need to
    -- run sig_deriver as a sub-derive, then mappend the Collect.
    where
    with_damage = with_control_damage
        (TrackTree.block_track_id track) (TrackTree.track_range track)

with_merger :: ScoreT.Typed ScoreT.Control -> Derive.Merger -> Signal.Control
    -> Derive.Deriver a -> Derive.Deriver a
with_merger (ScoreT.Typed typ control) merger signal =
    Derive.with_merged_control merger control (ScoreT.Typed typ signal)

merge_logs :: [Log.Msg] -> Derive.NoteDeriver -> Derive.NoteDeriver
merge_logs logs = fmap (Stream.merge_logs logs)

pitch_call :: Config -> TrackTree.Track -> ScoreT.PControl
    -> Pitch.ScaleId -> (Derive.PitchDeriver -> Derive.PitchDeriver)
    -> Derive.NoteDeriver -> Derive.NoteDeriver
pitch_call config track pcontrol scale_id transform deriver = do
    scale <- get_scale scale_id
    Derive.with_scale scale $ do
        (signal, logs) <- with_pitch_env pcontrol $
            derive_pitch (config_use_cache config) track transform
        -- Ignore errors, they should be logged on conversion.
        (nn_sig, _) <- psignal_to_nn signal
        stash_if_wanted track (Signal.coerce nn_sig)
        -- Apply and strip any control modifications made during the above
        -- derive.
        end <- Derive.real $ TrackTree.track_end track
        Derive.eval_control_mods end $ merge_logs logs $ with_damage $
            Derive.with_named_pitch pcontrol signal deriver
    where
    with_damage = with_control_damage (TrackTree.block_track_id track)
        (TrackTree.track_range track)

get_scale :: Pitch.ScaleId -> Derive.Deriver Scale.Scale
get_scale scale_id
    | scale_id == Pitch.empty_scale = Call.get_scale
    | otherwise = Derive.get_scale scale_id

with_control_damage :: Maybe (BlockId, TrackId) -> (TrackTime, TrackTime)
    -> Derive.Deriver d -> Derive.Deriver d
with_control_damage maybe_block_track_id track_range =
    maybe id get_damage maybe_block_track_id
    where
    get_damage (block_id, track_id) deriver = do
        damage <- Cache.get_control_damage block_id track_id track_range
        Internal.with_control_damage damage deriver

-- | Split the signal chunks and log msgs of the 'LEvent.LEvents' stream.
-- Return signal chunks merged into a signal, the logs cast to Score.Event
-- logs.
type TrackResults sig = (sig, [Log.Msg])

-- | Derive the signal of a control track.
derive_control :: Bool -> TrackTree.Track
    -> (Derive.ControlDeriver -> Derive.ControlDeriver)
    -> Derive.Deriver (TrackResults Signal.Control)
derive_control is_tempo track transform = do
    let track_type
            | is_tempo = ParseTitle.TempoTrack
            | otherwise = ParseTitle.ControlTrack
    (signal, logs) <- derive_track (track_info track track_type)
        (Cache.track track mempty . transform)
    signal <- trim_signal Signal.drop_after Signal.clip_after track signal
    return (signal, logs)

derive_pitch :: Bool -> TrackTree.Track
    -> (Derive.PitchDeriver -> Derive.PitchDeriver)
    -> Derive.Deriver (TrackResults PSignal.PSignal)
derive_pitch use_cache track transform = do
    let cache_track = if use_cache then Cache.track track mempty else id
    (psignal, logs) <- derive_track (track_info track ParseTitle.PitchTrack)
        (cache_track . transform)
    signal <- trim_signal PSignal.drop_after PSignal.clip_after track psignal
    return (signal, logs)

{- | The controls under a note track are intended to apply only to the note
    above them.  However, since signal calls frequently emit samples before,
    "Derive.Slice" includes one event past the end of the slice range.  To
    avoid a sample for the next note getting into this one, I trim off samples
    at and after the end of the slice.  Otherwise, the decay of each note would
    want to change pitch to that of the next note.

    Slices with a zero duration have a special exception that causes them to
    include a sample at the end time, since otherwise they wouldn't have any
    controls.  This also applies to non-zero slices which are nonetheless made
    zero by stretching to 0.
-}
trim_signal :: (RealTime -> sig -> sig)
    -> (RealTime -> sig -> sig) -> TrackTree.Track -> sig
    -> Derive.Deriver sig
trim_signal drop_after clip_after track signal =
    case TrackTree.track_sliced track of
        TrackTree.NotSliced -> return signal
        TrackTree.Inversion ->
            Derive.throw "unexpected Inversion of a control track"
        TrackTree.Sliced orientation -> do
            start <- Derive.real $ TrackTree.track_start track
            end <- Derive.real $ TrackTree.track_end track
            -- I need to keep a signal exactly at end for certain blocks.
            let trim
                    | start == end || orientation == Types.Negative = drop_after
                    | otherwise = clip_after
            return $ trim end signal
    -- TODO(polymorphic-signals)

derive_track :: (Monoid d, Derive.CallableExpr d) => EvalTrack.TrackInfo d
    -> (Derive.Deriver (Stream.Stream d) -> Derive.Deriver (Stream.Stream d))
    -> Derive.Deriver (TrackResults d)
derive_track tinfo transform = do
    stream <- transform $ do
        state <- Derive.get
        let (streams, threaded, collect) =
                EvalTrack.derive_control_track state tinfo
        Internal.merge_collect collect
        Internal.set_threaded threaded
        return $ compact $ concatMap Stream.to_list streams
    let (signal_chunks, logs) = Stream.partition stream
    -- I just merged the signals in 'compact', so this should just convert [x]
    -- to x.
    return (mconcat signal_chunks, logs)
    where
    -- Merge the signal here so it goes in the cache as one signal event.
    -- I can use concat instead of merge_asc_events because the signals
    -- will be merged with Signal.merge and the logs extracted.
    compact events = Stream.from_event_logs (mconcat sigs) logs
        where (sigs, logs) = LEvent.partition events

-- | Make a TrackInfo for control tracks.
track_info :: Monoid d => TrackTree.Track -> ParseTitle.Type
    -> EvalTrack.TrackInfo d
track_info track track_type = EvalTrack.TrackInfo
    { tinfo_track = track
    , tinfo_sub_tracks = []
    , tinfo_type = track_type
    , tinfo_get_last_val = last_signal_val
    }

last_signal_val :: Monoid d => EvalTrack.GetLastVal d
last_signal_val xs
    | null xs = Nothing
    | otherwise = Just (mconcat xs)

-- * TrackSignal

-- | If this track is to be rendered by the UI, stash the given signal in
-- either 'Derive.collect_track_signals' or 'Derive.collect_signal_fragments'.
stash_if_wanted :: TrackTree.Track -> Signal.Control -> Derive.Deriver ()
stash_if_wanted track sig = whenM is_normal_mode $
    whenJustM (render_of track) $ \(block_id, track_id, _) ->
        case TrackTree.track_sliced track of
            TrackTree.NotSliced -> stash_signal block_id track_id sig
            _ -> stash_signal_fragment block_id track_id
                -- The start of the range may not be unique in the case of
                -- orphans, I think because they aren't shifted to 0.  But
                -- the end should be unique.
                (snd $ TrackTree.track_range track) sig

is_normal_mode :: Derive.Deriver Bool
is_normal_mode = Derive.get_mode >>= \mode -> return $ case mode of
    Derive.Normal -> True
    _ -> False

stash_signal_fragment :: BlockId -> TrackId -> TrackTime -> Signal.Control
    -> Derive.Deriver ()
stash_signal_fragment block_id track_id slice_end sig =
    -- TODO I think this is faster than Internal.merge_collect, but I haven't
    -- profiled so I don't know
    Internal.modify_collect $ \collect -> collect
        { Derive.collect_signal_fragments =
            Map.alter insert (block_id, track_id) $
                Derive.collect_signal_fragments collect
        }
    where
    -- If a control is constant over multiple inverted notes, I'll get a bunch
    -- of identical signal fragments.  Rather than waiting for merge to
    -- eliminate them, it seems a bit more efficient to not collect them in the
    -- first place.
    insert (Just old) = Just $ case Maps.max old of
        Just (_, prev) | sig == prev -> old
        _ -> Map.insert slice_end sig old
    insert _ = Just $ Map.singleton slice_end sig

stash_signal :: BlockId -> TrackId -> Signal.Control -> Derive.Deriver ()
stash_signal block_id track_id sig = do
    warp <- Internal.get_warp
    put_track_signal block_id track_id $ EvalTrack.unwarp warp sig

put_track_signal :: BlockId -> TrackId -> Track.TrackSignal -> Derive.Deriver ()
put_track_signal block_id track_id tsig = Internal.merge_collect $ mempty
    { Derive.collect_track_signals = Map.singleton (block_id, track_id) tsig }

-- | Get render information if this track wants a TrackSignal.
render_of :: TrackTree.Track
    -> Derive.Deriver (Maybe (BlockId, TrackId, Maybe Track.RenderSource))
render_of track = case TrackTree.block_track_id track of
    Nothing -> return Nothing
    Just (block_id, track_id) -> ifM not_root (return Nothing) $ do
        (btrack, track) <- get_block_track block_id track_id
        let flags = Block.track_flags btrack
        return $ if Block.track_wants_signal flags track
            then Just (block_id, track_id,
                extract (Track.render_style (Track.track_render track)))
            else Nothing
    where
    not_root = not <$> Internal.is_root_block
    extract (Track.Line (Just source)) = Just source
    extract (Track.Filled (Just source)) = Just source
    extract _ = Nothing

get_block_track :: BlockId -> TrackId
    -> Derive.Deriver (Block.Track, Track.Track)
get_block_track block_id track_id = do
    track <- Derive.get_track track_id
    block <- Derive.get_block block_id
    btrack <- Derive.require
        ("get_block_track: " <> showt block_id <> " doesn't have "
            <> showt track_id) $
        List.find ((== Just track_id) . Block.track_id)
            (Block.block_tracks block)
    return (btrack, track)


-- * util

-- | Reduce a 'PSignal.PSignal' to raw note numbers, taking the current
-- transposition environment into account.
psignal_to_nn :: PSignal.PSignal
    -> Derive.Deriver (Signal.NoteNumber, [(RealTime, PSignal.PitchError)])
psignal_to_nn sig = do
    controls <- Internal.get_dynamic Derive.state_controls
    return $ PSignal.to_nn $ PSignal.apply_controls controls sig

with_control_env :: ScoreT.Control -> Derive.Merger -> Derive.Deriver a
    -> Derive.Deriver a
with_control_env control merger = Derive.with_vals
    [ (EnvKey.control, ScoreT.control_name control)
    , (EnvKey.merge, ShowVal.show_val merger)
    ]

with_pitch_env :: ScoreT.PControl -> Derive.Deriver a -> Derive.Deriver a
with_pitch_env = Derive.with_val EnvKey.control . ScoreT.pcontrol_name
