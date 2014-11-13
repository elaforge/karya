-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{- | Derivers for control tracks.  That means tempo, control, and pitch.

    Control tracks (specifically control tracks, not tempo or pitch) can have
    a combining operator.  If no operator is given, they are combined with
    @mul@.  @set@ will replace the signal.  So two tracks named @c@ will
    multiply, same as if the second were @mul c@.  If you want to override @c@
    then @set c@ will do that.

    A control with a combining operator but nothing to combine with should still
    do something sensible because operators come with an identity value, e.g.
    @1@ for @mul@ and @0@ for @add@.

    Tempo tracks don't support operators because they are converted into
    a warp, which is then combined via composition.  Pitch tracks always
    replace each other because adding together absolute pitches is undefined.
    Relative pitches can be added or multiplied, and this is expressed via
    normal controls using transposition signals like 'Controls.chromatic'.
-}
module Derive.Control (
    d_control_track
    -- * TrackSignal
    , stash_signal, render_of
#ifdef TESTING
    , derive_control
#endif
) where
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Monoid as Monoid
import qualified Data.Text as Text
import qualified Data.Tree as Tree

import qualified Util.Log as Log
import qualified Util.Map
import qualified Ui.Block as Block
import qualified Ui.Events as Events
import qualified Ui.Track as Track
import qualified Ui.TrackTree as TrackTree

import qualified Derive.Cache as Cache
import qualified Derive.Call.Util as Util
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Environ as Environ
import qualified Derive.Eval as Eval
import qualified Derive.EvalTrack as EvalTrack
import qualified Derive.LEvent as LEvent
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Scale as Scale
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Tempo as Tempo
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import Global
import Types


-- | Top level deriver for control tracks.
d_control_track :: TrackTree.EventsNode
    -> Derive.NoteDeriver -> Derive.NoteDeriver
d_control_track (Tree.Node track _) deriver = do
    let title = TrackTree.track_title track
    if Text.all Char.isSpace title then deriver else do
        (ctype, expr) <- either (\err -> Derive.throw $ "track title: " ++ err)
            return (ParseTitle.parse_control_expr title)
        eval_track track expr ctype deriver

-- * eval_track

eval_track :: TrackTree.Track -> [TrackLang.Call]
    -> ParseTitle.ControlType -> Derive.NoteDeriver -> Derive.NoteDeriver
eval_track track expr ctype deriver = case ctype of
    ParseTitle.Tempo maybe_sym -> do
        is_ly <- Derive.is_lilypond_derive
        let sig_deriver
                | is_ly = return (Signal.constant 1, [])
                | otherwise = with_control_env Controls.tempo "compose" $
                    derive_control True track transform
        tempo_call maybe_sym track sig_deriver deriver
    ParseTitle.Control maybe_op control -> do
        let control_name = Score.typed_val control
        merge <- lookup_merge control_name maybe_op
        let sig_deriver = with_control_env control_name (merge_name merge) $
                derive_control False track transform
        control_call track control merge sig_deriver deriver
    ParseTitle.Pitch scale_id maybe_name ->
        pitch_call track maybe_name scale_id transform deriver
    where
    transform :: Derive.Callable d => Derive.LogsDeriver d
        -> Derive.LogsDeriver d
    transform = Eval.eval_transformers cinfo expr
    cinfo = Derive.dummy_call_info 0 (TrackTree.track_end track) $ case ctype of
        ParseTitle.Tempo {} -> "tempo track"
        ParseTitle.Control {} -> "control track"
        ParseTitle.Pitch {} -> "pitch track"

merge_name :: Derive.Merge -> Text
merge_name Derive.Set = "set"
merge_name (Derive.Merge (Derive.ControlOp name _)) = name

-- | Get the combining operator for this track.
--
-- 'Controls.null' is used by control calls, and uses 'Derive.Set' by default.
-- Since the control call emits signal which then goes in a control track,
-- a merge operator would wind up being applied twice.
lookup_merge :: Score.Control -> Maybe TrackLang.CallId
    -> Derive.Deriver Derive.Merge
lookup_merge control op = case op of
    Nothing
        | control == Controls.null -> return Derive.Set
        | otherwise -> Derive.get_default_merge control
    Just sym -> Derive.get_merge sym

-- | A tempo track is derived like other signals, but in absolute time.
-- Otherwise it would wind up being composed with the environmental warp twice.
tempo_call :: Maybe TrackLang.Symbol -> TrackTree.Track
    -> Derive.Deriver (TrackResults Signal.Control)
    -> Derive.NoteDeriver -> Derive.NoteDeriver
tempo_call sym track sig_deriver deriver = do
    (signal, logs) <- Internal.in_real_time $ do
        (signal, logs) <- sig_deriver
        -- Do this in real time, so 'stash_if_wanted' knows it can directly
        -- reuse the signal.
        stash_if_wanted track signal
        return (signal, logs)

    -- 'with_damage' must be applied *inside* 'd_tempo'.  If it were outside,
    -- it would get the wrong RealTimes when it tried to create the
    -- ControlDamage.
    merge_logs logs $ dispatch_tempo sym (TrackTree.track_end track)
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

dispatch_tempo :: Maybe TrackLang.Symbol -> ScoreTime -> Maybe TrackId
    -> Signal.Tempo -> Derive.Deriver a -> Derive.Deriver a
dispatch_tempo sym block_dur maybe_track_id signal deriver = case sym of
    Nothing -> Tempo.with_tempo block_dur maybe_track_id signal deriver
    Just sym
        | sym == "hybrid" ->
            Tempo.with_hybrid block_dur maybe_track_id signal deriver
        | sym == "abs" ->
            Tempo.with_absolute block_dur maybe_track_id signal deriver
        | otherwise -> Derive.throw $
            "unknown tempo modifier: " <> untxt (ShowVal.show_val sym)

control_call :: TrackTree.Track -> Score.Typed Score.Control
    -> Derive.Merge -> (Derive.Deriver (TrackResults Signal.Control))
    -> Derive.NoteDeriver -> Derive.NoteDeriver
control_call track control merge control_deriver deriver = do
    (signal, logs) <- Internal.track_setup track control_deriver
    stash_if_wanted track signal
    -- Apply and strip any control modifications made during the above derive.
    end <- Derive.real $ TrackTree.track_end track
    Derive.eval_control_mods end $ merge_logs logs $ with_damage $
        with_control_op control merge signal deriver
    -- I think this forces sequentialness because 'deriver' runs in the state
    -- from the end of 'control_deriver'.  To make these parallelize, I need to
    -- run control_deriver as a sub-derive, then mappend the Collect.
    where
    with_damage = with_control_damage
        (TrackTree.block_track_id track) (TrackTree.track_range track)

with_control_op :: Score.Typed Score.Control -> Derive.Merge -> Signal.Control
    -> Derive.Deriver a -> Derive.Deriver a
with_control_op (Score.Typed typ control) merge signal =
    Derive.with_merged_control merge control (Score.Typed typ signal)

merge_logs :: [Log.Msg] -> Derive.NoteDeriver -> Derive.NoteDeriver
merge_logs logs deriver = do
    events <- deriver
    return $ Derive.merge_events (map LEvent.Log logs) events

pitch_call :: TrackTree.Track -> Maybe Score.Control -> Pitch.ScaleId
    -> (Derive.PitchDeriver -> Derive.PitchDeriver)
    -> Derive.NoteDeriver -> Derive.NoteDeriver
pitch_call track maybe_name scale_id transform deriver =
    Internal.track_setup track $ do
        scale <- get_scale scale_id
        Derive.with_scale scale $ do
            (signal, logs) <- derive_pitch True track transform
            -- Ignore errors, they should be logged on conversion.
            (nn_sig, _) <- pitch_signal_to_nn signal
            stash_if_wanted track (Signal.coerce nn_sig)
            -- Apply and strip any control modifications made during the above
            -- derive.
            end <- Derive.real $ TrackTree.track_end track
            Derive.eval_control_mods end $ merge_logs logs $ with_damage $
                Derive.with_pitch maybe_name signal deriver
    where
    with_damage = with_control_damage (TrackTree.block_track_id track)
        (TrackTree.track_range track)

get_scale :: Pitch.ScaleId -> Derive.Deriver Scale.Scale
get_scale scale_id
    | scale_id == Pitch.empty_scale = Util.get_scale
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
    (signal, logs) <- derive_track track track_type
        last_signal_val (Cache.track track mempty . transform)
    signal <- extend
        =<< trim_signal Signal.drop_after Signal.drop_at_after track signal
    return (signal, logs)
    where
    -- This is a special hack just for tempo tracks, documented by
    -- 'Tempo.extend_signal'.
    extend signal
        | is_tempo = do
            end <- Derive.real (TrackTree.track_end track)
            return $ Signal.coerce $ Tempo.extend_signal end $
                Signal.coerce signal
        | otherwise = return signal

derive_pitch :: Bool -> TrackTree.Track
    -> (Derive.PitchDeriver -> Derive.PitchDeriver)
    -> Derive.Deriver (TrackResults PitchSignal.Signal)
derive_pitch cache track transform = do
    let cache_track = if cache then Cache.track track mempty else id
    (signal, logs) <- derive_track track ParseTitle.PitchTrack last_signal_val
        (cache_track . transform)
    signal <- trim_signal PitchSignal.drop_after PitchSignal.drop_at_after
        track signal
    return (signal, logs)

-- | The controls under a note track are intended to apply only to the note
-- above them.  However, since signal calls frequently emit samples before,
-- "Derive.Slice" includes one event past the end of the slice range.  To avoid
-- a sample for the next note getting into this one, I trim off samples at and
-- after the end of the slice.  Otherwise, the decay of each note would want to
-- change pitch to that of the next note.
--
-- Slices with a zero duration have a special exception that causes them to
-- include a sample at the end time, since otherwise they wouldn't have
-- any controls.  This also applies to non-zero slices which are nonetheless
-- made zero by stretching to 0.
trim_signal :: (RealTime -> sig -> sig) -> (RealTime -> sig -> sig)
    -> TrackTree.Track -> sig -> Derive.Deriver sig
trim_signal drop_after drop_at_after track signal
    | TrackTree.track_sliced track = do
        start <- Derive.real $
            Events.time_begin (TrackTree.track_events track)
        end <- Derive.real $ TrackTree.track_end track
        return $ (if start == end then drop_after else drop_at_after) end signal
    | otherwise = return signal

derive_track :: (Monoid.Monoid d, Derive.Callable d) => TrackTree.Track
    -> ParseTitle.Type -> EvalTrack.GetLastVal d
    -> (Derive.LogsDeriver d -> Derive.LogsDeriver d)
    -> Derive.Deriver (TrackResults d)
derive_track track track_type get_last_val transform = do
    stream <- transform $ do
        state <- Derive.get
        let (stream, threaded, collect) =
                EvalTrack.derive_control_track state tinfo
        Internal.merge_collect collect
        Internal.set_threaded threaded
        return $ compact (concat stream)
    let (signal_chunks, logs) = LEvent.partition stream
    -- I just merged the signals in 'compact', so this should just convert [x]
    -- to x.
    return (mconcat signal_chunks, logs)
    where
    tinfo = EvalTrack.TrackInfo
        { EvalTrack.tinfo_track = track
        , EvalTrack.tinfo_sub_tracks = []
        , EvalTrack.tinfo_type = track_type
        , EvalTrack.tinfo_get_last_val = get_last_val
        }
    -- Merge the signal here so it goes in the cache as one signal event.
    -- I can use concat instead of merge_asc_events because the signals
    -- will be merged with Signal.merge and the logs extracted.
    compact events = LEvent.Event (mconcat sigs) : map LEvent.Log logs
        where (sigs, logs) = LEvent.partition events

last_signal_val :: Monoid.Monoid d => EvalTrack.GetLastVal d
last_signal_val xs
    | null xs = Nothing
    | otherwise = Just (mconcat xs)

-- * TrackSignal

-- | If this track is to be rendered by the UI, stash the given signal in
-- either 'Derive.collect_track_signals' or 'Derive.collect_signal_fragments'.
stash_if_wanted :: TrackTree.Track -> Signal.Control -> Derive.Deriver ()
stash_if_wanted track sig =
    whenJustM (render_of track) $ \(block_id, track_id, _) ->
        if TrackTree.track_sliced track
            then stash_signal_fragment block_id track_id
                -- The start of the range may not be unique in the case of
                -- orphans, I think because they aren't shifted to 0.  But
                -- the end should be unique.
                (snd $ TrackTree.track_range track) sig
            else stash_signal block_id track_id sig

stash_signal_fragment :: BlockId -> TrackId -> TrackTime -> Signal.Control
    -> Derive.Deriver ()
stash_signal_fragment block_id track_id pos sig =
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
    insert (Just old) = Just $ case Util.Map.max old of
        Just (_, prev) | sig == prev -> old
        _ -> Map.insert pos sig old
    insert _ = Just $ Map.singleton pos sig

stash_signal :: BlockId -> TrackId -> Signal.Control -> Derive.Deriver ()
stash_signal block_id track_id sig = do
    warp <- Internal.get_dynamic Derive.state_warp
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
        return $ if Block.wants_track_signal flags track
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
        ("get_block_track: " <> show block_id <> " doesn't have "
            <> show track_id) $
        List.find ((== Just track_id) . Block.track_id)
            (Block.block_tracks block)
    return (btrack, track)


-- * util

-- | Reduce a 'PitchSignal.Signal' to raw note numbers, taking the current
-- transposition environment into account.
pitch_signal_to_nn :: PitchSignal.Signal
    -> Derive.Deriver (Signal.NoteNumber, [PitchSignal.PitchError])
pitch_signal_to_nn sig = do
    controls <- Internal.get_dynamic Derive.state_controls
    environ <- Internal.get_environ
    return $ PitchSignal.to_nn $ PitchSignal.apply_controls environ controls sig

with_control_env :: Score.Control -> Text -> Derive.Deriver a
    -> Derive.Deriver a
with_control_env control merge =
    Derive.with_val Environ.control (Score.control_name control)
    . Derive.with_val Environ.merge merge
