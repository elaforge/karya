-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

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
module Derive.Control where
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Tree as Tree

import Util.Control
import qualified Util.Log as Log
import qualified Util.Seq as Seq

import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Track as Track
import qualified Ui.TrackTree as TrackTree

import qualified Derive.Cache as Cache
import qualified Derive.Call as Call
import qualified Derive.Call.Util as Util
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Environ as Environ
import qualified Derive.LEvent as LEvent
import qualified Derive.ParseBs as ParseBs
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Scale as Scale
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Tempo as Tempo
import qualified Derive.TrackInfo as TrackInfo
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import Types


type Pitch = PitchSignal.Signal
type Control = Signal.Control

-- | As returned by 'TrackTree.tevents_range', happpens to be used a lot here.
type TrackRange = (ScoreTime, ScoreTime)

-- | Top level deriver for control tracks.
d_control_track :: TrackTree.EventsNode
    -> Derive.NoteDeriver -> Derive.NoteDeriver
d_control_track (Tree.Node track _) deriver = do
    let title = TrackTree.tevents_title track
    if Text.all Char.isSpace title then deriver else do
    (ctype, expr) <- either (\err -> Derive.throw $ "track title: " ++ err)
        return (TrackInfo.parse_control_expr title)
    eval_track track expr ctype deriver

-- | Preprocess a track tree by splitting the control tracks.  An event
-- starting with @%@ splits the events below it into a new control track.  The
-- text after the @%@ becomes the new track title.  Split tracks with the same
-- title will be merged together, but they have to have exactly the same title.
-- If they have different titles but create the same control, they will wind up
-- in separate tracks and likely the last one will win, due to the implicit
-- leading 0 sample in each control track.
--
-- Preprocessing at the track tree level means the split tracks act like real
-- tracks and interact with slicing correctly.
--
-- This is experimental.  It provides a way to have short ad-hoc control
-- sections, which is likely to be convenient for individual calls that can use
-- a control signal.  On the other hand, it invisibly rearranges the score, and
-- that has been a real mess with slicing.  Hopefully it's simple enough that
-- it won't be as confusing as slicing and inversion.
--
-- TrackSignals should work out because the split tracks all have the same
-- TrackId, and so their individual signal fragments should be joined by
-- 'Track.merge_signals'.
split_control_tracks :: TrackTree.EventsTree -> TrackTree.EventsTree
split_control_tracks = map split
    where
    split (Tree.Node track subs) =
        case TrackInfo.track_type (TrackTree.tevents_title track) of
            TrackInfo.ControlTrack -> splice (split_control track) subs
            _ -> Tree.Node track (map split subs)
        where
        splice [] subs = Tree.Node track subs
        splice [x] subs = Tree.Node x subs
        splice (x:xs) subs = Tree.Node x [splice xs subs]

-- | Look for events starting with @%@, and split the track at each one.
-- Each split-off track is titled with the text after the @%@.
split_control :: TrackTree.TrackEvents -> [TrackTree.TrackEvents]
split_control track = extract $ split $ TrackTree.tevents_events track
    where
    split events = go (TrackTree.tevents_title track) events $
        mapMaybe switch_control $ Events.ascending events
    go title events [] = [(title, events)]
    go title events ((start, next_title) : switches) =
        (title, pre) : go next_title post switches
        where (pre, post) = Events.split_at_exclude start events
    switch_control event
        | Just ('%', title) <- Char8.uncons (Event.event_bytestring event),
            not (Char8.null title) =
                Just (Event.start event, ParseBs.to_text title)
        | otherwise = Nothing
    extract [] = [track]
    extract [_] = [track]
    extract tracks = map convert (merge tracks)
    convert (title,  events) = track
        { TrackTree.tevents_title = title
        , TrackTree.tevents_events = events
        }
    -- Tracks with the same name are merged back together.
    -- TODO Group by control, not title.
    merge = map merge_track . Seq.group_on fst
    merge_track [] = error "Seq.group_on postcondition violated"
    merge_track tracks@((title, _) : _) = (title, mconcat (map snd tracks))

-- * eval_track

eval_track :: TrackTree.TrackEvents -> [TrackLang.Call]
    -> TrackInfo.ControlType -> Derive.NoteDeriver -> Derive.NoteDeriver
eval_track track expr ctype deriver = case ctype of
    TrackInfo.Tempo maybe_sym -> ifM Derive.is_lilypond_derive deriver $
        tempo_call maybe_sym track
            (with_control_env Controls.tempo $
                derive_control True True tempo_track expr)
            deriver
    TrackInfo.Control maybe_op control -> do
        merge <- lookup_op (Score.typed_val control) maybe_op
        control_call track control merge
            (\cache -> with_control_env (Score.typed_val control) $
                derive_control cache False track expr)
            deriver
    TrackInfo.Pitch scale_id maybe_name ->
        pitch_call track maybe_name scale_id expr deriver
    where
    tempo_track = track { TrackTree.tevents_events = tempo_events }
    -- This is a hack due to the way the tempo track works.  Further notes
    -- are on the 'Perform.Signal.integrate' doc.
    tempo_events
        | Maybe.isNothing (Events.at (snd track_range) evts) =
            Events.insert [Event.event (snd track_range) 0 "set-prev"] evts
        | otherwise = evts
        where
        track_range = TrackTree.tevents_range track
        evts = TrackTree.tevents_events track

-- | Get the combining operator for this track.
--
-- 'Controls.null' is used by control calls, and uses 'Derive.Set' by default
-- instead of 'Derive.Default'.  Since the control call emits signal which then
-- goes in a control track, it would lead to multiplication being applied
-- twice.  In addition, applying a relative signal tends to create a leading
-- 0 sample, which then causes control calls to wipe out previous samples.
lookup_op :: Score.Control -> Maybe TrackLang.CallId
    -> Derive.Deriver Derive.Merge
lookup_op control op = case op of
    Nothing
        | control == Controls.null -> return Derive.Set
        | otherwise -> return Derive.Default
    Just sym
        | sym == "set" -> return Derive.Set
        | otherwise -> Derive.Merge <$> Derive.get_control_op sym

-- | A tempo track is derived like other signals, but in absolute time.
-- Otherwise it would wind up being composed with the environmental warp twice.
tempo_call :: Maybe TrackLang.Symbol -> TrackTree.TrackEvents
    -> Derive.Deriver (TrackResults Signal.Control)
    -> Derive.NoteDeriver -> Derive.NoteDeriver
tempo_call sym track sig_deriver deriver = do
    (signal, logs) <- Internal.in_real_time sig_deriver
    whenJust maybe_block_track_id $ \(block_id, track_id) ->
        unless (TrackTree.tevents_sliced track) $
            -- I don't need stash_signal because this is definitely in
            -- TrackTime.
            put_track_signal block_id track_id $
                Track.TrackSignal (Signal.coerce signal) 0 1 False
    -- 'with_damage' must be applied *inside* 'd_tempo'.  If it were outside,
    -- it would get the wrong RealTimes when it tried to create the
    -- ControlDamage.
    merge_logs logs $ dispatch_tempo sym (snd track_range) maybe_track_id
        (Signal.coerce signal) (with_damage deriver)
    where
    maybe_block_track_id = TrackTree.tevents_block_track_id track
    maybe_track_id = snd <$> maybe_block_track_id
    with_damage = maybe id get_damage maybe_block_track_id
    get_damage (block_id, track_id) deriver = do
        damage <- Cache.get_tempo_damage block_id track_id track_range
            (TrackTree.tevents_events track)
        Internal.with_control_damage damage deriver
    track_range = TrackTree.tevents_range track

dispatch_tempo :: Maybe TrackLang.Symbol -> ScoreTime -> Maybe TrackId
    -> Signal.Tempo -> Derive.Deriver a -> Derive.Deriver a
dispatch_tempo sym block_dur maybe_track_id signal deriver = case sym of
    Nothing -> Tempo.with_tempo block_dur maybe_track_id signal deriver
    Just sym
        | sym == "hybrid" ->
            Tempo.with_hybrid block_dur maybe_track_id signal deriver
        | otherwise -> Derive.throw $
            "unknown tempo modifier: " <> untxt (ShowVal.show_val sym)

control_call :: TrackTree.TrackEvents -> Score.Typed Score.Control
    -> Derive.Merge -> (Bool -> Derive.Deriver (TrackResults Signal.Control))
    -- ^ The deriver takes a switch to prevent caching if 'stash_signal' runs
    -- it again.
    -> Derive.NoteDeriver -> Derive.NoteDeriver
control_call track control merge control_deriver deriver = do
    (signal, logs) <- Internal.track_setup track (control_deriver True)
    stash_signal track signal (to_display <$> control_deriver False) False
    -- Apply and strip any control modifications made during the above derive.
    Derive.apply_control_mods $ merge_logs logs $ with_damage $
        with_control_op control merge signal deriver
    -- I think this forces sequentialness because 'deriver' runs in the state
    -- from the end of 'control_deriver'.  To make these parallelize, I need to
    -- run control_deriver as a sub-derive, then mappend the Collect.
    where
    with_damage = with_control_damage
        (TrackTree.tevents_block_track_id track) (TrackTree.tevents_range track)

with_control_op :: Score.Typed Score.Control -> Derive.Merge -> Signal.Control
    -> Derive.Deriver a -> Derive.Deriver a
with_control_op (Score.Typed typ control) merge signal =
    Derive.with_merged_control merge control (Score.Typed typ signal)

to_display :: TrackResults Signal.Control -> Signal.Display
to_display (sig, _) = Signal.coerce sig
    -- I discard the logs since I think if there is anything interesting it
    -- will be logged in the "real" derivation.

merge_logs :: [Log.Msg] -> Derive.NoteDeriver -> Derive.NoteDeriver
merge_logs logs deriver = do
    events <- deriver
    return $ Derive.merge_events (map LEvent.Log logs) events

pitch_call :: TrackTree.TrackEvents -> Maybe Score.Control -> Pitch.ScaleId
    -> [TrackLang.Call] -> Derive.NoteDeriver -> Derive.NoteDeriver
pitch_call track maybe_name scale_id expr deriver =
    Internal.track_setup track $ do
        scale <- get_scale scale_id
        Derive.with_scale scale $ do
            (signal, logs) <- derive_pitch True track expr
            -- Ignore errors, they should be logged on conversion.
            (nn_sig, _) <- pitch_signal_to_nn signal
            stash_signal track (Signal.coerce nn_sig)
                (to_psig $ derive_pitch False track expr) True
            -- Apply and strip any control modifications made during the above
            -- derive.
            Derive.apply_control_mods $ merge_logs logs $ with_damage $
                Derive.with_pitch maybe_name signal deriver
    where
    with_damage = with_control_damage (TrackTree.tevents_block_track_id track)
        (TrackTree.tevents_range track)
    to_psig derive = do
        (sig, _) <- derive
        Signal.coerce . fst <$> pitch_signal_to_nn sig

get_scale :: Pitch.ScaleId -> Derive.Deriver Scale.Scale
get_scale scale_id
    | scale_id == Pitch.empty_scale = Util.get_scale
    | otherwise = Derive.get_scale scale_id

with_control_damage :: Maybe (BlockId, TrackId) -> TrackRange
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
derive_control :: Bool -> Bool -> TrackTree.TrackEvents -> [TrackLang.Call]
    -> Derive.Deriver (TrackResults Signal.Control)
derive_control cache is_tempo track expr = do
    let (start, end) = TrackTree.tevents_range track
    stream <- Call.apply_transformers
        (Derive.dummy_call_info start (end-start) "control track") expr deriver
    let (signal_chunks, logs) = LEvent.partition stream
        -- I just did it in 'compact', so this should just convert [x] to x.
        signal = mconcat signal_chunks
    return (signal, logs)
    where
    deriver :: Derive.ControlDeriver
    deriver = (if cache then Cache.track track mempty else id) $ do
        state <- Derive.get
        let (stream, collect) = Call.derive_track state tinfo
                Call.control_last_sample (tevents track)
        Internal.merge_collect collect
        return $ compact (concat stream)
    -- Merge the signal here so it goes in the cache as one signal event.
    -- I can use concat instead of merge_asc_events because the signals
    -- will be merged with Signal.merge and the logs extracted.
    compact events = LEvent.Event (mconcat sigs) : map LEvent.Log logs
        where (sigs, logs) = LEvent.partition events
    tinfo = Call.TrackInfo
        { Call.tinfo_events_end = TrackTree.tevents_end track
        , Call.tinfo_track_range = TrackTree.tevents_range track
        , Call.tinfo_shifted = TrackTree.tevents_shifted track
        , Call.tinfo_sub_tracks = []
        , Call.tinfo_events_around = TrackTree.tevents_around track
        , Call.tinfo_type =
            if is_tempo then TrackInfo.TempoTrack else TrackInfo.ControlTrack
        , Call.tinfo_inverted = TrackTree.tevents_inverted track
        }

derive_pitch :: Bool -> TrackTree.TrackEvents -> [TrackLang.Call]
    -> Derive.Deriver (TrackResults Pitch)
derive_pitch cache track expr = do
    let (start, end) = TrackTree.tevents_range track
    stream <- Call.apply_transformers
        (Derive.dummy_call_info start (end-start) "pitch track") expr deriver
    let (signal_chunks, logs) = LEvent.partition stream
        -- I just did it in 'compact', so this should just convert [x] to x.
        signal = mconcat signal_chunks
    return (signal, logs)
    where
    deriver = (if cache then Cache.track track mempty else id) $ do
        state <- Derive.get
        let (stream, collect) = Call.derive_track state tinfo
                Call.pitch_last_sample (tevents track)
        Internal.merge_collect collect
        return $ compact (concat stream)
    -- Merge the signal here so it goes in the cache as one signal event.
    compact events = LEvent.Event (mconcat sigs) : map LEvent.Log logs
        where (sigs, logs) = LEvent.partition events
    tinfo = Call.TrackInfo
        { Call.tinfo_events_end = TrackTree.tevents_end track
        , Call.tinfo_track_range = TrackTree.tevents_range track
        , Call.tinfo_shifted = TrackTree.tevents_shifted track
        , Call.tinfo_sub_tracks = []
        , Call.tinfo_events_around = TrackTree.tevents_around track
        , Call.tinfo_type = TrackInfo.PitchTrack
        , Call.tinfo_inverted = TrackTree.tevents_inverted track
        }

tevents :: TrackTree.TrackEvents -> [Event.Event]
tevents = Events.ascending . TrackTree.tevents_events


-- * TrackSignal

-- | If this track is to be rendered by the UI, stash the given signal away in
-- the Derive state as a 'Track.TrackSignal'.  I may or may not need to
-- re-derive the signal, for reasons explained in the TrackSignal doc.
--
-- TODO if TrackId appears in more than one place I may wind up running this
-- redundantly.  However, I think the proper way to solve this is to cache
-- the signals and avoid recalculating the control track at all.  Perhaps just
-- add a warped signal to TrackSignal?
stash_signal :: TrackTree.TrackEvents
    -> Signal.Signal y -> Derive.Deriver Signal.Display
    -- ^ Both a signal and a deriver to produce the signal are provided.  If
    -- the block has no warp the already derived signal can be reused,
    -- otherwise it must be rederived.
    -> Bool
    -- ^ True if this is a pitch signal.
    -> Derive.Deriver ()
stash_signal track sig derive_sig is_pitch = whenM (signal_wanted track) $
    case TrackTree.tevents_block_track_id track of
        Just (block_id, track_id) | not (TrackTree.tevents_sliced track) ->
            stash block_id track_id =<< linear_tempo
        _ -> return ()
    where
    stash block_id track_id (Just (shift, stretch)) =
        put_track_signal block_id track_id $
            Track.TrackSignal (Signal.coerce sig) shift stretch is_pitch
    stash block_id track_id Nothing = do
        sig <- Derive.in_real_time derive_sig
        put_track_signal block_id track_id $ Track.TrackSignal sig 0 1 is_pitch

-- | Return (shift, stretch) if the tempo is linear.  This relies on an
-- optimization in 'Derive.d_tempo' to notice when the tempo is constant and
-- give it 'Score.id_warp_signal'.
linear_tempo :: Derive.Deriver (Maybe (ScoreTime, ScoreTime))
linear_tempo = do
    warp <- Internal.get_dynamic Derive.state_warp
    return $ if Score.warp_signal warp == Score.id_warp_signal
        then Just (Score.warp_shift warp, Score.warp_stretch warp)
        else Nothing

put_track_signal :: BlockId -> TrackId -> Track.TrackSignal -> Derive.Deriver ()
put_track_signal block_id track_id tsig =
    put_track_signals [((block_id, track_id), tsig)]

put_track_signals :: [((BlockId, TrackId), Track.TrackSignal)]
    -> Derive.Deriver ()
put_track_signals [] = return ()
put_track_signals tracks = Internal.merge_collect $ mempty
    { Derive.collect_track_signals =
        -- This is needed for 'split_control_tracks' signals to merge together
        -- as expected.
        Map.fromListWith Track.merge_signals tracks
    }

signal_wanted :: TrackTree.TrackEvents -> Derive.Deriver Bool
signal_wanted track = case TrackTree.tevents_block_track_id track of
        Just (block_id, track_id) -> Set.member (block_id, track_id) <$>
            Internal.get_constant Derive.state_wanted_track_signals
        Nothing -> return False

-- * track_signal

-- | Derive just the rendered track signal from a track, if this track is to
-- be rendered.  This is like 'eval_track' but specialized to derive only the
-- signal.  The track signal is normally stashed as a side-effect of control
-- track evaluation, but tracks below a note track are not evaluated normally.
derive_track_signals :: TrackTree.EventsTree -> Derive.Deriver ()
derive_track_signals = put_track_signals <=< track_signal_ <=< filter_rendered

-- | Filter out tracks that shouldn't be rendered, or don't want to be
-- rendered.  Since control tracks can depend on other controls, a parent
-- must be rendered if any of its children want to be rendered.
filter_rendered :: TrackTree.EventsTree -> Derive.Deriver TrackTree.EventsTree
filter_rendered = concatMapM f
    where
    f (Tree.Node track subs) | not (should_render False track) =
        filter_rendered subs
    f node@(Tree.Node track []) =
        ifM (signal_wanted track) (return [node]) (return [])
    f (Tree.Node track subs) = do
        filtered <- filter_rendered subs
        this_wanted <- signal_wanted track
        return $ case filtered of
            [] | not this_wanted -> []
            _ -> [Tree.Node track filtered]

-- | False if the track should not render at all, not even for the benefit of
-- sub tracks.
should_render :: Bool -> TrackTree.TrackEvents -> Bool
should_render note_track track =
    not (TrackTree.tevents_sliced track || Text.null title
        || Events.null (TrackTree.tevents_events track))
    && (if note_track then id else not) (TrackInfo.is_note_track title)
    where title = TrackTree.tevents_title track

track_signal_ :: TrackTree.EventsTree
    -> Derive.Deriver [((BlockId, TrackId), Track.TrackSignal)]
track_signal_ = concatMapM go
    where
    go (Tree.Node track subs) = do
        (ctype, expr) <-
            either (\err -> Derive.throw $ "track title: " ++ err) return
            (TrackInfo.parse_control_expr (TrackTree.tevents_title track))
        -- Since these signals are being rendered solely for display, they
        -- should ignore any tempo warping, just like 'stash_signal' does
        -- above.
        Derive.in_real_time $ eval_signal track expr ctype subs

eval_signal :: TrackTree.TrackEvents -> [TrackLang.Call]
    -> TrackInfo.ControlType -> [TrackTree.EventsNode]
    -> Derive.Deriver [((BlockId, TrackId), Track.TrackSignal)]
eval_signal track expr ctype subs
    -- They should all have TrackIds because of 'signal_wanted'.
    | Just (block_id, track_id) <- track_block_id = case ctype of
        TrackInfo.Tempo _ -> do
            (signal, logs) <- with_stack $ with_control_env Controls.tempo $
                derive_control False True track expr
            write logs
            sub_sigs <- track_signal_ subs
            return $ ((block_id, track_id), track_sig signal False) : sub_sigs
        TrackInfo.Control maybe_op control -> do
            merge <- lookup_op (Score.typed_val control) maybe_op
            (signal, logs) <- with_stack $
                with_control_env (Score.typed_val control) $
                derive_control False False track expr
            write logs
            sub_sigs <- Derive.apply_control_mods $
                with_control_op control merge signal $ track_signal_ subs
            -- Signals are combined with control mods only when they go into
            -- the ControlMap, so they're not visible in the UI.  I originally
            -- wanted to apply control modifications to the output signal to
            -- make them visible, but that's problematic because they then want
            -- to apply to the entire signal.  In real derivation, that doesn't
            -- happen because of slicing.  In any case, the track render is
            -- supposed to be the output of the track, not what the signal
            -- eventually becomes, so maybe not applying mods is the right
            -- thing.
            return $ ((block_id, track_id), track_sig signal False) : sub_sigs
        TrackInfo.Pitch scale_id maybe_name -> do
            scale <- get_scale scale_id
            (signal, logs) <- with_stack $ Derive.with_scale scale $
                derive_pitch False track expr
            write logs
            -- TODO I log derivation errors... why not log pitch errors?
            -- TODO maybe I shouldn't apply transposition, because it's going
            -- to the display
            (nn_signal, _) <- pitch_signal_to_nn signal
            sub_sigs <- Derive.apply_control_mods $
                Derive.with_pitch maybe_name signal $ track_signal_ subs
            return $ ((block_id, track_id), track_sig nn_signal True) : sub_sigs
    | otherwise = return []
    where
    track_block_id = TrackTree.tevents_block_track_id track
    -- Normally the TrackId is added to the stack by 'BlockUtil.derive_track',
    -- but I'm bypassing the usual track derivation so I need to add it myself.
    with_stack = case TrackTree.tevents_track_id track of
        Nothing -> id
        Just track_id -> Internal.with_stack_track track_id
    write = mapM_ (Log.write . prio) . Log.add_prefix "Track signal"
    prio msg = msg { Log.msg_prio = Log.Debug }
    track_sig signal is_pitch = Track.TrackSignal
        { Track.ts_signal = Signal.coerce signal
        , Track.ts_shift = 0
        , Track.ts_stretch = 1
        , Track.ts_is_pitch = is_pitch
        }

with_control_env :: Score.Control -> Derive.Deriver a -> Derive.Deriver a
with_control_env = Derive.with_val Environ.control . Score.control_name


-- * util

-- | Reduce a 'PitchSignal.Signal' to raw note numbers, taking the current
-- transposition environment into account.
pitch_signal_to_nn :: PitchSignal.Signal
    -> Derive.Deriver (Signal.NoteNumber, [PitchSignal.PitchError])
pitch_signal_to_nn sig = do
    controls <- Internal.get_dynamic Derive.state_controls
    environ <- Internal.get_environ
    return $ PitchSignal.to_nn $ PitchSignal.apply_controls environ controls sig
