-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Postprocs that change note start and duration.
module Derive.Call.Post.Move where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Util.ApproxEq as ApproxEq
import qualified Util.Map
import qualified Util.Seq as Seq

import qualified Derive.Call.Module as Module
import qualified Derive.Call.Post as Post
import qualified Derive.Call.Prelude.Note as Note
import qualified Derive.Call.Tags as Tags
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.Environ as Environ
import qualified Derive.Flags as Flags
import qualified Derive.LEvent as LEvent
import qualified Derive.PSignal as PSignal
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.TrackLang as TrackLang

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import Global
import Types


note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.transformer_call_map
    [ ("infer-duration", c_infer_duration)
    , ("apply-start-offset", c_apply_start_offset)
    , ("cancel", c_cancel)
    ]


-- * infer duration

c_infer_duration :: Derive.Transformer Derive.Note
c_infer_duration = Derive.transformer Module.prelude "infer-duration"
    Tags.postproc "Infer durations for 'Derive.Flags.infer-duration' events,\
    \ and possibly cancel notes with 'Derive.Flags.weak'.\
    \\nThis is intended to support Indonesian-style \"arrival beats\".\
    \ If there is a zero-duration note at the end of a block, the default note\
    \ deriver sets `+infer-duration` on it. This note will then replace any\
    \ notes at the beginning of the next block. If it replaces a note, it\
    \ takes on that note's duration and controls. Otherwise, it extends to the\
    \ start of the next note.\
    \\nThis also applies the 'Derive.Environ.suppress_until' env val, so a\
    \ call can cancel out other events."
    $ Sig.callt
    ( Sig.defaulted "final-duration" 1
        "If there is no following note, infer this duration."
    ) $ \final_dur _args deriver -> infer_duration2 final_dur <$> deriver

infer_duration2 :: RealTime -> Derive.Events -> Derive.Events
infer_duration2 final_dur = cancel_notes . suppress_notes
    where
    cancel_notes = Post.cat_maybes . Post.emap1_ process
        . Post.neighbors_same_hand id

    -- Cancel means the note just goes away.  But if an infer-duration cancels
    -- a note, it replaces it, which means it takes over its duration and
    -- controls.

    -- prev             cur     |  prev         cur
    -- weak+infer       weak    -> replace      Nothing
    -- infer            weak    -> replace      Nothing
    -- weak             weak    -> id           Nothing
    -- {}               weak    -> id           Nothing
    -- weak             {}      -> Nothing      id
    --
    -- strong+infer     {}     -> replace      Nothing
    -- strong           {}     -> id           Nothing
    process (maybe_prev, event, maybe_next) =
        fmap strip_flags . check_prev maybe_prev =<< check_next maybe_next event

    check_next :: Maybe Score.Event -> Score.Event -> Maybe Score.Event
    check_next maybe_next event = case maybe_next of
        Just next
            | not (same_start event next) -> Just $
                -- If the note isn't coincident, I won't replace it, but
                -- instead extend to the start of the next note.
                if has Flags.infer_duration event
                    then set_end (Score.event_start next) event
                    else event
            | is_weak next || has Flags.strong event -> Just $
                if has Flags.infer_duration event
                    then replace_note next event else event
            | is_weak event && not (is_weak next) -> Nothing
            | otherwise -> Just event
        Nothing
            | has Flags.infer_duration event -> Just $
                Score.set_duration final_dur event
            | otherwise -> Just event

    check_prev :: Maybe Score.Event -> Score.Event -> Maybe Score.Event
    check_prev maybe_prev event = case maybe_prev of
        Just prev
            | not (same_start prev event) -> Just event
            | is_weak event || has Flags.strong prev -> Nothing
        _ -> Just event
    is_weak = has Flags.weak
    -- Remove flags I've processed.  This way if there's another infer-duration
    -- in the call stack the processing won't happen twice.
    strip_flags = Score.remove_flags $
        Flags.infer_duration <> Flags.strong <> Flags.weak

    has = Score.has_flags
    same_start e1 e2 = Score.event_start e1 RealTime.== Score.event_start e2
    set_end end event = Score.set_duration (end - Score.event_start event) event

-- | Filter out events that fall at and before the 'Environ.suppress_until'
-- range of an event with the same (instrument, hand).  Only events that don't
-- have a suppress_until are suppressed.
--
-- This is complicated by the fact that an event should suppress coincident
-- events even if the supressor follows the suppressee in the list, so I have
-- to look into the future for the greatest suppress_until.
suppress_notes :: Derive.Events -> Derive.Events
suppress_notes =
    snd . Post.emap go Map.empty . Post.zip_on Post.nexts
        . Post.zip3_on (map Post.hand_key) (map get_suppress)
    where
    go suppressed (nexts, (key, suppress, event)) = case suppress of
        Nothing -> (,) suppressed $ case suppress_until of
            Just until | until >= Score.event_start event - RealTime.eta -> []
            _ -> [event]
        Just until -> (Map.insert key until suppressed, [event])
        where
        suppress_until = Seq.maximum $ Maybe.catMaybes $
            (Map.lookup key suppressed :) $ map suppress_of $
            takeWhile (coincident . event_of) $
            filter ((==key) . key_of) nexts
        coincident e = Score.event_start e
            <= Score.event_start event + RealTime.eta
    get_suppress :: Score.Event -> Maybe RealTime
    get_suppress =
        TrackLang.maybe_val Environ.suppress_until . Score.event_environ
    key_of (k, _, _) = k
    suppress_of (_, s, _) = s
    event_of (_, _, e) = e

-- | A note with inferred duration gets its start from the end of the previous
-- block, but its duration and the rest of its controls come from the
-- corresponding note at the beginning of the next block.
replace_note :: Score.Event -> Score.Event -> Score.Event
replace_note next event = event
    { Score.event_duration = Score.event_end next - start
    , Score.event_untransformed_pitch = pitch event
        <> PSignal.drop_before_at start (pitch next)
    , Score.event_untransformed_pitches = Util.Map.mappend
        (pitches event)
        (PSignal.drop_before_at start <$> pitches next)
    , Score.event_untransformed_controls = Util.Map.mappend
        (controls event)
        (fmap (Signal.drop_before_at start) <$> controls next)
    }
    where
    pitch = Score.event_transformed_pitch
    pitches = Score.event_transformed_pitches
    controls = Score.event_transformed_controls
    start = Score.event_start event

-- * cancel

c_cancel :: Derive.Transformer Derive.Note
c_cancel = Derive.transformer Module.prelude "cancel" Tags.postproc
    "Process the 'Derive.Flags.strong' and 'Derive.Flags.weak' flags.\
    \ This will cause notes to be dropped."
    $ make_cancel (cancel_strong_weak merge_infer) Post.hand_key

-- | Given a set of coincident notes, return either an error, or merge them
-- into a set of output notes.
type Cancel = [Score.Event] -> Either Text [Score.Event]

make_cancel :: Ord key => Cancel -> (Score.Event -> key)
    -> Derive.WithArgDoc (Derive.TransformerF Derive.Note)
make_cancel cancel key =
    Sig.callt (Sig.defaulted_env "final-duration" Sig.Unprefixed 1
        "If there is no following note, infer this duration."
    ) $ \final_dur _args deriver ->
        Derive.require_right id . group_and_cancel cancel key final_dur
            =<< deriver

group_and_cancel :: Ord key => Cancel -> (Score.Event -> key) -> RealTime
    -> Events -> Either Text Events
group_and_cancel cancel key final_dur =
    fmap (infer_duration final_dur . suppress_notes)
    . merge_groups cancel . group_coincident key

-- | Merge notes with 'Flags.strong' and 'Flags.weak'.  The rules are that
-- exactly one strong note wins, but >1 is ambiguous.  Otherwise, multiple
-- normal notes can win over weak notes, and exactly one weak note is ok, but
-- multiple weak notes with no normal ones is once again ambiguous.
cancel_strong_weak :: (Score.Event -> [Score.Event] -> Score.Event)
    -> [Score.Event] -> Either Text [Score.Event]
cancel_strong_weak merge events = case partition events of
    (strong : extras, weaks, normals)
        | null extras -> Right [merge strong (normals ++ weaks)]
        | otherwise -> Left $ "multiple " <> pretty Flags.strong <> " events: "
            <> Score.log_events (strong : extras)
    ([], weak : extras, [])
        | null extras -> Right [weak]
        | otherwise -> Left $ "multiple " <> pretty Flags.weak <> " events: "
            <> Score.log_events (weak : extras)
    ([], weaks, [normal]) -> Right [merge normal weaks]
    ([], [], normals) -> Right normals
    ([], weaks@(_:_), normals) -> Left $ "multiple normal events: "
        <> Score.log_events normals <> " and multiple " <> pretty Flags.weak
        <> " events: " <> Score.log_events weaks
    -- Multiple weak notes are ok if there are non-weak notes.
    where
    partition = Seq.partition2 (Score.has_flags Flags.strong)
        (Score.has_flags Flags.weak)

-- | Handle 'Flags.infer_duration'.
merge_infer :: Score.Event -> [Score.Event] -> Score.Event
merge_infer strong weaks
    | Score.has_flags Flags.infer_duration strong =
        set_end end $ Score.remove_flags Flags.infer_duration strong
    | otherwise = strong
    where
    set_end end event = Score.set_duration (end - Score.event_start event) event
    end = fromMaybe (Score.event_end strong) $
        Seq.maximum (map Score.event_end weaks)

infer_duration :: RealTime -> Derive.Events -> Derive.Events
infer_duration final_dur = Post.emap1_ infer . Post.nexts_same_hand id
    where
    infer (event, next)
        | Score.has_flags Flags.infer_duration event = maybe
            (Score.set_duration final_dur) (set_end . Score.event_start) next $
            Score.remove_flags Flags.infer_duration event
        | otherwise = event
    set_end end event = Score.set_duration (end - Score.event_start event) event

merge_groups :: ([a] -> Either Text [a]) -> [Either [LEvent.LEvent a] [a]]
    -> Either Text [LEvent.LEvent a]
merge_groups merge = concatMapM go
    where
    go (Left ungrouped) = Right ungrouped
    go (Right []) = Right []
    go (Right [e]) = Right [LEvent.Event e]
    go (Right es) = map LEvent.Event <$> merge es

type Events = [LEvent.LEvent Score.Event]

group_coincident :: Ord key => (Score.Event -> key) -> Events
    -> [Either Events [Score.Event]]
group_coincident key = go
    where
    go [] = []
    go (log@(LEvent.Log {}) : es) = Left [log] : go es
    go (LEvent.Event e : es) =
        (if null logs then id else (Left (map LEvent.Log logs) :)) $
            groups ++ go after
        where
        ((during, logs), after) = first LEvent.partition $
            span (LEvent.log_or $ same_start e) es
        -- [e] is going to be a common case, since most notes don't group.
        groups = map Right (Seq.group_sort key (e : during))
    same_start e1 e2 = Score.event_start e1 RealTime.== Score.event_start e2

-- * apply start offset

-- | Previously I applied the @%start-s@ and @%start-t@ controls in the note
-- generator, but I wound up with notes getting out of sync with their
-- controls.  Even if I apply the controls before inversion, it still doesn't
-- work other calls, like say block calls, and I can't apply the controls
-- before the block call
c_apply_start_offset :: Derive.Transformer Derive.Note
c_apply_start_offset =
    Derive.transformer Module.prelude "apply-start-offset" Tags.postproc
    ("Apply the " <> ShowVal.doc_val Environ.start_offset_val <> " env var.\
     \ This is set by note deriver from the "
     <> ShowVal.doc_val Controls.start_s <> " and "
     <> ShowVal.doc_val Controls.start_t <> " controls, so if you want those\
     \ controls to have an effect, you have to use this postproc."
    ) $ Sig.callt (
        Sig.defaulted "min-duration" Nothing "If given, notes on the same hand\
            \ won't be moved closer than this time. Otherwise, hand and\
            \ instrument is ignored."
    ) $ \min_dur _args deriver -> apply_start_offset min_dur <$> deriver

apply_start_offset :: Maybe RealTime -> Derive.Events -> Derive.Events
apply_start_offset maybe_min_dur =
    apply_offset . tweak_offset . Post.zip_on (map offset_of)
    where
    tweak_offset = case maybe_min_dur of
        Nothing -> id
        Just min_dur ->
            Post.emap1_ (tweak min_dur) . Post.neighbors_same_hand snd
    tweak min_dur (prev, (offset, event), next) = (new_offset, event)
        where
        new_offset = adjust_offset min_dur (extract <$> prev) (extract <$> next)
            offset (Score.event_start event)
        extract (offset, event) = (offset, Score.event_start event)

    apply_offset = Post.emap1_ apply . Post.neighbors_same_hand snd
    apply (_, (offset, event), maybe_next) =
        set_dur $ Score.move_start (fromMaybe Note.min_duration maybe_min_dur)
            offset event
        where
        set_dur event = case maybe_next of
            Nothing -> event
            Just (next_offset, next) -> Score.duration (const dur) event
                where
                dur = adjust_duration (Score.event_start next)
                    (Score.event_start next + next_offset) event

-- | Conceptually, all notes move together until they bump into each
-- other.  Or, they move without restriction, and then go to midway of
-- the overlap.  But the note's start is a hard lower or upper limit, so one
-- note moving can never cause another note to move, it can just cause it to
-- not move as much as it wanted.
--
-- TODO actually "half of the overlap" is not the same as "all move together".
-- For the latter, the overlap split depends on how far the note moved to get
-- there.  So instead of overlap/2 it's 'max 0 (overlap - n) / 2', where 'n' is
-- the imbalance between their move offsets.
--
-- TODO this is still broken if an offset causes an note to skip over another.
-- But that should be stopped by the next event, right?
adjust_offset :: RealTime -- ^ don't move notes any closer than this
    -> Maybe (RealTime, RealTime) -> Maybe (RealTime, RealTime)
    -> RealTime -> RealTime -> RealTime
adjust_offset min_dur prev next offset start
    | offset == 0 = offset
    | offset > 0 = case next of
        Nothing -> offset
            -- 0   1   2   3   4
            -- [----=+=>
            --     <-+-----|
            -- [---====+===)--->
            --     <---+---|
            -- [------->   )
            --     |------->
        Just (next_offset, next_start)
            | overlap <= 0 -> min (next_end - min_dur) end - start
            | otherwise -> (end - overlap + overlap / 2 - min_dur) - start
            where
            overlap = end - next_end
            end = min (max next_start next_end) (start + offset)
            next_end = max start (next_start + next_offset)
    | otherwise = case prev of
        Nothing -> offset
            -- 0   1   2   3   4
            -- <-------|
            -- (   <-------|
        Just (prev_offset, prev_start)
            -- If the prev_offset is positive, then it will have already given
            -- the min_dur space.
            | overlap <= 0 -> if prev_offset > 0
                then offset
                else max (prev_end + min_dur) end - start
            | otherwise -> (end + overlap - overlap / 2) - start
            where
            overlap = prev_end - end
            end = max (min prev_start prev_end) (start + offset)
            prev_end = min start (prev_start + prev_offset)

-- | Change the duration based on the movement of the next event.
--
-- If the event end touches the next start, then adjust dur by next_offset.  If
-- it's less, then shorten but don't lengthen.  If it overlaps the next note,
-- then leave it alone.
adjust_duration :: RealTime -> RealTime -> Score.Event -> RealTime
adjust_duration next new_next event =
    subtract (Score.event_start event) $ case ApproxEq.compare 0.001 end next of
        EQ -> new_next
        LT -> min new_next end
        GT -> end
    where end = Score.event_end event

offset_of :: Score.Event -> RealTime
offset_of = fromMaybe 0 . TrackLang.maybe_val Environ.start_offset_val
    . Score.event_environ
