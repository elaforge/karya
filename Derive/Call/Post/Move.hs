-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Postprocs that change note start and duration.
module Derive.Call.Post.Move where
import qualified Util.ApproxEq as ApproxEq
import qualified Util.Map as Map
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Note as Note
import qualified Derive.Call.Post as Post
import qualified Derive.Call.Tags as Tags
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.Environ as Environ
import qualified Derive.Flags as Flags
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Signal as Signal
import Global
import Types


note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.transformer_call_map
    [ ("infer-duration", c_infer_duration)
    , ("apply-start-offset", c_apply_start_offset)
    ]


-- * infer duration

c_infer_duration :: Derive.Transformer Derive.Note
c_infer_duration = Derive.transformer Module.prelude "infer-duration"
    Tags.postproc "Infer durations for `+infer-duration` events, and possibly\
    \ cancel notes with `+track-time-0`.\
    \\nThis is intended to support Indonesian-style \"arrival beats\".\
    \ If there is a zero-duration note at the end of a block, the default note\
    \ deriver sets `+infer-duration` on it. This note will then replace any\
    \ notes at the beginning of the next block. If it replaces a note, it\
    \ takes on that note's duration and controls. Otherwise, it extends to the\
    \ start of the next note."
    $ Sig.callt
    ( Sig.defaulted "final-duration" 1
        "If there is no following note, infer this duration."
    ) $ \final_dur _args deriver -> infer_duration final_dur <$> deriver

infer_duration :: RealTime -> Derive.Events -> Derive.Events
infer_duration final_dur = cancel_notes . infer_notes
    where
    infer_notes = Post.emap1_ infer . Post.neighbors_same_hand id
    cancel_notes =
        Post.cat_maybes . Post.emap1_ cancel . Post.neighbors_same_hand id

    cancel (maybe_prev, event, _)
        | has Flags.can_cancel event, Just prev <- maybe_prev,
                has Flags.infer_duration prev =
            Nothing
        | otherwise = Just event
    infer (_, event, maybe_next)
        | not (has Flags.infer_duration event) = event
        | Just next <- maybe_next = replace_note next event
        | otherwise = set_dur final_dur event
    set_dur dur = Score.set_duration dur
    has = Score.has_flags

-- | A note with inferred duration gets its start from the end of the previous
-- block, but its duration and the rest of its controls come from the
-- corresponding note at the beginning of the next block.
--
-- If there is no note to replace, it extends to the start of the next note.
replace_note :: Score.Event -> Score.Event -> Score.Event
replace_note next event
    | Score.has_flags Flags.can_cancel next =
        set_end (Score.event_end next) event
            { Score.event_untransformed_pitch = pitch event
                <> PitchSignal.drop_before_at start (pitch next)
            , Score.event_untransformed_pitches = Map.mappend
                (pitches event)
                (PitchSignal.drop_before_at start <$> pitches next)
            , Score.event_untransformed_controls = Map.mappend
                (controls event)
                (fmap (Signal.drop_before_at start) <$> controls next)
            }
    | otherwise = set_end (Score.event_start next) event
    where
    pitch = Score.event_transformed_pitch
    pitches = Score.event_transformed_pitches
    controls = Score.event_transformed_controls
    start = Score.event_start event
    set_end end = Score.set_duration (end - Score.event_start event)

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
            -- |----=+=>
            --     <-+-----|
            -- |---====+===)--->
            --     <---+---|
            -- |------->   )
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
