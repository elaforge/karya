-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Postprocs that change note start and duration.
module Derive.Call.Post.Move where
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Util.ApproxEq as ApproxEq
import qualified Util.Map
import qualified Util.Seq as Seq

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

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import Global
import Types


note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.transformer_call_map
    [ ("infer-duration", c_infer_duration)
    , ("apply-start-offset", c_apply_start_offset)
    -- TODO this should probably go in a NoteTransformer module, which
    -- is really Derive.Transformer Derive.Note
    , ("add-flag", c_add_flag)
    ]


-- * infer duration

c_infer_duration :: Derive.Transformer Derive.Note
c_infer_duration = Derive.transformer Module.prelude "infer-duration"
    Tags.postproc "Infer durations for 'Derive.Flags.infer-duration' events,\
    \ and possibly cancel notes with 'Derive.Flags.can_cancel'.\
    \\nThis is intended to support Indonesian-style \"arrival beats\".\
    \ If there is a zero-duration note at the end of a block, the default note\
    \ deriver sets `+infer-duration` on it. This note will then replace any\
    \ notes at the beginning of the next block. If it replaces a note, it\
    \ takes on that note's duration and controls. Otherwise, it extends to the\
    \ start of the next note.\
    \\nThis also applies the 'Environ.suppress_until' env val, so a call can\
    \ cancel out other events."
    $ Sig.callt
    ( Sig.defaulted "final-duration" 1
        "If there is no following note, infer this duration."
    ) $ \final_dur _args deriver -> infer_duration final_dur <$> deriver

infer_duration :: RealTime -> Derive.Events -> Derive.Events
infer_duration final_dur = cancel_notes . suppress_note
    where
    cancel_notes = Post.cat_maybes . Post.emap1_ process
        . Post.neighbors_same_hand id

    -- Cancel means the note just goes away.  But if an infer-duration cancels
    -- a note, it replaces it, which means it takes over its duration and
    -- controls.

    -- weak+infer       weak    -> replace      Nothing
    -- infer            weak    -> replace      Nothing
    -- weak             weak    -> id           Nothing
    -- {}               weak    -> id           Nothing
    -- weak             {}      -> Nothing      id
    --
    -- cancel_next+infer {}     -> replace      Nothing
    -- cancel_next      {}      -> id           Nothing
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
            | can_cancel next || has Flags.cancel_next event -> Just $
                if has Flags.infer_duration event
                    then replace_note next event else event
            | can_cancel event && not (can_cancel next) -> Nothing
            | otherwise -> Just event
        Nothing
            | has Flags.infer_duration event -> Just $
                Score.set_duration final_dur event
            | otherwise -> Just event

    check_prev :: Maybe Score.Event -> Score.Event -> Maybe Score.Event
    check_prev maybe_prev event = case maybe_prev of
        Just prev
            | not (same_start prev event) -> Just event
            | can_cancel event || has Flags.cancel_next prev -> Nothing
        _ -> Just event
    can_cancel = has Flags.can_cancel
    -- Remove flags I've processed.  This way if there's another infer-duration
    -- in the call stack the processing won't happen twice.
    strip_flags = Score.remove_flags $
        Flags.infer_duration <> Flags.cancel_next <> Flags.can_cancel

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
suppress_note :: Derive.Events -> Derive.Events
suppress_note =
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
        <> PitchSignal.drop_before_at start (pitch next)
    , Score.event_untransformed_pitches = Util.Map.mappend
        (pitches event)
        (PitchSignal.drop_before_at start <$> pitches next)
    , Score.event_untransformed_controls = Util.Map.mappend
        (controls event)
        (fmap (Signal.drop_before_at start) <$> controls next)
    }
    where
    pitch = Score.event_transformed_pitch
    pitches = Score.event_transformed_pitches
    controls = Score.event_transformed_controls
    start = Score.event_start event

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


-- * misc

c_add_flag :: Derive.Transformer Derive.Note
c_add_flag = Derive.transformer Module.prelude "add-flag" Tags.postproc
    "Add the given flags to transformed events.\
    \ Mostly for debugging and testing."
    $ Sig.callt (Sig.many1 "flag" "Add these flags.") $ \flags _args ->
        fmap $ Post.emap1_ $ Score.add_flags $ mconcatMap Flags.flag $
            NonEmpty.toList flags
