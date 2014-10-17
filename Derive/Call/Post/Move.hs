-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Postprocs that change note start and duration.
module Derive.Call.Post.Move where
import Util.Control
import qualified Util.Map as Map
import qualified Util.Seq as Seq

import qualified Derive.Call.Module as Module
import qualified Derive.Call.Note as Note
import qualified Derive.Call.Post as Post
import qualified Derive.Call.Tags as Tags
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.Environ as Environ
import qualified Derive.Flags as Flags
import qualified Derive.LEvent as LEvent
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Signal as Signal
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
    zip_with f xs = LEvent.zip (f xs) xs
    infer_notes = Post.emap1 infer . zip_with Post.nexts
    cancel_notes = Post.cat_maybes . Post.emap1 cancel . zip_with Post.prevs

    cancel (prevs, event)
        | has Flags.track_time_0 event,
                Just prev <- Seq.head (Post.same_hand event prevs),
                has Flags.infer_duration prev =
            Nothing
        | otherwise = Just event
    infer (nexts, event)
        | not (has Flags.infer_duration event) = event
        | Just next <- Seq.head (Post.same_hand event nexts) =
            replace_note next event
        | otherwise = set_dur final_dur event
    set_dur dur = Score.set_duration dur
    has = Score.has_flags

-- | A note with inferred duration gets its start from the end of the previous
-- block, but its duration and the rest of its controls come from the
-- corresponding note at the beginning of the next block.
--
-- If there is no note to replace, it extends to the start of the next note.
-- TODO and it doesn't get any controls, other than what it picked up at the
-- end of the last block.  I should fix this, but I'm not sure how.
replace_note :: Score.Event -> Score.Event -> Score.Event
replace_note next event
    | Score.has_flags Flags.track_time_0 next =
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
    ) $ Sig.call0t $ \_args deriver -> Post.emap1 apply_start_offset <$> deriver

apply_start_offset :: Score.Event -> Score.Event
apply_start_offset event =
    case TrackLang.maybe_val Environ.start_offset_val env of
        Nothing -> event
        Just offset -> Score.move_start Note.min_duration offset event
    where env = Score.event_environ event

-- TODO fancier version that won't move past neighbors:
-- Divide up by instrument and hand, then move each event by the offset control
-- inside it, but not so that it overlaps a neighbor.
