-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Ornaments for gender.  The unique thing about gender technique is the
-- delayed damping, so these calls deal with delayed damping.
module Derive.Call.Gender where
import Util.Control
import qualified Derive.Args as Args
import qualified Derive.Call.Post as Post
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.Pitches as Pitches
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import Derive.Sig (control, defaulted, typed_control)

import qualified Perform.Pitch as Pitch
import Types


note_calls :: Derive.NoteCallMap
note_calls = Derive.make_calls
    [ ("'", c_tick Nothing)
    , ("'^", c_tick (Just (Pitch.Diatonic (-1))))
    , ("'v", c_tick (Just (Pitch.Diatonic 1)))
    , ("realize-damp", c_realize_damp)
    ]

c_tick :: Maybe Pitch.Transpose -> Derive.NoteCall
c_tick transpose = Derive.stream_generator "tick" (Tags.idiom <> Tags.prev)
    ("Insert an intermediate grace note in the \"ngoret\" rambat style.\
    \ The grace note moves up for `'^`, down for `'v`, or is based\
    \ on the previous note's pitch for `'`."
    ) $ Sig.call ((,,)
    <$> defaulted "time" (typed_control "ngoret-time" 0.1 Score.Real)
        "Time between the grace note start and the main note. If there isn't\
        \ enough room after the previous note, it will be halfway between\
        \ the previous note and this one."
    <*> defaulted "damp" (typed_control "ngoret-damp" 0.5 Score.Real)
        "Time that the grace note overlaps with this one. So the total\
        \ duration is time+damp, though it will be clipped to the\
        \ end of the current note."
    <*> defaulted "dyn" (control "ngoret-dyn" 0.75)
        "The grace note's dyn will be this multiplier of the current dyn."
    ) $ \(time, damp, dyn_scale) ->
    Sub.inverting_around (2, 1) $ \args -> do
        start <- Args.real_start args
        transpose <- maybe (infer_transpose args start) return transpose
        time <- Util.real_time =<< Util.time_control_at Util.Real time start
        damp <- Util.time_control_at Util.Real damp start
        dyn_scale <- Util.control_at dyn_scale start
        dyn <- Util.dynamic start

        grace_start <- Derive.score (start - time)
        -- If there isn't room for the grace note, use the midpoint between the
        -- prev note and this one.
        grace_start <- return $ case Args.prev_start args of
            Nothing -> grace_start
            Just prev -> max grace_start $ (prev + Args.start args) / 2

        overlap <- Util.duration_from (Args.start args) damp
        let grace_end = min (Args.end args) (Args.start args + overlap)

        pitch <- Derive.require "pitch" =<< Derive.pitch_at start
        Derive.d_place grace_start (grace_end - grace_start)
                (Util.add_attrs damped_tag $
                    Util.pitched_note (Pitches.transpose transpose pitch)
                        (dyn * dyn_scale))
            <> Derive.d_place (Args.start args) (Args.duration args) Util.note

infer_transpose :: Derive.PassedArgs d -> RealTime
    -> Derive.Deriver Pitch.Transpose
infer_transpose args start = do
    prev <- Derive.real =<< Derive.require "previous event"
        (Args.prev_start args)
    prev_pitch <- Derive.require "previous pitch" =<< Derive.pitch_at prev
    this_pitch <- Derive.require "this pitch" =<< Derive.pitch_at start
    ifM ((<=) <$> Pitches.pitch_nn prev_pitch <*> Pitches.pitch_nn this_pitch)
        (return (Pitch.Diatonic (-1))) (return (Pitch.Diatonic 1))

c_realize_damp :: Derive.NoteCall
c_realize_damp = Derive.transformer "realize-damp" (Tags.idiom <> Tags.postproc)
    ("Extend the duration of events preceding one with a "
    <> ShowVal.show_val damped_tag
    <> " to the end of the event with the attr.\
    \ This is because the tick call can't modify its previous note.\
    \ TODO: Since there's no correspondence between tracks in different\
    \ blocks, the damping can't extend across block boundaries. I'd need\
    \ something like a 'hand' attribute to fix this."
    ) $ Sig.call0t $ \_ deriver -> Post.map_around realize <$> deriver
    where
    realize _prev event next = Score.remove_attributes damped_tag $
        case Post.filter_next_in_track event next of
            next : _ | Score.has_attribute damped_tag next ->
                Score.set_duration
                    (Score.event_end next - Score.event_start event) event
            _ -> event

-- | Mark events that were damped late, and whose previous event should be
-- extended to be damped together.
damped_tag :: Score.Attributes
damped_tag = Score.attr "damped-tag"
