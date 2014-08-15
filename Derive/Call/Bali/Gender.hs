-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Ornaments for gender.  The unique thing about gender technique is the
-- delayed damping, so these calls deal with delayed damping.
module Derive.Call.Bali.Gender (note_calls, ngoret) where
import Util.Control
import qualified Derive.Args as Args
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Post as Post
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import Derive.Sig (control, defaulted, typed_control)
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch


note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.call_maps
    [ ("'", gender_ngoret Nothing)
    , ("'^", gender_ngoret (Just (Pitch.Diatonic (-1))))
    , ("'_", gender_ngoret (Just (Pitch.Diatonic 1)))
    ]
    [ ("realize-damp", c_realize_damp) ]

module_ :: Module.Module
module_ = "bali" <> "gender"

gender_ngoret :: Maybe Pitch.Transpose -> Derive.Generator Derive.Note
gender_ngoret = ngoret module_ True damp_arg
    where
    damp_arg = defaulted "damp" (typed_control "ngoret-damp" 0.5 Score.Real)
        "Time that the grace note overlaps with this one. So the total\
        \ duration is time+damp, though it will be clipped to the\
        \ end of the current note."

ngoret :: Module.Module -> Bool -> Sig.Parser TrackLang.ValControl
    -> Maybe Pitch.Transpose -> Derive.Generator Derive.Note
ngoret module_ add_damped_tag damp_arg transpose =
    Derive.make_call module_ "ngoret"
    (Tags.inst <> Tags.ornament <> Tags.prev)
    ("Insert an intermediate grace note in the \"ngoret\" style.\
    \ The grace note moves up for `'^`, down for `'_`, or is based\
    \ on the previous note's pitch for `'`."
    ) $ Sig.call ((,,)
    <$> defaulted "time" (typed_control "ngoret-time" 0.1 Score.Real)
        "Time between the grace note start and the main note. If there isn't\
        \ enough room after the previous note, it will be halfway between\
        \ the previous note and this one."
    <*> damp_arg
    <*> defaulted "dyn" (control "ngoret-dyn" 0.75)
        "The grace note's dyn will be this multiplier of the current dyn."
    ) $ \(time, damp, dyn_scale) -> Sub.inverting $ \args -> do
        start <- Args.real_start args
        transpose <- maybe (infer_transpose args =<< Util.get_pitch start)
            return transpose
        time <- Derive.real =<< Util.time_control_at Util.Real time start
        damp <- Util.time_control_at Util.Real damp start
        dyn_scale <- Util.control_at dyn_scale start
        dyn <- Util.dynamic start

        grace_start <- Derive.score (start - time)
        -- If there isn't room for the grace note, use the midpoint between the
        -- prev note and this one.
        grace_start <- return $ case Args.prev_start args of
            Nothing -> grace_start
            Just prev -> max grace_start $ (prev + Args.start args) / 2
        overlap <- Util.score_duration (Args.start args) damp
        let grace_end = min (Args.end args) (Args.start args + overlap)

        let prev_touches = maybe False (>= Args.start args) (Args.prev_end args)
            with_tag
                | add_damped_tag && prev_touches = Util.add_attrs damped_tag
                | otherwise = id
        pitch <- Util.get_raw_pitch start
        Derive.place grace_start (grace_end - grace_start)
                (with_tag $ Util.with_dynamic (dyn * dyn_scale) $
                    Util.pitched_note (Pitches.transpose transpose pitch))
            <> Derive.place (Args.start args) (Args.duration args) Util.note

infer_transpose :: Derive.NoteArgs -> PitchSignal.Transposed
    -> Derive.Deriver Pitch.Transpose
infer_transpose args pitch = do
    prev <- Derive.require "no previous event" $ Args.prev_val args
    prev_pitch <- Derive.require "previous event lacks pitch" $
        Score.initial_pitch prev
    ifM ((<=) <$> Pitches.pitch_nn prev_pitch <*> Pitches.pitch_nn pitch)
        (return (Pitch.Diatonic (-1))) (return (Pitch.Diatonic 1))

c_realize_damp :: Derive.Transformer Derive.Note
c_realize_damp = Derive.transformer module_ "realize-damp"
    (Tags.inst <> Tags.postproc)
    ("Extend the duration of events preceding one with a "
    <> ShowVal.doc_val damped_tag <> " to the end of the event with the attr.\
    \ This is because the `ngoret` call can't modify its previous note.\
    \ TODO: Since there's no correspondence between tracks in different\
    \ blocks, the damping can't extend across block boundaries. I'd need\
    \ something like a 'hand' attribute to fix this."
    ) $ Sig.call0t $ \_ deriver -> do
        events <- deriver
        return $ Post.map_events_asc_ realize_damp $
            LEvent.zip (Post.nexts events) events

realize_damp :: ([Score.Event], Score.Event) -> [Score.Event]
realize_damp (nexts, event) = (:[]) $ Score.remove_attributes damped_tag $
    case Post.filter_next_in_track event nexts of
        next : _ | Score.event_end event >= Score.event_start next
                && Score.has_attribute damped_tag next ->
            Score.set_duration
                (Score.event_end next - Score.event_start event) event
        _ -> event

-- | Mark events that were damped late, and whose previous event should be
-- extended to be damped together.
damped_tag :: Score.Attributes
damped_tag = Score.attr "damped-tag"
