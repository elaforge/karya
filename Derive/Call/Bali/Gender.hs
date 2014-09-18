-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Ornaments for gender.  The unique thing about gender technique is the
-- delayed damping, so these calls deal with delayed damping.
module Derive.Call.Bali.Gender (
    note_calls, interval_arg, ngoret, c_realize_ngoret
) where
import Util.Control
import qualified Util.Log as Log
import qualified Util.Seq as Seq

import qualified Derive.Args as Args
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Post as Post
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.Environ as Environ
import qualified Derive.Flags as Flags
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
    [ ("'", gender_ngoret $ pure Nothing)
    , ("'n", gender_ngoret $ Just <$> interval_arg)
    , ("'^", gender_ngoret $ pure $ Just $ Pitch.Diatonic (-1))
    , ("'_", gender_ngoret $ pure $ Just $ Pitch.Diatonic 1)
    ]
    [ ("realize-ngoret", c_realize_ngoret)
    ]

module_ :: Module.Module
module_ = "bali" <> "gender"

gender_ngoret :: Sig.Parser (Maybe Pitch.Transpose)
    -> Derive.Generator Derive.Note
gender_ngoret = ngoret module_ True damp_arg
    where
    damp_arg = defaulted "damp" (typed_control "ngoret-damp" 0.5 Score.Real)
        "Time that the grace note overlaps with this one. So the total\
        \ duration is time+damp, though it will be clipped to the\
        \ end of the current note."

interval_arg :: Sig.Parser Pitch.Transpose
interval_arg = TrackLang.default_diatonic <$> Sig.required "interval"
    "The grace note is this interval from the destination pitch."

-- | Other instruments also have ngoret, but without gender's special damping
-- behaviour.
ngoret :: Module.Module -> Bool
    -- ^ Extend the previous note's duration to the end of the grace note.
    -> Sig.Parser TrackLang.ValControl
    -> Sig.Parser (Maybe Pitch.Transpose)
    -> Derive.Generator Derive.Note
ngoret module_ late_damping damp_arg interval_arg =
    Derive.make_call module_ "ngoret"
    (Tags.inst <> Tags.ornament <> Tags.requires_postproc)
    ("Insert an intermediate grace note in the \"ngoret\" style.\
    \ The grace note moves up for `'^`, down for `'`, or is based\
    \ on the previous note's pitch for `'`."
    ) $ Sig.call ((,,,,)
    <$> interval_arg
    <*> defaulted "time" (typed_control "ngoret-time" 0.1 Score.Real)
        "Time between the grace note start and the main note. If there isn't\
        \ enough room after the previous note, it will be halfway between\
        \ the previous note and this one."
    <*> damp_arg
    <*> defaulted "dyn" (control "ngoret-dyn" 0.75)
        "The grace note's dyn will be this multiplier of the current dyn."
    <*> Sig.environ "damp-threshold" Sig.Prefixed  0.25
        "A grace note with this much time will cause the previous note to be\
        \ shortened to not overlap. Under the threshold, and the damping of\
        \ the previous note will be delayed until the end of the grace note."
    ) $ \(maybe_interval, time, damp, dyn_scale, damp_threshold) ->
    Sub.inverting $ \args -> do
        start <- Args.real_start args
        time <- Derive.real =<< Util.time_control_at Util.Real time start
        damp <- Derive.real =<< Util.time_control_at Util.Real damp start
        maybe_pitch <- case maybe_interval of
            Nothing -> return Nothing
            Just transpose ->
                Just . Pitches.transpose transpose <$> Util.get_pitch start
        dyn_scale <- Util.control_at dyn_scale start
        dyn <- (*dyn_scale) <$> Util.dynamic start

        grace_start <- Derive.score (start - time)
        -- If there isn't room for the grace note, use the midpoint between the
        -- prev note and this one.
        grace_start <- return $ case Args.prev_start args of
            Nothing -> grace_start
            Just prev -> max grace_start $ (prev + Args.start args) / 2
        real_grace_start <- Derive.real grace_start
        let with_flags
                | late_damping && prev_touches = Util.add_flags $
                    if start - real_grace_start < damp_threshold
                        then extend_previous else shorten_previous
                | otherwise = id
            prev_touches = maybe False (>= Args.start args) (Args.prev_end args)
        overlap <- Util.score_duration (Args.start args) damp
        let grace_end = Args.start args + overlap
            grace_note = case maybe_pitch of
                Nothing -> Util.add_flags infer_pitch_flag Util.note
                Just pitch -> Util.pitched_note pitch
        Derive.place grace_start (grace_end - grace_start)
                (with_flags $ Util.with_dynamic dyn grace_note)
            <> Derive.place (Args.start args) (Args.duration args) Util.note

-- * realize

c_realize_ngoret :: Derive.Transformer Derive.Note
c_realize_ngoret = Derive.transformer module_ "realize-ngoret"
    (Tags.inst <> Tags.postproc)
    ("Realize pitches and positions emited by the `ngoret` call.\
    \ This is necessary because it needs to know the positions and pitches\
    \ of the previous and next notes, and those aren't necessarily available\
    \ when evaluating the track. This call needs a "
    <> ShowVal.doc_val Environ.hand <> " envron to figure out which which note\
    \ follows which."
    ) $ Sig.call0t $ \_ deriver -> realize_ngoret =<< deriver

realize_ngoret :: Derive.Events -> Derive.Deriver Derive.Events
realize_ngoret = Post.apply $ fmap merge . mapM realize . Seq.group_on key
    where
    -- TODO do I want to ignore streams with irrelevant instruments?
    key e = (Score.event_instrument e, event_hand e)
    realize = fmap (map (uncurry realize_damped) . Seq.zip_next)
        . apply realize_infer_pitch
    apply f = mapMaybeM (apply1 f) . Seq.zip_neighbors
        where
        apply1 f (prev, event, next) = case f prev event next of
            Right event -> return $ Just event
            Left err -> do
                Derive.with_event_stack event $ Log.warn err
                return Nothing
    merge = Seq.merge_lists Score.event_start

event_hand :: Score.Event -> Maybe Text
event_hand = TrackLang.maybe_val Environ.hand . Score.event_environ

realize_infer_pitch :: Maybe Score.Event -> Score.Event
    -> Maybe Score.Event -> Either Text Score.Event
realize_infer_pitch maybe_prev event maybe_next
    | Score.has_flags infer_pitch_flag event = do
        prev <- require "no previous event" maybe_prev
        next <- require "no next event" maybe_next
        pitch <- require "can't infer pitch" $ infer_pitch prev next
        return $ Score.remove_flags infer_pitch_flag $ event
            { Score.event_pitch = PitchSignal.constant pitch
            -- Also make sure the grace note doesn't go past the end of the
            -- next note.
            , Score.event_duration = min (Score.event_duration event)
                (Score.event_end next - Score.event_start event)
            }
    | otherwise = return event
    where require err = maybe (Left err) return

realize_damped :: Score.Event -> Maybe Score.Event -> Score.Event
realize_damped event maybe_next =
    Score.remove_flags (extend_previous <> shorten_previous) $
        maybe id set_dur maybe_next event
    where
    set_dur next
        | Score.has_flags extend_previous next =
            Score.set_duration (Score.event_end next - start)
        | Score.has_flags shorten_previous next =
            Score.set_duration (Score.event_start next - start)
        | otherwise = id
        where start = Score.event_start event

infer_pitch :: Score.Event -> Score.Event -> Maybe PitchSignal.Pitch
infer_pitch prev next = do
    prev_nn <- Score.initial_nn prev
    next_nn <- Score.initial_nn next
    let steps
            | prev_nn == next_nn = 0
            | prev_nn < next_nn = -1
            | otherwise = 1
    Pitches.transpose_d steps <$> Score.pitch_at (Score.event_start next) next

-- | Mark events whose should have their pitch inferred from the previous and
-- next events.
infer_pitch_flag :: Flags.Flags
infer_pitch_flag = Flags.flag "infer-pitch"

-- | Mark events that were damped late, and whose previous event should be
-- extended to be damped together.
extend_previous, shorten_previous :: Flags.Flags
extend_previous = Flags.flag "extend-previous-duration"
shorten_previous = Flags.flag "shorten-previous-duration"
