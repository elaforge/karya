-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Ornaments for gender.  The unique thing about gender technique is the
-- delayed damping, so these calls deal with delayed damping.
module Derive.Call.Bali.Gender (note_calls, ngoret) where
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
    [ ("'", gender_ngoret False Nothing)
    , ("'^", gender_ngoret False (Just (Pitch.Diatonic (-1))))
    , ("'_", gender_ngoret False (Just (Pitch.Diatonic 1)))

    , ("'-", gender_ngoret True Nothing)
    , ("'^-", gender_ngoret True (Just (Pitch.Diatonic (-1))))
    , ("'_-", gender_ngoret True (Just (Pitch.Diatonic 1)))

    ]
    [ ("realize-ngoret", c_realize_ngoret)
    ]

module_ :: Module.Module
module_ = "bali" <> "gender"

gender_ngoret :: Bool -> Maybe Pitch.Transpose -> Derive.Generator Derive.Note
gender_ngoret is_standalone = ngoret is_standalone module_ True damp_arg
    where
    damp_arg = defaulted "damp" (typed_control "ngoret-damp" 0.5 Score.Real)
        "Time that the grace note overlaps with this one. So the total\
        \ duration is time+damp, though it will be clipped to the\
        \ end of the current note."

-- | Other instruments also have ngoret, but without gender's special damping
-- behaviour.
ngoret :: Bool -> Module.Module -> Bool -> Sig.Parser TrackLang.ValControl
    -> Maybe Pitch.Transpose -> Derive.Generator Derive.Note
ngoret is_standalone module_ late_damping damp_arg maybe_transpose =
    Derive.make_call module_ "ngoret"
    (Tags.inst <> Tags.ornament <> Tags.requires_postproc)
    ("Insert an intermediate grace note in the \"ngoret\" style.\
    \ The grace note moves up for `'^`, down for `'`, or is based\
    \ on the previous note's pitch for `'`. These versions are attached to\
    \ a note, but there are standalone versions suffixed with `-` that are\
    \ replaced by the appropriate grace note."
    -- TODO or maybe I could make a zero-duration ngoret standalone
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
        time <- Derive.real =<< Util.time_control_at Util.Real time start
        damp <- Derive.real =<< Util.time_control_at Util.Real damp start
        maybe_pitch <- case maybe_transpose of
            Nothing -> return Nothing
            Just transpose ->
                Just . Pitches.transpose transpose <$> Util.get_pitch start
        dyn_scale <- Util.control_at dyn_scale start
        dyn <- (*dyn_scale) <$> Util.dynamic start
        let with_damped
                | late_damping && prev_touches = Util.add_attrs damped_tag
                | otherwise = id
            prev_touches = maybe False (>= Args.start args) (Args.prev_end args)
        (if is_standalone then emit_standalone else emit_attached)
            args start time damp dyn maybe_pitch with_damped
    where
    emit_standalone args start time damp dyn maybe_pitch with_damped = do
        let event_args =
                [ TrackLang.to_val maybe_pitch, TrackLang.to_val time
                , TrackLang.to_val damp
                ]
        next <- Derive.real (Args.next args)
        Util.with_dynamic dyn $ with_damped $
            Post.make_delayed args start next event_args

    emit_attached args start time damp dyn maybe_pitch with_damped = do
        grace_start <- Derive.score (start - time)
        -- If there isn't room for the grace note, use the midpoint between the
        -- prev note and this one.
        grace_start <- return $ case Args.prev_start args of
            Nothing -> grace_start
            Just prev -> max grace_start $ (prev + Args.start args) / 2
        overlap <- Util.score_duration (Args.start args) damp
        let grace_end = min (Args.end args) (Args.start args + overlap)
        Derive.place grace_start (grace_end - grace_start)
                (with_damped $ Util.with_dynamic dyn grace_note)
            <> Derive.place (Args.start args) (Args.duration args) Util.note
        where
        grace_note = case maybe_pitch of
            Nothing -> Util.add_attrs infer_pitch_tag Util.note
            Just pitch -> Util.pitched_note pitch

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
    realize = apply realize_damped <=< apply realize_infer_pitch
        <=< apply realize_standalone_ngoret
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

realize_standalone_ngoret :: Maybe Score.Event -> Score.Event
    -> Maybe Score.Event -> Either Text Score.Event
realize_standalone_ngoret maybe_prev event maybe_next
    | Just args <- Post.delayed_args "ngoret" event = do
        next <- require "no next event" maybe_next
        (maybe_pitch, time, damp) <- require "wrong number of args" $ do
            [a, b, c] <- return args
            return (a, b, c)
        pitch <- case TrackLang.from_val maybe_pitch of
            Just pitch -> return pitch
            Nothing -> require "can't infer pitch from previous note" $
                flip infer_pitch next =<< maybe_prev
        time <- Post.typecheck time
        damp <- Post.typecheck damp
        let start1 = Score.event_start next - time
            -- As with attached ngoret, if there isn't room for the grace note,
            -- use the midpoint between the prev note and the next one.
            start2 = maybe start1
                (max start1 . (/2) . (+ Score.event_start next)
                    . Score.event_start)
                maybe_prev
            end = Score.event_start next + damp
        return $ event
            { Score.event_start = start2
            , Score.event_duration = end - start2
            , Score.event_pitch = PitchSignal.constant pitch
            , Score.event_environ = TrackLang.delete_val Environ.args $
                Score.event_environ event
            }
    | otherwise = Right event

realize_infer_pitch :: Maybe Score.Event -> Score.Event
    -> Maybe Score.Event -> Either Text Score.Event
realize_infer_pitch maybe_prev event maybe_next
    | Score.has_attribute infer_pitch_tag event = do
        prev <- require "no previous event" maybe_prev
        next <- require "no next event" maybe_next
        pitch <- require "can't infer pitch" $ infer_pitch prev next
        return $ Score.remove_attributes infer_pitch_tag $
            event { Score.event_pitch = PitchSignal.constant pitch }
    | otherwise = return event

require :: Text -> Maybe a -> Either Text a
require err = maybe (Left err) return

realize_damped :: Maybe Score.Event -> Score.Event
    -> Maybe Score.Event -> Either Text Score.Event
realize_damped _ event maybe_next
    | Just next <- maybe_next, Score.has_attribute damped_tag next = Right $
        Score.set_duration (Score.event_end next - Score.event_start event)
            event
    | otherwise = Right $ Score.remove_attributes damped_tag event

infer_pitch :: Score.Event -> Score.Event -> Maybe PitchSignal.Pitch
infer_pitch prev next = do
    steps <- ifM ((<=) <$> Score.initial_nn prev <*> Score.initial_nn next)
        (return (-1)) (return 1)
    Pitches.transpose_d steps <$> Score.pitch_at (Score.event_start next) next

-- | Mark events whose should have their pitch inferred from the previous and
-- next events.
infer_pitch_tag :: Score.Attributes
infer_pitch_tag = Score.attr "infer-pitch-tag"

-- | Mark events that were damped late, and whose previous event should be
-- extended to be damped together.
damped_tag :: Score.Attributes
damped_tag = Score.attr "damped-tag"
