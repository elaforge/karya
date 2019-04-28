-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Ornaments for gender.  The unique thing about gender technique is the
-- delayed damping, so these calls deal with delayed damping.
module Derive.C.Bali.Gender (
    library, ngoret_variations
    , interval_arg, ngoret, c_realize_ngoret, realize_ngoret
    , weak
) where
import qualified Util.Log as Log
import qualified Util.Seq as Seq
import qualified Derive.Args as Args
import qualified Derive.Call as Call
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Post as Post
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Expr as Expr
import qualified Derive.Flags as Flags
import qualified Derive.Library as Library
import qualified Derive.PSignal as PSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import           Derive.Sig (control, typed_control)
import qualified Derive.Stream as Stream
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Pitch as Pitch

import           Global
import           Types


library :: Library.Library
library = mconcat
    [ Library.generators $
        (("weak", c_weak) : ngoret_variations gender_ngoret)
    , Library.transformers
        [ ("realize-ngoret", c_realize_ngoret)
        , ("infer-damp-simple", c_infer_damp_simple)
        ]
    ]

ngoret_variations :: (Sig.Parser (Maybe Pitch.Transpose) -> call)
    -> [(Expr.Symbol, call)]
ngoret_variations make =
    [ ("'", make $ pure Nothing)
    , ("'n", make $ Just <$> interval_arg)
    , ("'^", make $ pure $ Just $ Pitch.Diatonic (-1))
    , ("'-", make $ pure $ Just $ Pitch.Diatonic 0)
    , ("'_", make $ pure $ Just $ Pitch.Diatonic 1)
    ]

module_ :: Module.Module
module_ = "bali" <> "gender"

-- * ngoret

gender_ngoret :: Sig.Parser (Maybe Pitch.Transpose)
    -> Derive.Generator Derive.Note
gender_ngoret = ngoret module_ True damp_arg
    where
    damp_arg =
        Sig.defaulted "damp" (typed_control "ngoret-damp" 0.5 ScoreT.Real)
        "Time that the grace note overlaps with this one. So the total\
        \ duration is time+damp, though it will be clipped to the\
        \ end of the current note."

interval_arg :: Sig.Parser Pitch.Transpose
interval_arg = Typecheck.default_diatonic <$> Sig.required "interval"
    "The grace note is this interval from the destination pitch."

-- | Other instruments also have ngoret, but without gender's special damping
-- behaviour.
ngoret :: Module.Module -> Bool
    -- ^ Extend the previous note's duration to the end of the grace note.
    -> Sig.Parser DeriveT.ControlRef
    -- ^ Time grace note overlaps with this one.
    -> Sig.Parser (Maybe Pitch.Transpose)
    -> Derive.Generator Derive.Note
ngoret module_ late_damping damp_arg interval_arg =
    Derive.generator module_ "ngoret"
    (Tags.inst <> Tags.ornament <> Tags.requires_postproc)
    ("Insert an intermediate grace note in the \"ngoret\" style.\
    \ The grace note moves up for `'^`, down for `'_`, or is based\
    \ on the previous note's pitch for `'`.\
    \\nThis requires the `realize-ngoret` postproc."
    ) $ Sig.call ((,,,,)
    <$> interval_arg
    <*> Sig.defaulted "time" (typed_control "ngoret-time" 0.1 ScoreT.Real)
        "Time between the grace note start and the main note. If there isn't\
        \ enough room after the previous note, it will be halfway between\
        \ the previous note and this one."
    <*> damp_arg
    <*> Sig.defaulted "dyn" (control "ngoret-dyn" 0.75)
        "The grace note's dyn will be this multiplier of the current dyn."
    <*> Sig.environ "damp-threshold" Sig.Prefixed  0.15
        "A grace note with this much time will cause the previous note to be\
        \ shortened to not overlap. Under the threshold, and the damping of\
        \ the previous note will be delayed until the end of the grace note."
    ) $ \(maybe_interval, time, damp, dyn_scale, damp_threshold) args ->
    Sub.inverting_args args $ \args -> do
        start <- Args.real_start args
        time <- Derive.real =<< Call.time_control_at Typecheck.Real time start
        damp <- Derive.real =<< Call.time_control_at Typecheck.Real damp start
        maybe_pitch <- case maybe_interval of
            Nothing -> return Nothing
            Just transpose ->
                Just . Pitches.transpose transpose <$> Call.get_pitch start
        dyn_scale <- Call.control_at dyn_scale start
        dyn <- (*dyn_scale) <$> Call.dynamic start

        grace_start <- Derive.score (start - time)
        -- If there isn't room for the grace note, use the midpoint between the
        -- prev note and this one.
        grace_start <- return $ case Args.prev_start args of
            Nothing -> grace_start
            Just prev -> max grace_start $ (prev + Args.start args) / 2
        real_grace_start <- Derive.real grace_start
        let with_flags
                | late_damping && prev_touches = Call.add_flags $
                    if start - real_grace_start < damp_threshold
                        then extend_previous else shorten_previous
                | otherwise = id
            prev_touches = maybe False (>= Args.start args) (Args.prev_end args)
        overlap <- Call.score_duration (Args.start args) damp
        let grace_end = Args.start args + overlap
            grace_note = case maybe_pitch of
                Nothing -> Call.add_flags infer_pitch_flag Call.note
                Just pitch -> Call.pitched_note pitch
        Derive.place grace_start (grace_end - grace_start)
                (with_flags $ Call.with_dynamic dyn grace_note)
            <> Derive.place (Args.start args) (Args.duration args) Call.note

-- ** realize

c_realize_ngoret :: Derive.Transformer Derive.Note
c_realize_ngoret = Derive.transformer module_ "realize-ngoret"
    (Tags.inst <> Tags.postproc)
    ("Realize pitches and positions emited by the `ngoret` call.\
    \ This is necessary because it needs to know the positions and pitches\
    \ of the previous and next notes, and those aren't necessarily available\
    \ when evaluating the track. This call needs a "
    <> ShowVal.doc EnvKey.hand <> " envron to figure out which which note\
    \ follows which."
    ) $ Sig.call0t $ \_ deriver -> realize_ngoret =<< deriver

realize_ngoret :: Stream.Stream Score.Event -> Derive.NoteDeriver
realize_ngoret =
    Post.apply_m $ fmap merge . mapM realize . Seq.group_sort Post.hand_key
    where
    -- TODO do I want to ignore streams with irrelevant instruments?
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

realize_infer_pitch :: Maybe Score.Event -> Score.Event
    -> Maybe Score.Event -> Either Text Score.Event
realize_infer_pitch maybe_prev event maybe_next
    | Score.has_flags infer_pitch_flag event = do
        prev <- require "no previous event" maybe_prev
        next <- require "no next event" maybe_next
        pitch <- first ("can't infer pitch: "<>) $ infer_pitch prev next
        -- Also make sure the grace note doesn't go past the end of the next
        -- note.
        let dur = min (Score.event_duration event)
                (Score.event_end next - Score.event_start event)
        return $ Score.remove_flags infer_pitch_flag $ Score.set_duration dur $
            Score.set_pitch (PSignal.constant pitch) event
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

infer_pitch :: Score.Event -> Score.Event -> Either Text PSignal.Pitch
infer_pitch prev next = do
    prev_nn <- tryJust ("no prev nn: " <> Score.short_event prev) $
        Score.initial_nn prev
    next_nn <- tryJust ("no next nn: " <> Score.short_event next) $
        Score.initial_nn next
    let steps
            | prev_nn == next_nn = 0
            | prev_nn < next_nn = -1
            | otherwise = 1
    Pitches.transpose_d steps <$>
        tryJust ("no pitch at " <> pretty (Score.event_start next))
            (Score.pitch_at (Score.event_start next) next)

-- | Mark events whose should have their pitch inferred from the previous and
-- next events.
infer_pitch_flag :: Flags.Flags
infer_pitch_flag = Flags.flag "infer-pitch"

-- | Mark grace notes that were damped late, and whose previous event should be
-- extended to be damped together.
extend_previous :: Flags.Flags
extend_previous = Flags.flag "extend-previous-duration"

-- | Mark grace notes that don't cause a late damp.  The previous event's
-- duration should be shortened to end where the grace note begins.
shorten_previous :: Flags.Flags
shorten_previous = Flags.flag "shorten-previous-duration"

-- * weak

c_weak :: Derive.Generator Derive.Note
c_weak = Derive.generator module_ "weak" Tags.inst
    "Weak notes are filler notes."
    $ Sig.call (
    Sig.defaulted "strength" (control "strength" 0.5)
        "From low strength to high, omit the note, then play it muted, and\
        \ then play it open but softly."
    ) $ \strength -> Sub.inverting (weak strength)

weak :: DeriveT.ControlRef -> Derive.PassedArgs a -> Derive.NoteDeriver
weak strength args = do
    strength <- Call.control_at strength =<< Args.real_start args
    -- This biases %mute values to be lower, and 0 before it unmutes.
    let mute = max 0 $ 1 - (strength + (1 - unmute_threshold))
    if strength <= omit_threshold then mempty
        else Call.with_constant Controls.mute mute $ Call.placed_note args
    where
    omit_threshold = 0.25
    unmute_threshold = 0.75

-- * im

-- ** infer damp

{- |
    Simple version:
    - Any note immediately followed by the same pitch gets its duration
    extended to the end of the last note with the same pitch.

    Fancy version:
    - All notes ring until explicitly damped.
    - A gap between notes in the same hand adds a damp.  A pitch followed by a
    different one in the same hand adds a damp to the first.
    - The hand requires time to actually do the damp.  Adjacent pitches by 1 or
    2 can be simultaneous.  Otherwise, you need a certain amount of time when
    that hand is not busy damping.
-}
c_infer_damp_simple :: Derive.Transformer Derive.Note
c_infer_damp_simple =
    Derive.transformer (module_ <> "im") "infer-damp-simple" Tags.postproc
    ("Simple gender damping. Duration is extended if the next note on the same\
    \ hand has the same pitch and the gap is < " <> ShowVal.doc gap <> ".")
    $ Sig.callt (Sig.required "insts" "Apply damping to these instruments.")
    $ \insts _args ->
        fmap $ Post.emap1_ (infer insts) . Post.nexts_by Post.hand_key
    where
    infer insts =
        Post.only fst (Post.has_instrument insts) $ infer_damp_simple gap
    -- Less than this much time before the next note of the same pitch means
    -- extend the duration.
    gap = 0.15

infer_damp_simple :: RealTime -> (Score.Event, [Score.Event]) -> Score.Event
infer_damp_simple gap (event, nexts)
    | Score.event_duration event /= 0 && new_end > Score.event_end event =
        Score.set_duration (new_end - Score.event_start event) event
    | otherwise = event
    where
    new_end = go event nexts
    go prev (next:nexts)
        | Score.event_start next - Score.event_end prev <= gap
                && Score.initial_note prev == Score.initial_note next
            = go next nexts
    go prev _ = Score.event_end prev
